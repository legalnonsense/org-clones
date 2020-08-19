;;; org-clones.el --- Clone and sync Orgmode headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; URL: http://example.com/package-name.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: org transclusion clones outline

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package creates clones org headings. It uses the following
;; terminology:

;; "Node" means an entry in an org outline
;;
;; "Headline" means the headline text of the entry, but does
;;            not include the headline's todo state or tags
;;
;; "Body" means everything after the headline, planning line,
;;        and property drawers until the next node.

;; This package allows the user to clone a node, which duplicates
;; the node's headline and body. An overlay is then placed over each clone.
;; If the user attempts to edit the clone, they are prompted to either enter
;; an edit-mode, which will sync all changes to other clones upon completion,
;; or to unlink the clone. 

;;;; Installation

;;;;; Manual

;; Install these required packages:

;; + org-id
;; + org-ml
;; + ov

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'org-clones)

;;;; Usage

;; Run one of these commands:

;; `org-clones-create-clone' when the point is on a node you wish to clone

;;;; Tips

;; + You can customize settings in the `org-clones' group.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-ql)
(require 'ov)
(require 'org-ml)

;;;; Faces

(defface org-clones-clone-highlight
  '((t (:background "orchid" :box t)))
  "Body of cloned nodes."
  :group 'org-clones)

;;;; Customization

(defcustom org-clones-clone-prefix-string "◈ "
  "String prepended to the headline of a cloned node."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-empty-body-string "[empty clone body]\n"
  "Place holder inserted into clones with empty bodies.
Can be any string other than whitespace."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-empty-headling-string "[empty clone headline]"
  "Place holder inserted into clones with empty headlines.
Can be any string other than whitespace."
  :group 'org-clones
  :type 'string)

;;;; Variables

(defvar org-clones-clone-headline-text-props
  '((cursor-sensor-functions . (org-clones--text-watcher-func-headline-watcher)))
  "List of text properties applied to the headline of cloned nodes.")

(defvar org-clones-clone-body-text-props
  '((cursor-sensor-functions . (org-clones--text-watcher-func-body-watcher)))
  "List of text properties applied to the body of cloned nodes.")

(defvar org-clones--restore-state nil
  "When editing a clone, save the current headline and body
to restore if the edit is abandoned.")

(defvar org-clones--temp-marker nil
  "Temporary storage for a marker for clone creation in 
separate file.")

(defvar org-clones--headline-re "^*+ " ; outline-regexp
  "Org headline regexp.")

(defvar org-clones--not-whitespace-re "[^[:space:]]"
  "Regexp to match any non-whitespace charcter.")

;;;; Macros

(defmacro org-clones--inhibit-read-only (&rest body)
  "Substitute for (let ((inhibit-read-only t)) ...)."
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro org-clones--iterate-over-clones (&rest body)
  "Execute BODY at each clone of node at point."
  `(save-excursion
     (when-let ((clone-ids (org-clones--get-clone-ids)))
       (cl-loop for clone-id in clone-ids
		do (org-clones--with-point-at-id clone-id
		     ,@body)))))

(defmacro org-clones--with-point-at-id (id &rest body)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer, execute BODY,
move back."
  (declare (indent defun))
  `(--when-let (org-id-find ,id 'marker)
     (save-excursion 
       (with-current-buffer (marker-buffer it)
	 (goto-char it)
	 ,@body))))

;;;; Headline functions

(defun org-clones--goto-headline-start ()
  "Goto the first point of the headline, after the
leading stars."
  (org-back-to-heading t)
  (re-search-forward org-clones--headline-re (point-at-eol))
  (when-let ((todo (org-get-todo-state)))
    (re-search-forward todo (point-at-eol) t))
  (if (re-search-forward org-clones--not-whitespace-re
			 (point-at-eol)
			 t)
      (forward-char -1))
  (point))

(defun org-clones--get-headline-start ()
  "Get the point at the start of the headling, after
the leading stars."
  (save-excursion
    (org-clones--goto-headline-start)))

(defun org-clones--goto-headline-end ()
  "Goto the last point of the headline (i.e., before the
tag line."
  (org-back-to-heading t)
  (if (re-search-forward
       (concat ":" org-tag-re ":") (point-at-eol) t)
      (goto-char (1- (match-beginning 0)))
    (end-of-line))
  (when (re-search-backward org-clones--not-whitespace-re)
    (goto-char (match-end 0)))
  (point))

(defun org-clones--get-headline-end ()
  "Get the point at the end of the headline, but
before the ellipsis."
  (save-excursion (org-clones--goto-headline-end)))

(defun org-clones--delete-headline ()
  "Delete the headline of the heading at point."
  (org-clones--inhibit-read-only
   (unless (string=
	    (plist-get (cadr (org-element-at-point)) :raw-value)
	    "")
     (delete-region (org-clones--get-headline-start)
		    (org-clones--get-headline-end)))))

(defun org-clones--get-headline-string ()
  "Get the full text of a headline at point, including
TODO state, headline text, and tags." 
  (buffer-substring (org-clones--get-headline-start)
		    (org-clones--get-headline-end)))

;; (defun org-clones--replace-headline (headline)
;;   "Replace the headline text at point with HEADLINE."
;;   (save-excursion 
;;     (org-clones--delete-headline)
;;     (org-clones--goto-headline-start)
;;     (insert headline)))

(defun org-clones--replace-headline (headline)
  "Replace the headline text at point with HEADLINE"
  ;; I don't like using `org-ml' just for this one function
  ;; but it works so well!
  (org-ml-update-this-headline*
    (org-ml-set-property :title `(,(concat headline " ")) it)))

(defun org-clones--get-body-end ()
  "Get the end point of the body of the current node."
  (save-excursion (org-clones--goto-body-end)))

(defun org-clones--node-body-p ()
  "Does this node have a body (i.e., a section in org-element
parlance?"
  (org-clones--parse-body))

;;;; Body functions 

(defun org-clones--insert-blank-body ()
  "Insert `org-clones-empty-body-string' into the body 
of the current node."
  (org-clones--replace-body org-clones-empty-body-string))

(defun org-clones--goto-body-start ()
  "Go to the start of the body of the current node,
and return the point."
  (org-end-of-meta-data t)
  (when (re-search-backward org-clones--not-whitespace-re nil t)
    (forward-char 2))
  (point))

(defun org-clones--get-body-start ()
  "Get the start point of the body of the current node."
  (save-excursion (org-clones--goto-body-start)))

(defun org-clones--goto-body-end ()
  "Goto the end of the body of the current node, 
and return the point."
  (unless (outline-next-heading)
    (goto-char (point-max)))
  (re-search-backward org-clones--not-whitespace-re
		      nil t)
  (goto-char (match-end 0)))

(defun org-clones--replace-body (body)
  "Replace the body of the current node with
BODY."
  (org-back-to-heading)
  (save-excursion 
    (org-clones--delete-body))
  (org-clones--goto-body-start)
  (save-excursion 
    (insert (or body
		org-clones-empty-body-string)
	    "\n")))

(defun org-clones--parse-body ()
  "Parse all elements from the start of the body to the next node.
and return the tree beginning with the section element."
  (org-element--parse-elements (save-excursion (org-back-to-heading)
					       (org-end-of-meta-data t)
					       (point))
			       (or (save-excursion (outline-next-heading))
				   (point-max))
			       'first-section nil nil nil nil))

(defun org-clones--get-body-string ()
  "Get the body of the current node as a string." 
  (org-element-interpret-data 
   (org-clones--get-section-elements)))

(defun org-clones--get-section-elements ()
  "Reduce the section data to the component elements,
e.g., '((paragraph (...))
        (src-block (...)) ...)."
  (cddar (org-clones--parse-body)))

(defun org-clones--get-body-section-plist ()
  "Get the plist associated with the section element, 
e.g. (:begin 1 :end 10 :contents-begin ...)."
  (cadar (org-clones--parse-body)))

(defun org-clones--delete-body ()
  (org-clones--inhibit-read-only
   (when-let* ((prop-list (org-clones--get-body-section-plist))
	       (beg (plist-get prop-list :begin))
	       (end (plist-get prop-list :end)))
     (delete-region beg end))))

;;;; Navigation functions 

(defun org-clones--get-clone-ids ()
  "Get the org-ids of this node's clones. Return
nil if there are none."
  (org-entry-get-multivalued-property
   (point)
   "ORG-CLONES"))

(defun org-clones--last-node-p ()
  "Is this the last node in the document?"
  (not (or (save-excursion (org-get-next-sibling))
	   (save-excursion (org-goto-first-child)))))

(defun org-clones--prompt-for-source-and-move ()
  "Prompt user for a node and move to it."
  (org-goto))

(defun org-clones--update-clones ()
  "Update all clones of the current node to match
the headline and body of the current node and
place text properties and overlays in the cloned nodes."
  (interactive)
  (org-clones--remove-clone-effects)
  (let ((headline (org-clones--get-headline-string))
	(body (if (string= "" (org-clones--get-body-string))
		  org-clones-empty-body-string
		(org-clones--get-body-string))))

    ;; Replace the headline in the current node to ensure
    ;; there aren't differences in white space between the nodes
    (org-clones--replace-body body)
    (org-clones--put-clone-effects)
    (org-clones--iterate-over-clones
     (org-clones--remove-clone-effects)
     (org-clones--replace-headline headline)
     (org-clones--replace-body body)
     (org-clones--put-clone-effects))))

;; FIX ME
(defun org-clones--unsync-this-clone ()
  (let ((this-id (org-id-get))
	(clone-ids (org-clones--get-clone-ids)))
    (cl-loop for clone-id in clone-ids
	     do (org-clones--with-point-at-id clone-id
		  (org-entry-remove-from-multivalued-property
		   (point) "ORG-CLONES" this-id)
		  (unless (org-clones--get-clone-ids)
		    (org-clones--remove-clone-effects))))
    (message "This node is no longer synced with other clones.")))

;;; Text properties and overlays 
(defun org-clones--put-text-properties ()
  "Make the node at point read-only, for the purposes
of locking edits of the headline and body."
  (cl-loop
   with beg = (org-clones--get-headline-start)
   with end = (org-clones--get-headline-end)
   for (prop . val) in org-clones-clone-headline-text-props
   do (put-text-property beg end prop val))

  (cl-loop
   with beg = (org-clones--get-body-start)
   with end = (org-clones--get-body-end)
   for (prop . val) in org-clones-clone-body-text-props
   do (put-text-property beg end prop val)))

(defun org-clones--remove-text-properties ()
  "Remove text properties for the current node."
  ;; It's possible there is a read-only property here...
  (let ((inhibit-read-only t))
    (set-text-properties (org-clones--get-headline-start)
			 (org-clones--get-headline-end)
			 nil)
    (set-text-properties (org-clones--get-body-start)
			 (org-clones--get-body-end)
			 nil)))


;;;; Cursor-sensor-functions

(cl-defmacro org-clones--create-text-watcher (name &key
						   enter exit storage-form
						   change no-change)

  "Define a function for use with `cursor-sensor-mode' and the 
associated text property, cursor-sensor-functions. 

NAME is the name used to create the underlying function and 
storage variable.

ENTER is a form executed when the cursor enters the text field.

When the cursor enters the text field, STORAGE-FORM will run 
and store the return value in a variable named 
 `org-clones--text-watcher-storage-NAME'.
(This variable is available to retrieve the initial value of the
text field after the cursor enters the field, if needed.)

STORAGE-FORM is a form which, when evaluated, returns a string. 
The string is stored when the cursor enters the field, and when
the cursor exits the field. If these strings are not equal, then
evaluate CHANGE. Otherwise, evaluate NO-CHANGE. The comparison
between of the values of STORAGE-FORM are done with `string='.

EXIT is a form executed when cursor exits the text field, regardless
of whether there was a change or not. EXIT will be evaluated before 
CHANGE or NO CHANGE.

See `cursor-sensor-mode' for more details."
  (declare (indent defun))
  (let ((var-name
	 (intern (concat "org-clones--text-watcher-storage-"
			 (symbol-name name))))
	(function-name
	 (intern (concat "org-clones--text-watcher-func-"
			 (symbol-name name)))))
    `(progn
       (if (boundp ',var-name)
	   (setq ,var-name nil)
	 (defvar ,var-name nil))
       (defun ,function-name (window last-pos entered-or-left)
	 ;; Banged my head against the keyboard many
	 ;; times before realizing this let has to be here.
	 (let ((cursor-sensor-inhibit t))
	   (pcase entered-or-left
	     (`entered
	      (setq ,var-name ,@storage-form)
	      ,@enter)
	     (`left
	      ;; Probably a better way to handle these
	      ;; save-excursions.
	      (save-excursion
		(goto-char last-pos)
		,@exit)
	      (if (string=
		   (save-excursion
		     (goto-char last-pos)
		     ,@storage-form)
		   ,var-name)
		  (save-excursion
		    ,@no-change
		    (setq ,var-name nil))
		(setq ,var-name
		      (save-excursion
			(goto-char last-pos)
			,@storage-form))
		(save-excursion
		  (goto-char last-pos)
		  ,@change)
		(setq ,var-name nil)))))))))

(org-clones--create-text-watcher headline-watcher
  :enter ((message "Entered cloned headline!")
	  ;; There are better ways to handle this overlay
	  ;; but this is just a demonstration...
	  (org-clones--begin-edit)
	  (ov (org-clones--get-headline-start)
	      (org-clones--get-headline-end)
	      'face '(:background "pink"
				  :foreground "black"
				  :box t)
	      'priority 10))
  :exit ((ov-clear 'face
		   '(:background "pink"
				 :foreground "black"
				 :box t)
		   (org-clones--get-headline-start)
		   (org-clones--get-headline-end)))
  :change ((org-clones--prompt-before-commit))
  :storage-form ((org-clones--get-headline-string)))

(org-clones--create-text-watcher body-watcher
  :enter ((message "Entered cloned body!")
	  (ov (org-clones--get-body-start)
	      (org-clones--get-body-end)
	      'face '(:background "pink"
				  :foreground "black"
				  :box t)
	      'priority 10))
  :exit ((ov-clear 'face
		   '(:background "pink"
				 :foreground "black"
				 :box t)
		   (org-clones--get-body-start)
		   (org-clones--get-body-end)))
  :change ((org-clones--prompt-before-commit))
  :storage-form ((org-clones--get-body-string)))

(defun org-clones--cursor-sensor-mode-check ()
  "Turn `cursor-sensor-mode' on or off depending on
whether any buffer text has the cursor-sensor-functions
text property."
  (if (save-excursion
	(save-restriction
	  (widen)		
	  (next-single-property-change
	   (point-min)
	   'cursor-sensor-functions)))
      ;; If so, enable cursor-sensor-mode...
      (cursor-sensor-mode 1)
    ;; ...otherwise, disable it. 
    (cursor-sensor-mode -1)))

;;;; Editing clones

(defun org-clones--begin-edit ()
  "Save the state of a clone before an edit begins."
  (setq org-clones--restore-state
	(cons (org-clones--get-headline-string)
	      (org-clones--get-body-string))))

(defun org-clones--prompt-before-commit ()
  "Ask the user if they want to edit the node
without syncing the clones. If so, unlink the current 
clone."
  (interactive)
  (if (y-or-n-p "This node has clones. Sync your changes to all clones?")
      (progn (org-clones--update-clones)
	     (message "Clones updated."))
    (if (y-or-n-p "You don't want to update? Fine. Unsync this clone?")
	(progn 
	  (org-clones--unsync-this-clone)
	  (message "Removed this node from the clone list."))
      (if (y-or-n-p "Return node to its previous state?")
	  (org-clones--discard-edit)
	(message "You have chosen an impossible path. The clones are updated. Fuck off.")
	(org-clones--update-clones)))))

(defun org-clones--discard-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (interactive)
  (org-clones--replace-headline
   (car org-clones--restore-state))
  (org-clones--replace-body
   (cdr org-clones--restore-state))
  (org-clones--put-clone-effects)
  (org-clones-edit-mode -1))

;;;; Commands

;;;###autoload
(defun org-clones-store-marker ()
  "Store a marker to create a clone."
  (interactive)
  (setq org-clones--temp-marker (point-marker))
  (message "Stored %S for clone creation."
	   (org-no-properties 
	    (org-clones--get-headline-string))))

;;;###autoload
(defun org-clones-create-clone-from-marker ()
  "Create a clone from the previously stored marker."
  (interactive)
  (if org-clones--temp-marker
      (org-clones-create-clone org-clones--temp-marker)
    (user-error "You have not stored the source node yet."))
  (setq org-clones--temp-marker nil))

;;;###autoload
(defun org-clones-create-clone (&optional source-marker)
  "Insert a new headline, prompt the user for the source node,
add clone properties to the source, add clone properties to the clone
and add the headline and body from the source to the clone.  
SOURCE-POINT is a marker for the location of the source node"
  (interactive)
  (let (source-headline source-body source-id source-clone-list	clone-id)
    ;; At the new heading...
    (org-insert-heading-respect-content)
    (setq clone-id (org-id-get-create))
    ;; At the source heading...
    (cl-flet ((source-node-prep
	       nil
	       (org-back-to-heading)
	       (org-clones--remove-clone-effects)
	       (setq source-headline (org-clones--get-headline-string))
	       (setq source-body (org-clones--get-body-string))
	       (when (string= "" source-body)
		 (org-clones--insert-blank-body)
		 (setq source-body org-clones-empty-body-string))
	       (setq source-id (org-id-get-create))
	       (org-entry-add-to-multivalued-property (point)
						      "ORG-CLONES"
						      clone-id)
	       (setq source-clone-list (org-clones--get-clone-ids))
	       (org-clones--put-clone-effects)))
      (if source-marker
	  (with-current-buffer (marker-buffer source-marker)
	    (source-node-prep))
	(save-excursion 
	  (org-clones--prompt-for-source-and-move)
	  (source-node-prep))))
    ;; For each clone from the source, add new clone id
    (cl-loop for clone-id in source-clone-list
	     do
	     (org-clones--with-point-at-id clone-id
	       (cl-loop for clone in source-clone-list
			do
			(unless (string= clone (org-id-get-create))
			  (org-entry-add-to-multivalued-property (point)
								 "ORG-CLONES"
								 clone)))))
    ;; At the new clone...
    (org-entry-add-to-multivalued-property (point)
					   "ORG-CLONES"
					   source-id)
    (org-clones--replace-headline source-headline)
    (org-clones--replace-body source-body)
    (org-clones--put-clone-effects)))

(defun org-clones--put-clone-effects ()
  "Do whatever is necessary to colorize the clones."
  nil)

;;;###autoload
(defun org-clones-initialize ()
  "Initialize the buffer. This is used instead of invoking
a minor mode."
  (org-clones--put-all-clone-effects)
  (org-clones--cursor-sensor-mode-check))

;;;; Footer

(provide 'org-clones)

