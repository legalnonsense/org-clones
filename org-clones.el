;;; org-clones.el --- Clone and sync Orgmode headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; URL: http://www.github.com/legalnonsense/org-clones
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

;; Add `org-clones-initialize' to your org-mode hook.
;; With the cursor on an org heading, run
;; `org-clones-create-clone'. You have created a clone. 
;; Edit the clone, and your edits will sync.
;; You can also create clones across files with 
;; `org-clones-store-marker' at an org heading and
;; running `org-clones-create-clone-from-marker'
;; at the location (of any file) where you want the
;; clone. 

;; See http://www.github.com/legalnonsense/org-clones for more
;; information. 

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
(require 'org-ml)

;;;; Faces

(defface org-clones-current-clone
  '((t (:background "orchid" :box (line-width -1))))
  "Face applied when the point is inside a cloned
node (i.e., headline or body."
  :group 'org-clones)

;;;; Customization

(defcustom org-clones-clone-prefix-string "◈ "
  "String prepended to the headline of a cloned node."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-clone-headline-overlay-props
  `(before-string ,org-clones-clone-prefix-string
		  org-clones-headline-overlay t
		  evaporate t)
  "Overlays placed on each clone, regardless of whether the 
cursor is on the cloned node."
  :group 'org-clones
  :type 'plist)

(defcustom org-clones-empty-body-string "[empty clone body]\n"
  "Place holder inserted into clones with empty bodies.
Can be any string other than whitespace. Must end with a newline.
Must be a string other than whitespace."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-empty-headling-string "[empty clone headline]"
  "Place holder inserted into clones with empty headlines.
Must be a string other than whitespace."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-prompt-before-syncing t
  "Whether to prompt the user before syncing changes to all clones."
  :group 'org-clones
  :type 'boolean)

;;;; Variables

(defvar org-clones-cursor-sensor-functions
  '(org-clones--text-watcher-func-clone-watcher)
  "List of cursor-sensor-functions to apply to the headline
and body of cloned nodes.")

(defvar org-clones--restore-state nil
  "When editing a clone, save the current headline and body
to restore if the edit is abandoned.")

(defvar org-clones--temp-marker nil
  "Temporary storage for a marker for clone creation.")

(defvar org-clones--temp-overlay nil
  "Temporary holder for the transient headline 
or body overlay.")
(make-local-variable 'org-clones--temp-overlay)

(defvar org-clones--headline-re "^*+ "
  "Org headline regexp.")

(defvar org-clones--not-whitespace-re "[^[:space:]]"
  "Regexp matching any non-whitespace charcter.")

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

(defmacro org-clones--iterate-over-all-clones-in-buffer (&rest body)
  "Execute BODY at any clone which has a non-nil :ORG-CLONES: property, in 
the buffer (but do not iterate over clones outside the buffer)."
  ;; This method appears to be faster than `org-ql' and
  ;; is *much* faster than `org-map-entries'.
  `(save-excursion
     (goto-char (point-min))
     (while (re-search-forward org-property-drawer-re nil t)
       (goto-char (match-beginning 0))
       (when (re-search-forward ":ORG-CLONES:" nil (match-end 0))
	 (when (org-entry-get (point) "ORG-CLONES")
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

(defun org-clones--at-headline-p ()
  "Is the point inside a headline?"
  (and (outline-on-heading-p t)
       (<= (point) (org-clones--get-headline-end))
       (>= (point) (org-clones--get-headline-start))))

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
  (buffer-substring-no-properties (org-clones--get-headline-start)
				  (org-clones--get-headline-end)))

;; (defun org-clones--replace-headline (headline)
;;   "Replace the headline text at point with HEADLINE."
;;   (save-excursion 
;;     (org-clones--delete-headline)
;;     (org-clones--goto-headline-start)
;;     (insert headline)))

;; TODO re-write this to get rid of org-ml
(defun org-clones--replace-headline (headline)
  "Replace the headline text at point with HEADLINE"
  (org-ml-update-this-headline*
    (org-ml-set-property :title `(,(concat headline " ")) it)))

;;;; Body functions 

(defun org-clones--insert-blank-body ()
  "Insert `org-clones-empty-body-string' into the body 
of the current node."
  (org-clones--replace-body org-clones-empty-body-string))

(defun org-clones--get-body-end ()
  "Get the end point of the body of the current node."
  (save-excursion (org-clones--goto-body-end)))

(defun org-clones--node-body-p ()
  "Does this node have a body (i.e., a section in org-element
parlance?"
  (org-clones--parse-body))

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

(defun org-clones--at-body-p ()
  "Is the point inside the body of a node?"
  (when-let ((start (org-clones--get-body-start))
	     (end (org-clones--get-body-end)))
    (and (<= (point) end)
	 (>= (point) start))))

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
  (org-no-properties 
   (org-element-interpret-data 
    (org-clones--get-section-elements))))

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

;;;; Clone interaction 

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

(defun org-clones--prompt-for-source-node-and-move ()
  "Prompt user for a node and move to it."
  (org-goto))

(defun org-clones--fold-property-drawer (&optional unfold)
  "Fold the property drawer at the current heading. If 
UNFOLD is non-nil, then unfold the drawer."
  (save-excursion
    (org-back-to-heading)
    (re-search-forward org-property-drawer-re)
    (goto-char (match-beginning 0))
    (org-flag-drawer (not unfold))))

(defun org-clones--sync-clones ()
  "Update all clones of the current node to match
the headline and body of the current node and
place text properties and overlays in the cloned nodes."
  (interactive)
  (org-clones--remove-clone-effects)
  (let ((headline (org-clones--get-headline-string))
	(body (if (string= "" (org-clones--get-body-string))
		  org-clones-empty-body-string
		(org-clones--get-body-string))))
    ;; Replace the body in the current node to 
    ;; normalize whitespace 
    (org-clones--replace-body body)
    (org-clones--put-clone-effects)
    (org-clones--fold-property-drawer)
    (org-clones--iterate-over-clones
     (org-clones--remove-clone-effects)
     (org-clones--replace-headline headline)
     (org-clones--replace-body body)
     (org-clones--put-clone-effects)
     (org-clones--fold-property-drawer)))
  (message "Clones updated."))

;;; Text properties and overlays 

(defun org-clones--put-headline-overlay ()
  "Put overlays in `org-clones-clone-headline-overlay-props' on the current node." 
  (org-clones--remove-headline-overlay)
  (let ((headline-overlay (make-overlay (org-clones--get-headline-start)
					(org-clones--get-headline-end)))
	(len (length org-clones-clone-headline-overlay-props))
	(i 0))
    (while (< i len)
      (overlay-put headline-overlay
		   (nth i org-clones-clone-headline-overlay-props)
		   (nth (1+ i) org-clones-clone-headline-overlay-props))
      (setq i (+ i 2)))
    ;; So we can find it later...
    headline-overlay))

(defun org-clones--remove-headline-overlay ()
  "Remove the overlays in `org-clones-clone-headline-overlay-props' from the 
current node."
  (org-with-wide-buffer
   (remove-overlays (org-clones--get-headline-start)
		    (org-clones--get-headline-end)
		    'org-clones-headline-overlay t)))

(defun org-clones--put-cursor-sensor-props (&optional remove)
  "Remove `org-clones-cursor-sensor-functions' from the the current node."
  (cl-loop for start in `(,(org-clones--get-headline-start)
			  ,(org-clones--get-body-start))
	   for end in `(,(org-clones--get-headline-end)
			,(org-clones--get-body-end))
	   do (cl-loop for func in org-clones-cursor-sensor-functions
		       do (org-clones--change-cursor-sensor-prop
			   start end func remove))))

(defun org-clones--remove-cursor-sensor-props ()
  "Remove `org-clones-cursor-sensor-functions' from the headline and 
body of the current node."
  (org-clones--put-cursor-sensor-props 'remove))

(defun org-clones--put-clone-effects ()
  "Put overlay and text properties at the current
node."
  (org-clones--put-cursor-sensor-props)
  (org-clones--put-headline-overlay))

(defun org-clones--remove-clone-effects ()
  "Remove overlay and text properties at the current
node."
  (org-clones--remove-cursor-sensor-props)
  (org-clones--remove-headline-overlay))

(defun org-clones--reset-clone-effects ()
  (org-clones--remove-cursor-sensor-props)
  (org-clones--put-clone-effects))

;;;; Emergency functions

(defun org-clones--reset-all-clone-effects-in-buffer ()
  "Remove all clone effets on all clones in buffer."
  (org-clones--iterate-over-all-clones-in-buffer
   (org-clones--remove-clone-effects)
   (org-clones--put-clone-effects)))

(defun org-clones--remove-all-clone-effects-in-buffer ()
  "Remove all clone effets on all clones in buffer."
  (org-clones--iterate-over-all-clones-in-buffer
   (org-clones--remove-clone-effects)))

;;;; Cursor-sensor-functions

(defun org-clones--change-cursor-sensor-prop (beg end func &optional remove)
  "Add FUNC to the list of cursor-sensor-functions from BEG to END. If REMOVE
is non-nil, then remove FUNC from the cursor-sensor-functions property."
  (cl-loop
   for pos from beg to end
   do (let ((func-list (get-text-property pos 'cursor-sensor-functions)))
	(put-text-property
	 pos (1+ pos) 'cursor-sensor-functions
	 (if remove
	     (pcase func-list
	       ((pred listp)
		(remq func func-list))
	       ((guard (equal func func-list)) nil)
	       (_ func-list))
	   (pcase func-list
	     ((pred null) (list func))
	     ((pred listp) (cl-pushnew func func-list))
	     (_ (if (equal func-list func)
		    func
		  (list func func-list)))))))))

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
       (defun ,function-name (_window last-pos entered-or-left)
	 (let ((cursor-sensor-inhibit t))
	   (pcase entered-or-left
	     (`entered
	      (setq ,var-name ,@storage-form)
	      ,@enter)
	     (`left
	      ;; I think these save excursions needs to be
	      ;; separate because the user functions could
	      ;; move the point.
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

(defun org-clones--get-relevant-string ()
  "Get the body string if at the node body;
get the headline string if at the headline."
  (cond ((org-clones--at-headline-p)
	 (org-clones--get-headline-string))
	((org-clones--at-body-p)
	 (org-clones--get-body-string))
	(t (error
	    (concat "Point is not at a headline or body."
		    "There is no string to get.")))))

(org-clones--create-text-watcher clone-watcher
  :enter ((message "Entered cloned!")
	  (let (beg end)
	    (cond ((org-clones--at-headline-p)
		   (setq beg (org-clones--get-headline-start))
		   (setq end (org-clones--get-headline-end)))
		  ((org-clones--at-body-p)
		   (setq beg (org-clones--get-body-start))
		   (setq end (org-clones--get-body-end))))
	    (move-overlay org-clones--temp-overlay beg end)
	    (org-clones--begin-edit)))
  :exit ((delete-overlay org-clones--temp-overlay))
  :change ((org-clones--prompt-before-syncing))
  :no-change ((message "Exited clone with no changes."))
  :storage-form ((cond ((org-clones--at-headline-p)
			(org-clones--get-headline-string))
		       ((org-clones--at-body-p)
			(org-clones--get-body-string)))))

;;;; Editing clones

(defun org-clones--begin-edit ()
  "Save the state of a clone before an edit begins."
  (setq org-clones--restore-state
	(cons (org-clones--get-headline-string)
	      (org-clones--get-body-string))))

(defun org-clones--prompt-before-syncing ()
  "Ask the user if they want to edit the node
without syncing the clones. If so, unlink the current 
clone."
  (if (not org-clones-prompt-before-syncing)
      (org-clones--sync-clones)
    (let ((last-nonmenu-event t))
      ;; Without this let, y-or-n-p pops a dialog box
      (if (y-or-n-p "Sync your changes to all clones?")
	  (org-clones--sync-clones)
	(if (y-or-n-p "Unsync this clone?")
	    (org-clones-unsync-this-clone)
	  (if (y-or-n-p "Discard this edit?")
	      (org-clones--discard-edit)
	    ;; If the user takes an impossible path,
	    ;; send them back to the beginning.
	    (org-clones--prompt-before-syncing)))))))

(defun org-clones--discard-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (org-clones--replace-headline
   (car org-clones--restore-state))
  (org-clones--replace-body
   (cdr org-clones--restore-state))
  (org-clones--put-clone-effects)
  (message "Org-clones: Discarded edit.")
  (setq org-clones--restore-state nil))

;;;; Commands

;; FIX ME: This does not remove the clones from the current node
;; ... OR DOES IT!?
(defun org-clones-unsync-this-clone ()
  (interactive)
  (let ((this-id (org-id-get))
	(clone-ids (org-clones--get-clone-ids)))
    (cl-loop for clone-id in clone-ids
	     do (org-clones--with-point-at-id clone-id
		  (org-entry-remove-from-multivalued-property
		   (point) "ORG-CLONES" this-id)
		  (unless (org-clones--get-clone-ids)
		    (org-clones--remove-clone-effects))))
    (org-set-property "ORG-CLONES" "nil")
    (org-clones--remove-clone-effects)
    (message "This node is no longer synced with other clones.")))

;;;###autoload
(defun org-clones-store-marker ()
  "Store a marker to create a clone."
  (interactive)
  (org-back-to-heading)
  (setq org-clones--temp-marker (point-marker))
  (message "Stored %S for clone creation."
	   (org-no-properties 
	    (org-clones--get-headline-string))))

;;;###autoload
(defun org-clones-create-clone-from-marker ()
  "Create a clone from the previously stored marker."
  (interactive)
  (if org-clones--temp-marker
      ;; Make sure org-clones-mode is active in the buffer
      (progn 
	(unless org-clones-mode (org-clones-mode 1))
	(org-clones-create-clone org-clones--temp-marker))
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
	       ()
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
      (save-excursion 
	(if source-marker
	    (with-current-buffer (marker-buffer source-marker)
	      (goto-char source-marker)
	      (source-node-prep))
	  (org-clones--prompt-for-source-node-and-move)
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

;;;; Initialization 

(defun org-clones--initialize-temp-overlay ()
  "Initialize temporary overlay (`org-clones--temp-overlay')
used to highlight the clone at point. This overlay is reused
each time the point is in the headline or body of a cloned node."
  (setq org-clones--temp-overlay
	(make-overlay 1 2 nil nil t))
  (overlay-put org-clones--temp-overlay
	       'face 'org-clones-current-clone)
  (delete-overlay org-clones--temp-overlay))

(defun org-clones--initialize-overlays-in-buffer ()
  "Put overlays on all clones in current buffer."
  (org-clones--iterate-over-all-clones-in-buffer 
   (org-clones--put-headline-overlay)))

;;;###autoload
(define-minor-mode org-clones-mode
  "Org heading transclusion minor mode."
  nil
  " ORG-CLONES"
  nil
  (if org-clones-mode
      (progn
	(cursor-sensor-mode -1)
	(org-clones--initialize-temp-overlay)
	(org-clones--initialize-overlays-in-buffer)
	(cursor-sensor-mode 1))))

;;;; Footer

(provide 'org-clones)
