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
(require 'ov)
(require 'org-ml)

;;;; Faces

(defface org-clones-clone
  '((t (:background "orchid")))
  "Body of cloned nodes."
  :group 'org-clones)

;;;; Customization

(defcustom org-clones-complete-edit-keybind "C-c C-c"
  "Keybinding to complete editing a clone. Must be a 
string acceptable to the `kbd' function."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-discard-edit-keybind "C-c C-k"
  "Keybinding to complete editing a clone. Must be a 
string acceptable to the `kbd' function."
  :group 'org-clones
  :type 'string)

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

(defvar org-clones--restore-state nil
  "When editing a clone, save the current headline and body
to restore if the edit is abandoned.")

(defvar org-clones--temp-marker nil
  "Temporary storage for a marker for clone creation in 
separate file.")

(defvar org-clones--previous-header-line header-line-format
  "Holds the previous `header-line-format' value to restore later.")

(defvar org-clones--headline-re "^*+ " ; outline-regexp
  "Org headline regexp.")

(defvar org-clones--not-whitespace-re "[^[:space:]]"
  "Regexp to match any non-whitespace charcter.")

;;;; Keymaps

(setq org-clones-overlay-map
      (let ((map (make-keymap)))
	(suppress-keymap map)
	(define-key map [remap self-insert-command] #'org-clones--prompt-before-edit)
	(define-key map [remap newline] #'org-clones--prompt-before-edit)
	map))
      ;;"Keymap for overlays put on clones.")

;;;; Macros

(defmacro org-clones--inhibit-read-only (&rest body)
  "Quick substitute for (let ((inhibit-read-only t)) ...)."
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro org-clones--iterate-over-clones (&rest body)
  "Execute BODY at each clone of node at point."
  `(save-excursion
     (when-let ((clone-ids (org-clones--get-clone-ids)))
       (cl-loop for clone-id in clone-ids
		do (org-clones--with-point-at-id
		     clone-id
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
  (if-let ((todo (org-get-todo-state)))
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
leading stars."
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
  (save-excursion 
    (org-clones--goto-headline-end)))

(defun org-clones--delete-headline ()
  "Replace the headline of the heading at point."
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
  (org-ml-update-this-headline*
    (org-ml-set-property :title `(,headline) it)))

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

(defun org-clones--get-body-as-string ()
  "Get the body of the current node as a string." 
  (org-element-interpret-data 
   (org-clones--get-section-elements)))

(defun org-clones--get-section-elements ()
  "Reduce the section data to the component elements,
e.g., '((paragraph (...))
        (src-block (...)) ...)."
  (cddar (org-clones--parse-body)))

(defun org-clones--get-section-plist ()
  "Get the plist associated with the section element, 
e.g. (:begin 1 :end 10 :contents-begin ...)."
  (cadar (org-clones--parse-body)))

(defun org-clones--delete-body ()
  (org-clones--inhibit-read-only
   (when-let* ((prop-list (org-clones--get-section-plist))
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
	(body (if (string= "" (org-clones--get-body-as-string))
		  org-clones-empty-body-string
		(org-clones--get-body-as-string))))

    ;; Replace the headline in the current node to ensure
    ;; there aren't differences in white space between the nodes
    (org-clones--replace-body body)
    (org-clones--put-clone-effects)
    (org-clones--iterate-over-clones
     (org-clones--remove-clone-effects)
     (org-clones--replace-headline headline)
     (org-clones--replace-body body)
     (org-clones--put-clone-effects))))

(defun org-clones--unsync-this-clone ()
  (let ((this-id (org-id-get))
	(clone-ids (org-clones--get-clone-ids)))
    (org-clones--remove-clone-effects)
    (cl-loop for clone-id in clone-ids
	     do (org-clones--with-point-at-id clone-id
		  (org-entry-remove-from-multivalued-property
		   (point) "ORG-CLONES" this-id)
		  (unless (org-clones--get-clone-ids)
		    (org-clones--remove-clone-effects))))
    (message "This node is now independent of other clones. It will not be synced.")))

;;; Text properties and overlays 

(defun org-clones--make-read-only ()
  "Make the node at point read-only, for the purposes
of locking edits of the headline and body."
  (put-text-property (org-clones--get-headline-start)
		     (org-clones--get-headline-end)
		     'org-clones t)
  (put-text-property (org-clones--get-headline-start)
		     (org-clones--get-headline-end)
		     'read-only t)
  (put-text-property (org-clones--get-body-start)
		     (org-clones--get-body-end)
		     'org-clones t)
  (put-text-property (org-clones--get-body-start)
		     (org-clones--get-body-end)
		     'read-only t))

(defun org-clones--remove-read-only ()
  "Remove read-only text properties for the current node."
  (let ((inhibit-read-only t))
    (remove-text-properties (org-clones--get-headline-start)
			    (org-clones--get-headline-end)
			    '(read-only t 'face t))
    (remove-text-properties (org-clones--get-body-start)
			    (org-clones--get-body-end)
			    '(read-only t 'face t))))

(defun org-clones--put-text-properties ()
  "Make the node at point read-only, for the purposes
of locking edits of the headline and body."
  ;; For the headline...
  (cl-loop
   with beg = (org-clones--get-headline-start)
   with end = (org-clones--get-headline-end)
   for (prop . val) in `((org-clones-headline . t)
			 (read-only . t))
   do (put-text-property beg end prop val))

  ;; For the body...
  (cl-loop
   with beg = (org-clones--get-body-start)
   with end = (org-clones--get-body-end)
   for (prop . val) in `((org-clones-body . t)
			 (read-only . t))
   do (put-text-property beg end prop val)))

(defun org-clones--remove-text-properties ()
  "Remove read-only text properties for the current node."
  (let ((inhibit-read-only t))
    (remove-text-properties (org-clones--get-headline-start)
			    (org-clones--get-headline-end)
			    '(read-only t org-clones t))

    (remove-text-properties (org-clones--get-body-start)
			    (org-clones--get-body-end)
			    '(read-only t org-clones t))))

(defun org-clones--remove-overlays ()
  "Remove the clone overlay at the headline and body
of the current node."
  (ov-clear (org-clones--get-headline-start)
	    (org-clones--get-headline-end)
	    'any)
  (when (org-clones--node-body-p)
    (ov-clear (org-clones--get-body-start)
	      (org-clones--get-body-end)
	      'any)))

(defun org-clones--put-clone-effects ()
  "Put overlay and text properties at the current
node."
  ;;(org-clones--put-text-properties)
  (org-clones--put-overlays))

(defun org-clones--remove-clone-effects ()
  "Remove overlay and text properties at the current
node."
  ;;(org-clones--remove-text-properties)
  (org-clones--remove-overlays))

(defun org-clones--put-all-clone-effects-in-buffer ()
  "Clear all overlays and text properties that might have been set 
previously. Place a new set of overlays and text properties at each
node with a ORG-CLONES property."
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (org-with-wide-buffer
   (org-ql-select (current-buffer)
     '(property "ORG-CLONES")
     :action (lambda ()
	       (org-clones--iterate-over-clones
		(org-clones--remove-clone-effects)
		(org-clones--put-clone-effects))))))

;; (defun org-clones--remove-all-text-props-in-buffer ()
;;   (org-clones--inhibit-read-only
;;    (cl-loop for points being the intervals of (current-buffer) property 'org-clones
;; 	    if (get-text-property (car points) 'org-clones)
;; 	    do (remove-list-of-text-properties (car points) (cdr points)
;; 					       '(asdf face read-only)))))

(defun org-clones--remove-all-clone-effects-in-buffer ()
"Remove clone effects from all clones."
(setq org-ql-cache (make-hash-table :weakness 'key))
(org-with-wide-buffer
 (org-ql-select (current-buffer)
   '(property "ORG-CLONES")
   :action (lambda ()
	     (org-clones--iterate-over-clones
	      (org-clones--remove-clone-effects))))))

(defun org-clones--put-overlays ()
  "Put the clone overlay at the headline and body
of the current node."
  (ov (org-clones--get-headline-start)
      (org-clones--get-headline-end)
      'face 'org-clones-clone
      'keymap org-clones-overlay-map
      'before-string org-clones-clone-prefix-string
      'evaporate t
      'org-clones t)

  (ov (org-clones--get-body-start)
      (org-clones--get-body-end)
      'face 'org-clones-clone
      'keymap org-clones-overlay-map
      'evaporate t
      'org-clones t))

(defun org-clones--edit-clone ()
  "Start edit mode."
  (interactive)
  (org-clones-edit-mode 1))

;;;; Editing clones

(defun org-clones--prompt-before-edit ()
  "Ask the user if they want to edit the node
without syncing the clones. If so, unlink the current 
clone."
  (interactive)
  (if (yes-or-no-p "This node has clones. Sync your edits to the clones?")
      (org-clones--edit-clone)
    (org-clones--unsync-this-clone)))

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

(defun org-clones--complete-edit ()
  "Sync all clones with the current state of the 
node being edited."
  (interactive)
  (org-clones--update-clones)
  (org-clones-edit-mode -1))

;;;; Commands


;;;###autoload
(defun org-clones-store-marker ()
  "Store a marker to create a clone."
  (interactive)
  (setq org-clones--temp-marker (point-marker))
  (message "Stored %S for clone creation."
	   (org-clones--get-headline-string)))

;;;###autoload
(defun org-clones-create-clone-from-marker ()
  "Create a clone from the previously stored marker."
  (interactive)
  (if org-clones--temp-marker
      (org-clones-create-clone org-clones--temp-marker)
    (user-error "You have not marked the source node yet."))
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
	       (setq source-body (org-clones--get-body-as-string))
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
	     do (org-clones--with-point-at-id clone-id
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

;;;; Minor modes

;;;###autoload
(define-minor-mode org-clones-mode
  "Org heading transclusion minor mode."
  nil
  " ORG-CLONES"
  nil
  (if org-clones-mode
      (org-clones--put-all-clone-effects-in-buffer)
    (org-clones--remove-all-clone-effects-in-buffer)))

(define-minor-mode org-clones-edit-mode
  "Mode to edit clones."
  nil
  " EDIT-CLONE"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd org-clones-complete-edit-keybind) #'org-clones--complete-edit)
    (define-key map (kbd org-clones-discard-edit-keybind) #'org-clones--discard-edit)
    map)
  (if org-clones-edit-mode
      (progn
	(setq org-clones--restore-state
	      (cons (org-clones--get-headline-string)
		    (org-clones--get-body-as-string)))
	(org-clones--remove-clone-effects)
	(setq org-clones--previous-header-line header-line-format)
	(setq header-line-format
	      (concat
	       "Edit cloned node. \""
	       org-clones-complete-edit-keybind
	       "\" to finish and update. \""
	       org-clones-discard-edit-keybind
	       "\" to abandon.")))
    (setq org-clones--restore-state nil)
    (setq header-line-format
	  org-clones--previous-header-line)))

;;;; Support

(defun org-clones--delete-buffer ()
  (org-clones--inhibit-read-only
   (erase-buffer)))

;;;; Development

(cl-defmacro org-clones--create-text-watcher (name start end &optional &key
						   enter exit storage-form
						   change no-change disable
						   other-props)
  "NAME is the name used to create the underlying function and variable.

Text properties will go from START to END.

All other keys are optional:

ENTER is a form executed when the cursor enters the text field.
When the cursor enters the text field, STORAGE-FORM will run 
and store the return value in a variable named 
 `org-clones--text-watcher-storage-NAME'.
This variable is available to retrieve the initial value of the
text field after the cursor enters the field.

If STORAGE-FORM is omitted or nil, the default behavior is to store the
value of `buffer-substring' from START to END. 

EXIT is a form executed when cursor exits the text field. 

CHANGE is the form executed if there was a change of the text
from START to END, comparing the value of STORAGE-FORM before 
and after the cursor leaves the field. This comparison is done 
with `string='. 

NO-CHANGE is a form executed if there was no change to the text. 

Note: EXIT will be evaluated _before_ CHANGE or NO CHANGE.

If DISABLE is t, it removes all text properties set by the macro call. 

OTHER-PROPS are other text properties to from START to END. They 
will also be removed if DISABLE is t.

This marco will automatically turn `cursor-sensor-mode' on or off
as appropriate."

  (declare (indent defun))
  ;; Create the function and variable names for later use
  (let ((var-name
	 (intern (concat "org-clones--text-watcher-storage-"
			 (symbol-name name))))
	(function-name
	 (intern (concat "org-clones--text-watcher-func-"
			 (symbol-name name)))))
    ;; If the user did not provide STORAGE-FORM,
    ;; set it to the default
    ;;
    ;; TODO: the default should probably get the text-property
    ;; start and end range rather than using START and END
    (when (null storage-form)
      (setq storage-form `((buffer-substring ,start ,end))))
    `(progn
       ;; Create a unique storage variable
       (if (boundp ',var-name)
	   (setq ,var-name nil)
	 (defvar ,var-name nil))
       ;; Create a function name that will be added to the
       ;; cursor-sensor-functions list.
       (defun ,function-name (window last-pos entered-or-left)
	 ;; Check if we entered or left the cursor field, and call
	 ;; the appropriate functions
	 (pcase entered-or-left
	   ;; If we entered the field:
	   (`entered
	    ;; Update the storage value...
	    (setq ,var-name ,@storage-form)
	    ;; ... and run ENTER.
	    ,@enter)

	   ;; If we left the field:
	   (`left
	    (setq xxx (1+ xxx))
	    ;; Run EXIT...
	    ,@exit
	    ;; ...check if the field was modified...	  
	    (if (string=
		 (prog2
		     (setq cursor-sensor-inhibit t)
		     (save-excursion
		       (goto-char last-pos)
		       (message "%s" ,@storage-form)
		       ,@storage-form)
		   (setq cursor-sensor-inhibit nil))
		 ,var-name)
		;; ...if not, run NO-CHANGE and set the storage to nil...
		(progn 
		  ,@no-change
		  (setq ,var-name nil))
	      ;; ...otherwise, update the storage to the current value...
	      (setq ,var-name
		    (prog2
			(setq cursor-sensor-inhibit t)
			(save-excursion
			  (goto-char last-pos)
			  (message "%s" ,@storage-form)
			  ,@storage-form)
		      (setq cursor-sensor-inhibit nil)))

	      ;; ...run CHANGE...
	      (prog2
		  (setq cursor-sensor-inhibit t)
		  (save-excursion
		    (goto-char last-pos)
		    ,@change)
		(setq cursor-sensor-inhibit nil))
	      ;; ...and reset the storage variable.
	      (setq ,var-name nil)))))


       ;; If we are disabling the function:
       (if ,disable
	   (progn 
	     ;; Remove the function from the function list...
	     (put-text-property
	      ,start
	      ,end
	      'cursor-sensor-functions
	      ;; (This removes the function from the cursor-sensor-functions
	      ;; list while keeping other functions in the list)
	      (remq ',function-name
		    (-list (get-text-property ,start
					      'cursor-sensor-functions))))
	     ;; ...remove other text properties set by the user...
	     (when ',other-props
	       (remove-text-properties ,start ,end ',other-props))
	     ;; ...and reset the value of var-name.
	     (setq ,var-name nil))
	 
	 ;; Otherwise, we are enabling the function:
	 ;; Put it in the cursor-sensor-function list
	 ;; (unless it's there already)...
	 (unless (memq ',function-name (get-text-property
					,start
					'cursor-sensor-functions))

	   ;; TODO:
	   ;; For now, only one function can be added to the 
	   ;; `cursor-sensor-functions' property. If a second function is added
	   ;; to text that previously had a value for that property, the 
	   ;; value will be overridden, rather than added. Fixing this requires
	   ;; writing a function akin to `add-face-text-property' which preserves
	   ;; the previous value of the text property. The only way I see to do this
	   ;; is to iterate over each point in the text property range.
	   ;; (`add-face-text-property' is a C function.)
	   ;;
	   ;; I do not see the use-case for overlapping text fields
	   ;; so this is not a priority (though should be an easy fix). 
	   (put-text-property
	    ,start
	    ,end
	    'cursor-sensor-functions
	    (list ',function-name))) ;; See next comment

	 ;; BUG: the below (commented) code does not work (it should replace
	 ;; (list ',function-name), above) because it will pick up the
	 ;; previous property value and apply it to the current range,
	 ;; thereby changing the start and end of the previous property value.
	 ;;
	 ;; (append (-list ',function-name) (get-text-property
	 ;; 				     ,start
	 ;; 				     'cursor-sensor-functions))))
	 
	 ;; ...and add any other text properties supplied by the user.
	 (when ',other-props
	   (add-text-properties ,start ,end ',other-props)))

       ;; Should `cursor-sensor-mode' be turned on? 
       (if (save-excursion
	     (save-restriction
	       (widen)
	       ;; Are there any cursor-sensor-function text properties
	       ;; in the buffer?
	       (next-single-property-change (point-min) 'cursor-sensor-functions)))
	   ;; If so, enable cursor-sensor-mode...
	   (cursor-sensor-mode 1)
	 ;; ...otherwise, disable it. 
	 (cursor-sensor-mode -1)))))



;;;; Footer

(provide 'org-clones)



;;; Org-clones.el ends here
(cl-defmacro xxx (&key a)
  (if (null a)
      (setq a 'b))
  `(progn ',a))

(macroexpand '(xxx))
