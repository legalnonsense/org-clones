;;; org-clones.el --- Clone Orgmode headings  -*- lexical-binding: t; -*-

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
;; the node's headline and body.  An overlay is then placed over each clone.
;; If the user attempts to edit the clone, they are prompted to either enter
;; an edit-mode, which will sync all changes to other clones upon completion,
;; or to unlink the clone. 

;;;; Installation

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file:

;; (require 'org-clones)

;;;; Usage

;; M-x `org-clones-mode' (or add it to your org-mode hook).
;; With the cursor on an org heading, run
;; `org-clones-create-clone'.  You have created a clone. 
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

;;;; Customization

(defgroup org-clones ()
  "Heading transclusion for Orgmode files."
  :group 'org
  :prefix "org-clones-")

(defcustom org-clones-commit-edit-shortcut "C-c C-c"
  "Shortcut to commit edits when in `org-clones-edit-mode'
Accepts any string acceptable to `kbd'."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-abort-edit-shortcut "C-c C-k"
  "Shortcut to abort edits when in `org-clones-edit-mode'
Accepts any string acceptable to `kbd'."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-start-edit-shortcut "C-c C-c"
  "Shortcut to initiate `org-clones-edit-mode'
Accepts any string acceptable to `kbd'."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-jump-to-next-clone-shortcut "n"
  "Shortcut to jump to next clone via `org-clones-jump-to-clones'
Accepts any string acceptable to `kbd'."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-clone-prefix-icon "◈ "
  "String prepended to the headline of a cloned node."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-empty-body-string "[empty clone body]"
  "Place holder inserted into clones with empty bodies.  
Can be any string other than whitespace.  Must end with a newline.  
Must be a string other than whitespace."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-empty-headling-string "[empty clone headline]"
  "Place holder inserted into clones with empty headlines.  
Must be a string other than whitespace."
  :group 'org-clones
  :type 'string)

(defcustom org-clones-use-popup-prompt nil
  "Whether to use a dialog box to prompt before syncing clones."
  :group 'org-clones
  :type 'boolean)

(defcustom org-clones-prompt-before-syncing nil
  "Whether to prompt the user before syncing changes to all clones."
  :group 'org-clones
  :type 'boolean)

;;;; Keymaps

(defvar org-clones--clone-cycle-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd org-clones-jump-to-next-clone-shortcut)
      #'org-clones-jump-to-clones)
    map)
  "Transient keymap for clone cycling.")

(defvar org-clones--transient-clone-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd org-clones-start-edit-shortcut)
      #'org-clones--begin-edit)
    map)
  "Keymap for transient overlays.")

;;;; Faces

(defgroup org-clones-faces ()
  "Faces for org-clones."
  :group 'org-clones)

(defface org-clones-current-clone
  '((t (:background "orchid" :foreground "black")))
  "Face applied when the point is inside a cloned
  node (i.e., headline or body)."
  :group 'org-clones-faces)

(defface org-clones-clone
  '((t (:background "black")))
  "Face applied to the headline and body of each cloned node."
  :group 'org-clones-faces)

;;;; Variables

(defvar org-clones--cursor-sensor-functions '(org-clones--text-watcher)
  "List of cursor-sensor-functions to apply to the headline
  and body of cloned nodes.")

(defvar org-clones--temp-marker nil
  "Temporary storage for a marker for clone creation.")

(defvar org-clones--org-headline-re org-outline-regexp
  "Org headline regexp.")

(defvar org-clones--not-whitespace-re "[^[:space:]]"
  "Regexp matching any non-whitespace charcter.")

(defvar org-clones--clone-cycle-list nil
  "Temporary storage when cycling through clones.")

(defvar org-clones--clone-cycle-buffer-list nil
  "Temporary storage for the buffer name and `header-line-format'
  list when cycling through clones.")

(defvar org-clones--clone-cycle-header-msg
  "Press 'n' to jump to next clone; any key to quit."
  "Message displayed in the header-line when cycling
through clones.")

(defvar org-clones--transient-overlay-properties
  `(face org-clones-current-clone
	 keymap ,org-clones--transient-clone-overlay-map
	 priority 1000
	 evaporate t)
  "Properties to be added to the transient overlay.")

(defvar org-clones--clone-headline-overlay-props
  `(before-string ,org-clones-clone-prefix-icon
		  evaporate t)
  "Overlays placed on each clone, regardless of whether the 
cursor is on the cloned node.  Must be a plist of overlay properties.
By default, this only displays `org-clones-clone-prefix-icon'.")

(defvar org-clones--edit-mode-header-line
  '(:eval
    (format 
     "Edit cloned node. '%s' to finish and update. '%s' to abandon."
     org-clones-commit-edit-shortcut
     org-clones-abort-edit-shortcut))
  "The value of header-line-format when `org-clones-edit-mode' is 
invoked.")

(defvar org-clones--node-text-properties nil
  "Text properties to place on the headline and body of each node.
Note: If you want to chage the cursor-sensor-functions property, 
(perhaps for use with a different package) use 
`org-clones--put-clone-cursor-sensor-props'.  This variable is for other 
text properties (e.g., a face, keymap, etc.).

Note: 'face does not work with org-mode. Use 'font-lock-face.
      'keymap does not work with org-mode. Use a keymap in an
      overlay instead." )

(defvar org-clones--node-overlay-properties
  '(face org-clones-clone)
  "Overlay properties placed at the headline and body of each node.")

(defvar org-clones--previous-header-line nil
  "Temporary storage for the value of `header-line-format'.")
(make-variable-buffer-local 'org-clones--previous-header-line)

(defvar org-clones--transient-overlay nil
  "Temporary holder for the transient headline 
  or body overlay.")
(make-variable-buffer-local 'org-clones--transient-overlay)

(defvar org-clones--restore-state nil
  "When editing a clone, save the current headline and body
  to restore if the edit is abandoned.")
(make-variable-buffer-local 'org-clones--restore-state)

;;;; Macros

(defmacro org-clones--iterate-over-clones (&rest body)
  "Execute BODY at each clone of node at point."
  `(save-excursion
     (when-let ((clone-ids (org-clones--get-clone-ids)))
       (cl-loop for clone-id in clone-ids
		do (org-clones--with-point-at-id clone-id
		     ,@body)))))

(defmacro org-clones--iterate-over-all-clones-in-buffer (&rest body)
  "Execute BODY at any clone which has a non-nil :ORG-CLONES: property, 
in the buffer (but do not iterate over clones outside the buffer)."
  `(save-excursion
     (goto-char (point-min))
     (while (re-search-forward org-property-drawer-re nil t)
       (goto-char (match-beginning 0))
       (when (re-search-forward ":ORG-CLONES:" nil (match-end 0))
	 (when (org-entry-get (point) "ORG-CLONES")
	   ,@body)))))

(defmacro org-clones--with-point-at-id (id &rest body)
  "Switch to the buffer containing the entry with `org-id' ID.
Move the cursor to that entry in that buffer, execute BODY,
move back."
  (declare (indent defun))
  `(when-let ((marker (org-id-find ,id 'marker)))
     (with-current-buffer (marker-buffer marker)
       (goto-char marker)
       (org-show-entry)
       ,@body)))

;;;; Headline functions

(defun org-clones--goto-headline-start ()
  "Goto the first point of the headline, after the
leading stars."
  (org-back-to-heading t)
  (re-search-forward org-clones--org-headline-re (point-at-eol))
  (when-let ((todo (org-get-todo-state)))
    (re-search-forward todo (point-at-eol) t)
    (forward-char 1))
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

(defun org-clones--get-headline-string ()
  "Get the full text of a headline at point, excluding the
leading stars, TODO state, and tags."
  (save-excursion
    (org-back-to-heading)
    (org-no-properties
     (plist-get (cadr (org-element-at-point)) :raw-value))))

(defun org-clones--delete-headline ()
  "Delete the headline of the heading at point."
  (let ((inhibit-read-only t))
    (unless (string=
	     (plist-get (cadr (org-element-at-point)) :raw-value)
	     "")
      (delete-region (org-clones--get-headline-start)
		     (org-clones--get-headline-end)))))

(defun org-clones--replace-headline (headline)
  "Replace the headline text at point with HEADLINE."
  (let ((inhibit-read-only t))
    (save-excursion 
      (org-clones--delete-headline)
      (org-clones--goto-headline-start)
      (insert headline)
      (org--align-tags-here org-tags-column))))

;;;; Body functions 

(defun org-clones--node-body-p ()
  "Does this node have a body (i.e., a 'section' in org-element
parlance)?"
  (org-clones--parse-body))

(defun org-clones--goto-body-end ()
  "Goto the end of the body of the current node, 
and return the point. The end of the body is defined
as the last non-whitespace character before the next heading."
  (goto-char (org-entry-end-position))
  (re-search-backward org-clones--not-whitespace-re nil t)
  (goto-char (match-end 0))
  (point))

(defun org-clones--get-body-end ()
  "Get the end point of the body of the current node."
  (save-excursion (org-clones--goto-body-end)))

(defun org-clones--goto-body-start ()
  "Go to the start of the body of the current node,
and return the point. The start of the body is defined as
the point after the planning line, drawers immediately following
the planning line, and any closing note."
  (org-end-of-meta-data t)
  (let ((section (org-clones--get-section-elements)))
    ;; This is a ridiculous way to check if there is a closing note!
    (if (and (eq (caar section) 'plain-list)
	     (eq (car (caddar section)) 'item)
	     (eq (caaddr (caddar section)) 'paragraph)
	     ;; I don't think there is a variable to control
	     ;; the prefix of the closing note, so using "CLOSING NOTE "
	     ;; and assuming it is static.
	     (string= (caddr (caddr (caddar section))) "CLOSING NOTE "))
	(goto-char (plist-get (cadar section) :end)))
    (point)))

(defun org-clones--get-body-start ()
  "Get the start point of the body of the current node."
  (save-excursion (org-clones--goto-body-start)))

(defun org-clones--at-body-p ()
  "Is the point inside the body of a node?"
  (when-let ((start (org-clones--get-body-start))
	     (end (org-clones--get-body-end)))
    (and (<= (point) end)
	 (>= (point) start))))

(defun org-clones--normalize-node-format ()
  "Placeholder for normalizing the format of a node."
  nil)

(defun org-clones--replace-body (body)
  "Replace the body of the current node with
BODY. If BODY is nil, then use `org-clones-empty-body-string'."
  (let ((inhibit-read-only t))
    (org-back-to-heading)
    (save-excursion 
      (org-clones--delete-body))
    (org-clones--goto-body-start)
    (save-excursion 
      (insert (or body
		  org-clones-empty-body-string)
	      "\n"))
    (org-clones--normalize-node-format)))

(defun org-clones--parse-body ()
  "Parse all elements from the start of the body to the next node.
and return the tree beginning with the section element."
  (org-element--parse-elements (save-excursion (org-back-to-heading)
					       (org-end-of-meta-data t)
					       (point))
			       (or (save-excursion (outline-next-heading))
				   (point-max))
			       'first-section nil nil nil nil))

(defun org-clones--remove-closing-note-from-section-elements (section)
  "If there is a closing note in the section, remove it and return the section list."
  (if (and (eq (caar section) 'plain-list)
	   (eq (car (caddar section)) 'item)
	   (eq (caaddr (caddar section)) 'paragraph)
	   (string= (caddr (caddr (caddar section))) "CLOSING NOTE "))
      (cdr section)
    section))

(defun org-clones--get-body-string ()
  "Get the body of the current node as a string."
  (org-no-properties 
   (org-element-interpret-data
    (org-clones--remove-closing-note-from-section-elements
     (org-clones--get-section-elements)))))

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
  (let ((inhibit-read-only t))
    (delete-region (org-clones--get-body-start)
		   (save-excursion 
		     (outline-next-heading) (point)))))

(defun org-clones--fold-property-drawer (&optional unfold)
  "Fold the property drawer for the heading at point. If 
UNFOLD is non-nil, then unfold the drawer."
  (save-excursion
    (org-back-to-heading t)
    (when (re-search-forward
	   org-property-drawer-re
	   (save-excursion (or (outline-next-heading)
			       (point-max)))
	   'no-error)
      (org-flag-drawer (not unfold)))))

;;;; Clone functions

(defun org-clones--at-clone-p (&optional pos)
  "Is the current point (or POS) in a cloned node?"
  (org-entry-get (or pos (point)) "ORG-CLONES"))

(defun org-clones--get-clone-ids ()
  "Get the org-ids of this node's clones. Return
nil if there are none."
  (org-entry-get-multivalued-property
   (point)
   "ORG-CLONES"))

(defun org-clones--get-range-of-field-at-point ()
  "Return a cons cell with the car being the start point and
cdr being the end point of the of the headline or body of the node,
 depending on the location of the point."
  (cond ((org-clones--at-headline-p)
	 (cons (org-clones--get-headline-start)
	       (org-clones--get-headline-end)))
	((org-clones--at-body-p)
	 (cons (org-clones--get-body-start)
	       (org-clones--get-body-end)))))

(defun org-clones--last-node-p ()
  "Is this the last node in the file?"
  (not (or (save-excursion (org-get-next-sibling))
	   (save-excursion (org-goto-first-child)))))

(defun org-clones--prompt-for-source-node-and-move ()
  "Prompt user for a node and move to it."
  (org-goto))

(defun org-clones--sync-clones (&optional no-prompt)
  "Update all clones of the current node to match
the headline and body of the current node and
place text properties and overlays in the cloned nodes.
If NO-PROMPT is non-nil, do not prompt the user
regardless of the value of `org-clones-prompt-before-syncing'."
  (interactive)
  (if (and (not no-prompt)
	   org-clones-prompt-before-syncing)
      (org-clones--prompt-before-syncing)
    (org-clones--remove-clone-effects)
    (let ((inhibit-read-only t)
	  (headline (org-clones--get-headline-string))
	  (body (if (string= "" (org-clones--get-body-string))
		    org-clones-empty-body-string
		  (org-clones--get-body-string))))
      ;; Replace the body in the current node to 
      ;; normalize whitespace
      (org-clones--replace-body body)
      (org-clones--put-clone-effects)
      (org-clones--iterate-over-clones
       (org-clones--remove-clone-effects)
       (org-clones--replace-headline headline)
       (org-clones--replace-body body)
       (org-clones--put-clone-effects))))
  (when org-clones-edit-mode
    (org-clones-edit-mode -1))
  (org-clones--put-transient-overlay)
  (message "Clones synced."))

;;; Overlays

(defun org-clones--put-overlay-props (overlay props)
  "Add PROPS to OVERLAY and return OVERLAY."
  (cl-loop for x from 0 to (1- (length props)) by 2
	   do (overlay-put overlay
			   (nth x props)
			   (nth (1+ x) props))
	   finally return overlay))

(defun org-clones--put-node-overlay (&optional remove)
  "Put `org-clones--node-overlay-properties' in the headline and body of
the node at point. If REMOVE is non-nil, remove the overlays."
  (when org-clones--node-overlay-properties
    (if remove
	(progn 
	  (remove-overlays (org-clones--get-headline-start)
			   (org-clones--get-headline-end)
			   'org-clones-user-overlay t)
	  (remove-overlays (org-clones--get-body-start)
			   (org-clones--get-body-end)
			   'org-clones-user-overlay t))
      (let* ((headline-overlay (make-overlay (org-clones--get-headline-start)
					     (org-clones--get-headline-end)))
	     (body-overlay (make-overlay (org-clones--get-body-start)
					 (org-clones--get-body-end)))
	     (props (append org-clones--node-overlay-properties '(org-clones-user-overlay t))))
	(cons (org-clones--put-overlay-props headline-overlay props)
	      (org-clones--put-overlay-props body-overlay props))))))

(defun org-clones--remove-node-overlays ()
  "Remove `org-clones--node-overlay-properties' from the current node."
  (org-clones--put-node-overlay 'remove))

(defun org-clones--put-headline-overlay (&optional remove)
  "Put overlays in `org-clones--clone-headline-overlay-props' on the 
current node. If REMOVE is non-nil, remove the the overlay."
  (if remove
      (remove-overlays (org-clones--get-headline-start)
		       (org-clones--get-headline-end)
		       'org-clones-clone-headline-overlay t)
    (org-clones--remove-headline-overlay)
    (let ((headline-overlay (make-overlay (org-clones--get-headline-start)
					  (org-clones--get-headline-end)))
	  (props (append org-clones--clone-headline-overlay-props
			 '(org-clones-clone-headline-overlay t))))
      (org-clones--put-overlay-props headline-overlay props))))

(defun org-clones--remove-headline-overlay ()
  "Remove the overlays in `org-clones--clone-headline-overlay-props' from the 
current node."
  (org-clones--put-headline-overlay 'remove))

(defun org-clones--put-transient-overlay ()
  "Put the `org-clones--transient-overlay' in the cursor field
at point."
  (when-let* ((points (org-clones--get-range-of-field-at-point))
	      (beg (car points))
	      (end (cdr points)))
    (put-text-property beg end 'read-only t)
    (move-overlay org-clones--transient-overlay beg end)))

;;;; Text properties 

(defun org-clones--put-node-text-properties (&optional remove)
  "Put `org-clones--node-text-properties' in the headline and body of
the node at point. If REMOVE is non-nil, remove the properties."
  (when org-clones--node-text-properties
    (cl-loop for start in `(,(org-clones--get-headline-start)
			    ,(org-clones--get-body-start))
	     for end in `(,(org-clones--get-headline-end)
			  ,(org-clones--get-body-end))
	     do  (if remove 
		     (remove-list-of-text-properties
		      start
		      end
		      org-clones--node-text-properties)
		   (add-text-properties
		    start
		    end
		    org-clones--node-text-properties)))))

(defun org-clones--remove-node-text-properties ()
  "Remove `org-clones--node-text-properties' from the current node."
  (org-clones--put-node-text-properties 'remove))

(defun org-clones--put-clone-effects ()
  "Put overlay and text properties at the current
node. 'Clone effects' means: cursor sensor properties, 
text properties, headline overlay, node overlay, and 
automatically folding the property drawer."
  (let ((inhibit-read-only t))
    (org-clones--normalize-node-format)
    (org-clones--put-clone-cursor-sensor-props)
    (org-clones--put-headline-overlay)
    (org-clones--put-node-overlay)
    (org-clones--put-node-text-properties)
    (org-clones--fold-property-drawer)))

(defun org-clones--remove-clone-effects ()
  "Remove overlay and text properties at the current
node."
  (let ((inhibit-read-only t))
    (org-clones--remove-node-text-properties)
    (org-clones--remove-cursor-sensor-props)
    (org-clones--remove-node-overlays)
    (org-clones--remove-headline-overlay)))

(defun org-clones--reset-clone-effects ()
  "Remove and replace all clone effects for the current 
node."
  (let ((inhibit-read-only t))
    (org-clones--remove-clone-effects)
    (org-clones--put-clone-effects)))

;;;; Developement functions

;; (defun org-clones--remove-all-cursor-sensors-in-buffer ()
;;   "Remove all cursor sensor text properties in the buffer."
;;   (when (y-or-n-p
;; 	 (concat
;; 	  "This will remove all cursor-sensor-functions, even "
;; 	  "those that are not associated with org-clones. Continue?"))
;;     (let ((inhibit-read-only t))
;;       (set-text-properties
;;        (point-min)
;;        (point-max)
;;        '(cursor-sensor-functions nil)))))

;; (defun org-clones--reset-all-clone-effects-in-buffer ()
;;   "Reset all clone effets on all clones in buffer."
;;   (org-clones--iterate-over-all-clones-in-buffer
;;    (org-clones--remove-clone-effects)
;;    (org-clones--put-clone-effects)))

;; (defun org-clones--remove-all-clone-effects-in-buffer ()
;;   "Remove all clone effets on all clones in buffer."
;;   (org-clones--iterate-over-all-clones-in-buffer
;;    (org-clones--remove-clone-effects)))

;; (defun org-clones--highlight-cursor-sensor-props ()
;;   "Highlight any points in the buffer with a non-nil cursor-sensor-functions
;; text property."
;;   (save-excursion 
;;     (goto-char (point-min))
;;     (cl-loop for points being the intervals of (current-buffer)
;; 	     property 'cursor-sensor-functions
;; 	     do (when (get-text-property (car points)
;; 					 'cursor-sensor-functions)
;; 		  (ov 
;; 		   (car points)
;; 		   (cdr points)
;; 		   'font-lock-face
;; 		   '(:background "yellow" :foreground "black"))))))

;;;; Cursor-sensor-functions

(defun org-clones--change-cursor-sensor-prop (beg end func &optional remove)
  "Add FUNC to the list of cursor-sensor-functions from BEG to END. If REMOVE
is non-nil, then remove FUNC from the cursor-sensor-functions property. This 
preserves any other functions already stored in the cursor-sensor-fucntions 
text property, so it will not interfere with other modes using that text property."
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

(defun org-clones--put-clone-cursor-sensor-props (&optional remove)
  "Put `org-clones--cursor-sensor-functions' on the headline and body
of the current node. Note: these are treated differently than other
text properties because the value is a list and we need to preserve
and pre-existing values."
  (cl-loop for start in `(,(org-clones--get-headline-start)
			  ,(org-clones--get-body-start))
	   for end in `(,(org-clones--get-headline-end)
			,(org-clones--get-body-end))
	   do (cl-loop for func in org-clones--cursor-sensor-functions
		       do (org-clones--change-cursor-sensor-prop
			   start end func remove))))

(defun org-clones--remove-cursor-sensor-props ()
  "Remove `org-clones--cursor-sensor-functions' from the headline and 
body of the current node."
  (org-clones--put-clone-cursor-sensor-props 'remove))

(defun org-clones--text-watcher (_window last-pos entered-or-left)
  "If ENTERED-OR-LEFT is eq to 'enter, then place the transient 
overlay in the headline or body (as appropriate) and make the text
read only. If the value is 'left, then delete the transient overlay
and remove the read-only text property. See `cursor-sensor-mode' for
details on the arguments."
  (pcase entered-or-left
    (`entered
     (org-clones--put-transient-overlay)
     (message "Entered cloned node. Type '%s' to edit."
	      org-clones-start-edit-shortcut))
    (`left
     (when-let* ((points
		  (save-excursion (goto-char last-pos)
				  (org-clones--get-range-of-field-at-point)))
		 (beg (car points))
		 (end (cdr points)))
       (let ((inhibit-read-only t))
	 (put-text-property beg end 'read-only nil)
	 (delete-overlay org-clones--transient-overlay))))))

;;;; Editing clones

(defun org-clones--begin-edit ()
  "Invoke `org-clones-edit-mode'."
  (interactive)
  (org-clones-edit-mode 1))

(defun org-clones--prompt-before-syncing ()
  "Ask the user if they want to edit the node
without syncing the clones. If so, unlink the current 
clone."
  (interactive)
  ;; Without this let, y-or-n-p pops a dialog box
  ;; by default due to something involving `cursor-sensor-mode'
  (let ((last-nonmenu-event
	 (not org-clones-use-popup-prompt)))
    (if (y-or-n-p "Sync your changes to all clones?")
	(org-clones--sync-clones t)
      (if (y-or-n-p "Unsync this clone?")
	  (org-clones-unclone-this-clone)
	(if (y-or-n-p "Discard this edit?")
	    (org-clones--discard-edit)
	  ;; If the user takes an impossible path,
	  ;; send them back to the beginning.
	  (org-clones--prompt-before-syncing)))))
  (org-clones-edit-mode -1))

(defun org-clones--discard-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (interactive)
  (org-clones--replace-headline
   (car org-clones--restore-state))
  (org-clones--replace-body
   (cdr org-clones--restore-state))
  (org-clones--put-clone-effects)
  (org-clones-edit-mode -1)
  (message "Org-clones: Discarded edit."))

(define-minor-mode org-clones-edit-mode
  "Mode to edit clones."
  nil
  " EDIT-CLONE"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd org-clones-commit-edit-shortcut)
      #'org-clones--sync-clones)
    (define-key map (kbd org-clones-abort-edit-shortcut)
      #'org-clones--discard-edit)
    map)
  (if org-clones-edit-mode
      (progn
	(setq cursor-sensor-inhibit nil)
	(setq org-clones--previous-header-line header-line-format)
	(setq header-line-format org-clones--edit-mode-header-line)
	(setq org-clones--restore-state
	      (cons (org-clones--get-headline-string)
		    (org-clones--get-body-string)))
	(overlay-put org-clones--transient-overlay 'keymap nil)
	(let ((inhibit-read-only t)
	      (bounds (org-clones--get-range-of-field-at-point)))
	  (put-text-property (car bounds) (cdr bounds) 'read-only nil)))
    (setq header-line-format org-clones--previous-header-line)
    (when (equal header-line-format
		 org-clones--previous-header-line)
      (setq header-line-format nil))
    (overlay-put org-clones--transient-overlay
		 'keymap
		 org-clones--transient-clone-overlay-map)
    (setq org-clones--restore-state nil)))

;;;; Initialization 

(defun org-clones--initialize-transient-overlay ()
  "Initialize `org-clones--transient-overlay'
used to highlight the clone at point. This overlay is reused
each time the point is in the headline or body of a cloned node."
  (setq org-clones--transient-overlay
	(make-overlay 1 2 nil nil t))
  (org-clones--put-overlay-props
   org-clones--transient-overlay
   org-clones--transient-overlay-properties)
  (delete-overlay org-clones--transient-overlay))

(defun org-clones--initialize-overlays-in-buffer ()
  "Put overlays on all clones in current buffer."
  (org-clones--iterate-over-all-clones-in-buffer 
   (org-clones--put-headline-overlay)))

(defun org-clones--cursor-sensor-mode-check ()
  "Turn `cursor-sensor-mode' on or off depending on 
whether there are any cursor-sensor-functions text
 properties in the buffer." 
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

(defun org-clones--org-id-precheck ()
  "Org-id needs a file to be saved and sometimes needs
to run `org-id-update-id-locations' for it to work property.
This forces the user to save the buffer, and makes sure
`org-id' can find the file and id."
  (if (buffer-file-name)
      ;; (unless (member (buffer-file-name)
      ;; 		  (hash-table-values org-id-locations))
      ;; TODO: I don't think this fixes the problem.  It seems
      ;; like a bug in `org-id'. 
      (org-id-update-id-locations (list (buffer-file-name)) 'silent)
    (error "You must save this file before creating a clone")))

;;;; Commands

(defun org-clones-jump-to-clones ()
  "Allow the user to cycle through any clones 
of the node at point by pressing `org-clones-jump-to-next-clone-shortcut'."
  (interactive)
  (when (org-clones--at-clone-p)
    (unless org-clones--clone-cycle-list
      (setq org-clones--clone-cycle-list
	    (append (org-clones--get-clone-ids)
		    (list (org-id-get)))))
    (when org-clones--clone-cycle-list
      (set-transient-map
       org-clones--clone-cycle-map t
       ;; Cleanup function for the transient keymap
       (lambda ()
	 (setq org-clones--clone-cycle-list nil)
	 (cl-loop for (buffer . header) in
		  org-clones--clone-cycle-buffer-list
		  do (with-current-buffer buffer
		       (setq header-line-format header)))
	 (setq org-clones--clone-cycle-buffer-list nil)))
      (let ((last-pop (pop org-clones--clone-cycle-list)))
	(org-id-goto last-pop)
	(cl-pushnew (cons (buffer-name)
			  header-line-format)
		    org-clones--clone-cycle-buffer-list
		    :test 'equal)
	(setq header-line-format
	      org-clones--clone-cycle-header-msg)
	(setq org-clones--clone-cycle-list
	      (append org-clones--clone-cycle-list
		      (list last-pop)))))))

;;;###autoload
(define-minor-mode org-clones-mode
  "Org heading transclusion minor mode."
  nil
  " ORG-CLONES"
  (make-sparse-keymap)
  (if org-clones-mode
      (progn
	(org-clones--initialize-transient-overlay)
	(org-clones--reset-all-clone-effects-in-buffer)
	(org-clones--initialize-overlays-in-buffer)
	(org-clones--cursor-sensor-mode-check))
    (org-clones--remove-all-clone-effects-in-buffer)
    (org-clones--cursor-sensor-mode-check)))

;;;###autoload 
(defun org-clones-delete-this-clone ()
  "Delete the clone at point and remove references to it from
other cloned nodes."
  (interactive)
  (org-back-to-heading)
  (org-clones-unclone-this-clone)
  (let* ((element-plist (cadr (org-element-at-point)))
	 (beg (plist-get element-plist :begin))
	 (end (plist-get element-plist :end)))
    (delete-region beg end)))

(defun org-clones-unclone-this-clone ()
  "Remove the org-clones property from this node, and remove this
node's id from any nodes which contain it."
  (interactive)
  (let ((this-id (org-id-get))
	(clone-ids (org-clones--get-clone-ids)))
    (save-excursion 
      (cl-loop for clone-id in clone-ids
	       do (org-clones--with-point-at-id clone-id
		    (org-entry-remove-from-multivalued-property
		     (point) "ORG-CLONES" this-id)
		    (unless (org-clones--get-clone-ids)
		      (org-clones--remove-clone-effects)))))
    (org-clones--remove-clone-effects)
    (org-set-property "ORG-CLONES" "nil")
    (message "This node is no longer synced with other clones.")))

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
(defun org-clones-create-clone-dwim ()
  "Create a clone from the stored marker if it exists;
otherwise, prompt for the source node."
  (org-clones-create-clone org-clones--temp-marker)
  (when org-clones--temp-marker
    (setq org-clones--temp-marker nil)))

;;;###autoload
(defun org-clones-create-clone (&optional source-marker)
  "Insert a new headline, prompt the user for the source node,
add clone properties to the source, add clone properties to the clone
and add the headline and body from the source to the clone.  
SOURCE-POINT is a marker for the location of the source node"
  (interactive)
  (unless org-clones-mode
    (org-clones-mode 1))
  (unless cursor-sensor-mode
    (cursor-sensor-mode 1))
  (let (source-headline source-body source-id source-clone-list	clone-id)
    ;; At the new heading...
    (org-insert-heading-respect-content)
    (org-clones--org-id-precheck) 
    (setq clone-id (org-id-get-create))
    
    ;; At the source heading...
    (cl-flet ((source-node-prep
	       nil
	       (org-clones--org-id-precheck)
	       (org-back-to-heading)
	       (org-clones--remove-clone-effects)
	       (setq source-headline (org-clones--get-headline-string))
	       (setq source-body (org-clones--get-body-string))
	       (when (string= "" source-body)
		 (org-clones--replace-body nil)
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

;;;; Footer

(provide 'org-clones)

;;; org-clones.el ends here
