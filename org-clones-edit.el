;;; org-clones.el --- Clone org headings -*- lexical-binding: t; -*-

(require 'org-clones)

(defvar org-clones--current-state-of-node nil
  "When editing a clone, save the current headline and body
to restore if the edit is abandoned.")

(defvar org-clones--previous-header-line header-line-format
  "Holds the previous `header-line-format' value to restore later.")

(defun org-clones--discard-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (interactive)
  (org-clones--replace-headline
   (car org-clones--current-state-of-node))
  (org-clones--replace-node-body
   (cdr org-clones--current-state-of-node))
  (org-clones--put-clone-props)
  (org-clones-edit-mode -1))

(defun org-clones--complete-edit ()
  "Sync all clones with the current state of the 
node being edited."
  (interactive)
  (org-clones--update-clones)
  (org-clones-edit-mode -1))

(define-minor-mode org-clones-edit-mode
  "Mode to edit clones."
  nil
  " EDIT-CLONE"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-clones--complete-edit)
    (define-key map (kbd "C-c C-k") #'org-clones--discard-edit)
    map)
  (if org-clones-edit-mode
      (progn
	(setq org-clones--current-state-of-node
	      (cons (org-clones--get-headline)
		    (org-clones--get-node-body)))
	(org-clones--remove-clone-props)
	(setq org-clones--previous-header-line header-line-format)
	(setq header-line-format
	      (concat
	       "Edit cloned node. 'C-c C-c' to finish and update."
	       "'C-c C-k' to abandon.")))
    (setq org-clones--current-state-of-node nil)
    (setq header-line-format
	  org-clones--previous-header-line)))


