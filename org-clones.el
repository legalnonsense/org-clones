;; org-clones.el --- Clone org headings -*- lexical-binding: t; -*-

;; "Node" means an entry in an org outline
;; "Body" means everything after the headline, planning line,
;;        and property drawers until the next node.
;;        It does not include whitespace between the text and
;;        next node. 


(require 'org)
(require 'org-id)
(require 'ov)

(defface org-clones-clone
  '((t (:background "black")))
  "Body of cloned nodes.")

(face-spec-set 'org-clones-clone '((t (:background "black"))))

(defvar org-clones--headline-re outline-regexp
  "Org headline regexp.")

(defcustom org-clones-clone-logbook nil
  "Clone the logbook of the heading.")

(defcustom org-clones-clone-drawers nil
  "Clone the drawers of the heading.")

(defmacro org-clones--with-point-at-id (id &rest body)
  "Switch to the buffer containing the entry with id ID.
  Move the cursor to that entry in that buffer, and execute BODY."
  (declare (indent defun))
  `(--when-let (org-id-find ,id 'marker)
     (save-excursion 
       (with-current-buffer (marker-buffer it)
	 (goto-char it)
	 ,@body))))

(defun org-clones--goto-body-start ()
  "Goto the start of the body of the current node,
and return the point."
  (org-end-of-meta-data t)
  (point))

(defun org-clones--get-body-start ()
  "Get the start point of the body of the current node."
  (save-excursion (org-clones--goto-body-start)))

(defun org-clones--goto-body-end ()
  "Goto the end of the body of the current node, 
and return the point." 
  (org-back-to-heading)
  (end-of-line)
  (if (re-search-forward org-clones--headline-re nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max)))
  (re-search-backward "[^[:space:]]")
  (end-of-line)
  (forward-char 1)
  (point))

(defun org-clones--goto-headline-end ()
  "Goto the last point of the headline (i.e., before the
leading stars."
  (org-back-to-heading t)
  (end-of-line)
  (point))

(defun org-clones--get-headline-end ()
  "Get the point at the end of the headline, but
before the ellipsis."
  (save-excursion 
    (org-clones--goto-headline-end)))

(defun org-clones--goto-headline-start ()
  "Goto the first point of the headline, after the
leading stars."
  (org-back-to-heading t)
  (re-search-forward org-clones--headline-re nil (point-at-eol))
  (point))

(defun org-clones--get-headline-start ()
  "Get the point at the start of the headling, after
the leading stars."
  (save-excursion
    (org-clones--goto-headline-start)))

(defun org-clones--delete-headline ()
  "Replace the headline of the heading at point." 
  (delete-region (org-clones--get-headline-start)
		 (org-clones--get-headline-end)))

(defun org-clones--get-headline ()
  "Get the full text of a headline at point, including
TODO state, headline text, and tags." 
  (buffer-substring (org-clones--get-headline-start)
		    (org-clones--get-headline-end)))

(defun org-clones--replace-headline (headline)
          "Replace the headline text at point with HEADLINE."
          (save-excursion 
            (org-clones--delete-headline)
            (org-clones--goto-headline-start)
            (insert headline)))

(defun org-clones--get-body-end ()
            "Get the end point of the body of the current node."
            (save-excursion (org-clones--goto-body-end)))

(defun org-clones--node-body-p ()
      "Does this node have a body?"
      (/= (org-clones--get-body-start)
      (org-clones--get-body-end)))

(defun org-clones--get-node-body ()
  "Get the body of the current node as a string."
  (buffer-substring
   (org-clones--get-body-start)
   (org-clones--get-body-end)))

(defun org-clones--delete-node-body ()
  "Delete the body of the current node."
  (when (org-clones--node-body-p)
    (org-back-to-heading)
    (save-excursion
      (delete-region (org-clones--get-body-start)
		     (org-clones--get-body-end)))))

(defun org-clones--replace-node-body (body)
  "Replace the body of the current node with BODY."
  (save-excursion 
    (org-clones--delete-node-body)
    (goto-char (org-clones--goto-body-end))
    (insert body
	    "\n")))

(defun org-clones--replace-node-body-by-id (id body)
  "Goto ID and replace its body with BODY."
  (save-excursion
    (org-clones--with-point-at-id id
      (org-clones--replace-node-body body))))

(defun org-clones--add-unique-property (prop val)
  "Add VAL to PROP if it is not already present, retaining
  all existing VALS."
  (org-entry-add-to-multivalued-property
   (point)
   "ORG-CLONES"
   val))

(defun org-clones--prompt-for-source-and-move ()
  "Prompt user for the source node and move to it."
  (org-goto))

(defun org-clones-create-clone (&optional id)
  "Insert a new headline, prompt the user for the source node,
add clone properties to the source, add clone properties to the clone
and add the headline and body from the source to the clone."
  (interactive)
  (let ((source-headline nil)
	(source-body nil)
	(source-id nil)
	(source-clone-list nil)
	(clone-id nil))

    ;; Create the new heading, save the ID
    (org-insert-heading-respect-content)
    (setq clone-id (org-id-get-create))
    
    ;; At the source node...
    (save-excursion 
      (org-clones--prompt-for-source-and-move)
      (setq source-headline (org-clones--get-headline))
      (setq source-body (org-clones--get-node-body))
      (setq source-id (org-id-get-create))
      (org-entry-add-to-multivalued-property (point)
					     "CLONED-WITH"
					     clone-id)
      (setq source-clone-list (org-entry-get-multivalued-property
			       (point)
			       "CLONED-WITH"))
      (org-clones--put-clone-effects))

    ;; For each clone from the source, add new clone id
    (cl-loop for clone-id in source-clone-list
	     do (org-clones--with-point-at-id clone-id
		  (cl-loop for clone in source-clone-list
			   do
			   (unless (string= clone (org-id-get-create))
			     (org-entry-add-to-multivalued-property (point)
								    "CLONED-WITH"
								    clone)))))
    
    ;; At the new clone...
    (org-entry-add-to-multivalued-property (point)
					   "CLONED-WITH"
					   source-id)
    (org-clones--replace-headline source-headline)
    (org-clones--replace-node-body source-body)
    (org-clones--put-clone-effects)))

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

(defun org-clones--remove-text-properties ()
      "Remove read-only text properties for the current node."
      (let ((inhibit-read-only t))
    (remove-text-properties (org-clones--get-headline-start)
			    (org-clones--get-headline-end)
			    '(read-only t org-clones t))

    (remove-text-properties (org-clones--get-body-start)
			    (org-clones--get-body-end)
			    '(read-only t org-clones t))))

(defvar org-clones-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-clones--edit-clone)
    map)
  "Keymap for overlays put on clones.")
	  
(defun org-clones--put-overlays ()
  "Put the clone overlay at the headline and body
of the current node."
  (ov (org-clones--get-headline-start)
      (org-clones--get-headline-end)
      'face 'org-clones-clone
      'keymap org-clones-overlay-map)
  
  (ov (org-clones--get-body-start)
      (org-clones--get-body-end)
      'face 'org-clones-clone
      'keymap org-clones-overlay-map))

(defun org-clones--remove-overlays ()
  "Remove the clone overlay at the headline and body
of the current node."
  (ov-clear (org-clones--get-headline-start)
	    (org-clones--get-headline-end)
	    'face 'org-clones-clone)
  (ov-clear (org-clones--get-body-start)
	    (org-clones--get-body-end)
	    'face 'org-clones-clone))

(defun org-clones--put-clone-effects ()
  "Put overlay and text properties at the current
node."
  (org-clones--put-text-properties)
  (org-clones--put-overlays))

(defun org-clones--remove-clone-effects ()
  "Remove overlay and text properties at the current
node."
  (org-clones--remove-text-properties)
  (org-clones--remove-overlays))

(defun org-clones--put-all-clone-effects ()
  "Clear all overlays and text properties that might have been set 
previously. Place a new set of overlays and text properties at each
node with a CLONED-WITH property."
  (org-ql-select (current-buffer)
    '(property "CLONED-WITH")
    :action (lambda ()
	      (org-clones--iterate-over-clones
	       (org-clones--put-clone-effects)))))

(defun org-clones--remove-all-clone-effects ()
  "Remove clone effects from all clones."
  (let ((inhibit-read-only t))
    (org-ql-select (current-buffer)
      '(property "CLONED-WITH")
      :action (lambda ()
		(org-clones--iterate-over-clones
		 (org-clones--remove-clone-effects))))))

(defun org-clones--update-clones ()
  "Update all clones of the current node to match
the headline and body of the current node, then
re-place text properties and overlays in the cloned nodes."
  (interactive)
  (org-clones--remove-clone-effects)
  (let ((headline (org-clones--get-headline))
	(body (org-clones--get-node-body)))
    (org-clones--put-clone-effects)
    (org-clones--iterate-over-clones
     (org-clones--remove-clone-effects)
     (org-clones--replace-headline headline)
     (org-clones--replace-node-body body)
     (org-clones--put-clone-effects))))

(defmacro org-clones--iterate-over-clones (&rest body)
  "Execute BODY at each clone, excluding the present node."
  `(save-excursion
     (when-let ((clone-ids (org-entry-get-multivalued-property
			    (point)
			    "CLONED-WITH")))
       (cl-loop for clone-id in clone-ids
		do (org-clones--with-point-at-id
		    clone-id
		    ,@body)))))

(defun org-clones--edit-clone ()
  "Start edit mode."
  (interactive)
  (org-clones-edit-mode 1))

;; ;;;###autoload
;; (define-minor-mode org-clones-mode
;;   "Org heading transclusion minor mode."
;;   nil
;;   " ORG-CLONES"
;;   nil
;;   (if org-clones-mode) nil nil
;;   ;; refresh overlays
;;   ;; delete overlays and text properties
;;   ;; check for conflicts and resolve them
;;   )


(provide 'org-clones)



