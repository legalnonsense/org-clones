;;; org-clones-ert.el --- Testing  -*- lexical-binding: t; -*-

(defun org-clones-ert--replace-buffer-text (text)
  (erase-buffer)
  (insert text)
  (goto-char (point-min)))

(ert-deftest org-clones--headline-functions-test ()
  (with-current-buffer (get-buffer-create "org-clones-ert-tests.org")
    (org-mode)
    ;; Node with children
    (org-clones-ert--replace-buffer-text "* Parent node\n** Child node1\n** Child node2")
    (should (string= (org-clones--get-headline-string) "Parent node"))
    (outline-next-heading)
    (should (string= (org-clones--get-headline-string) "Child node1"))

    ;; Node with TODO, progress cookie, and tags
    (org-clones-ert--replace-buffer-text "* TODO Parent [0/0] node :tags1:tags2:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string) "Parent node"))

    ;; COMMENT, progress cookie, tags, and stray colons
    (org-clones-ert--replace-buffer-text "*** COMMENT [55%] Parent: node :tags:")
    ;;(org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string) " "))

    (org-clones-ert--replace-buffer-text "*** COMMENT [55%] Parent :node :tags:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string) "Parent :node"))

    ;; Multiple progress cookies
    (org-clones-ert--replace-buffer-text "*** COMMENT [55%] Parent: [4/5] node :tags:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string)
		     "Parent: node"))

    (org-clones-ert--replace-buffer-text "* test src_elisp{(org-entry-get (point) \"ASD\")} {{{results(=123=)}}}\n:PROPERTIES:\n\n:ASD:      123\n:ID:       c39ce8ba-d96b-49eb-9642-20e2ee9ad045\n:END:\n")
    (should (string= (org-clones--get-headline-string) "test src_elisp{(org-entry-get (point) \"ASD\")}"))

    (org-clones-ert--replace-buffer-text "*** COMMENT [55%] Parent: [4/5] node :tags:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string)
		     "Parent: node"))

    (org-clones-ert--replace-buffer-text "* TODO [#A] COMMENT Parent [50%] node  :tags2:tags3:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string)
		     "Parent node"))

    (org-clones-ert--replace-buffer-text "* TODO [#A] Parent COMMENT [50%] node  :tags2:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string)
		     "Parent COMMENT node"))

    (org-clones-ert--replace-buffer-text "*** [#A] COMMENT [55%] Parent: [4/5] node :tags:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string)
		     "Parent: node"))))


(ert 'org-clones--headline-functions-test)

