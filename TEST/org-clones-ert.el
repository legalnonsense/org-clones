;;; org-clones-ert.el --- Testing  -*- lexical-binding: t; -*-

(defun org-clones-ert--replace-buffer-text (text)
  (erase-buffer)
  (insert text)
  (goto-char (point-min)))

(setq org-clones-ert--body-test-text 
      "* done [#A] COMMENT Parent node [0%] [0/0] :tags:
SCHEDULED: <2020-09-04 Fri> DEADLINE: <2020-09-04 Fri>
:PROPERTIES:
:ID:       b2b87a7a-a80e-4ca7-bbc8-5de56b7a1729
:ORG-CLONES: 3c4195b4-8762-48ff-8dd7-adb6988d4020
:END:
- CLOSING NOTE [2020-09-04 Fri 10:23] \\
  asdfasdfasdf
akkdsf

** done [#B] Parent node :more:tags:
:PROPERTIES:
:ID:       3c4195b4-8762-48ff-8dd7-adb6988d4020
:ORG-CLONES: b2b87a7a-a80e-4ca7-bbc8-5de56b7a1729
:END:
akkdsf

** another headline ")

(ert-deftest org-clones--body-functions-test ()
  (with-current-buffer (get-buffer-create "org-clones-ert-tests.org")
    (org-mode)
    (org-clones-ert--replace-buffer-text
     "* done [#A] COMMENT Parent node [0%] [0/0] :tags:
SCHEDULED: <2020-09-04 Fri> DEADLINE: <2020-09-04 Fri>
:PROPERTIES:
:ID:       b2b87a7a-a80e-4ca7-bbc8-5de56b7a1729
:ORG-CLONES: 3c4195b4-8762-48ff-8dd7-adb6988d4020
:END:
- CLOSING NOTE [2020-09-04 Fri 10:23] \\
  asdfasdfasdf
akkdsf

** done [#B] Parent node :more:tags:
:PROPERTIES:
:ID:       3c4195b4-8762-48ff-8dd7-adb6988d4020
:ORG-CLONES: b2b87a7a-a80e-4ca7-bbc8-5de56b7a1729
:END:
akkdsf

** another headline ")
    (org-clones--replace-body "12345")
    (should (string= (org-clones--get-body-string)
		     "12345\n"))
    (outline-next-heading)
    (should (string= (org-clones--get-body-string)
		     "akkdsf\n"))
    (outline-next-heading)
    (should (string= (org-clones--get-body-string)
		     ""))
    (goto-char (point-min))
    (org-add-note)
     

     (ert 'org-clones--body-functions-test)

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

    (org-clones--replace-headline "some new text [4/5]")
    (org-clones--normalize-headline)
    (should (string= (buffer-substring-no-properties (point-at-bol) (point-at-eol))
		     "* TODO some new text [4/5] [0/0] :tags1:tags2:"))
    
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

    (org-clones-ert--replace-buffer-text "* TODO [#C] COMMENT test src_elisp{(org-entry-get (point) \"ASD\")} [1/2] {{{results(=123=)}}}\n:PROPERTIES:\n\n:ASD:      123\n:ID:       c39ce8ba-d96b-49eb-9642-20e2ee9ad045\n:END:\n")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string) "test src_elisp{(org-entry-get (point) \"ASD\")}"))

    (org-clones-ert--replace-buffer-text "* TODO COMMENT test src_elisp{(org-entry-get (point) \"ASD\")} [#C] [1/2] {{{results(=123=)}}} :tags:more_tags:\n:PROPERTIES:\n\n:ASD:      123\n:ID:       c39ce8ba-d96b-49eb-9642-20e2ee9ad045\n:END:\n")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string) "test src_elisp{(org-entry-get (point) \"ASD\")}"))
    
    (org-clones-ert--replace-buffer-text "*** COMMENT [55%] Parent: [4/5] node :tags:")
    (org-clones--normalize-headline)
    (should (string= (org-clones--get-headline-string)
		     "Parent: node"))
    (org-clones--replace-headline "PARENT NODE!")
    (should (string= (buffer-substring-no-properties (point-at-bol) (point-at-eol))
		     "*** COMMENT PARENT NODE! [55%] [4/5] :tags:"))

    (org-clones--replace-headline "PARENT COMMENT !")
    (should (string= (buffer-substring-no-properties (point-at-bol) (point-at-eol))
		     "*** COMMENT PARENT COMMENT ! [55%] [4/5] :tags:"))
    
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

