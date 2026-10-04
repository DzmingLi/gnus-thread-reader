;;; gnus-thread-reader-test.el --- Reader mark behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
;; Load source so the Gnus mark accessors remain replaceable in this test.
(load (expand-file-name "../gnus-thread-reader.el"
                        (file-name-directory load-file-name)) nil t)

(ert-deftest gnus-thread-reader-marks-visited-loaded-articles-only ()
  (let ((summary (generate-new-buffer " *gnus-summary-test*"))
        (reader (generate-new-buffer " *gnus-reader-test*"))
        (marks (make-hash-table :test #'eql))
        (marked nil))
    (unwind-protect
        (with-current-buffer reader
          (gnus-thread-reader-mode)
          (let ((inhibit-read-only t))
            (insert "root\nreply\n")
            (put-text-property 1 5 'thread-reader-id "1")
            (put-text-property 6 11 'thread-reader-id "2"))
          (setq gnus-thread-reader--summary summary)
          (dolist (number '(1 2))
            (puthash (number-to-string number)
                     (make-full-mail-header
                      number "Subject" "Author" "" (format "<%d@test>" number)
                      "" 0 0 "" nil)
                     gnus-thread-reader--headers)
            (puthash number gnus-unread-mark marks))
          (puthash "1" '(plain . "root") gnus-thread-reader--bodies)
          (cl-letf (((symbol-function 'gnus-thread-reader--source)
                     (lambda () summary))
                    ((symbol-function 'gnus-thread-reader--checked-header)
                     (lambda (&rest _) t))
                    ((symbol-function 'gnus-summary-article-mark)
                     (lambda (number) (gethash number marks)))
                    ((symbol-function 'gnus-summary-mark-article)
                     (lambda (number mark)
                       (push number marked)
                       (puthash number mark marks)))
                    ((symbol-function 'gnus-set-mode-line)
                     (lambda (&rest _) nil)))
            (goto-char 1)
            (should (gnus-thread-reader--mark-current-read))
            (should (equal marked '(1)))
            ;; A manual unread mark persists until this article is left.
            (puthash 1 gnus-unread-mark marks)
            (should-not (gnus-thread-reader--mark-current-read))
            (goto-char 6)
            (should-not (gnus-thread-reader--mark-current-read))
            (puthash "2" '(plain . "reply") gnus-thread-reader--bodies)
            (should (gnus-thread-reader--mark-current-read))
            (should (equal marked '(2 1)))
            (goto-char 1)
            (should (gnus-thread-reader--mark-current-read))
            (should (equal marked '(1 2 1)))))
      (kill-buffer reader)
      (kill-buffer summary))))

(ert-deftest gnus-thread-reader-summary-policy-survives-group-parameters ()
  (with-temp-buffer
    (let ((gnus-newsgroup-threads nil))
      (setq-local gnus-show-threads nil)
      (setq-local gnus-thread-hide-subtree nil)
      (gnus-thread-reader-summary-mode 1)
      ;; Group parameters are applied after mode hooks on group entry.
      (setq-local gnus-show-threads nil)
      (setq-local gnus-thread-hide-subtree nil)
      (run-hooks 'gnus-summary-generate-hook)
      (should gnus-show-threads)
      (should gnus-thread-hide-subtree)
      (should (eq (lookup-key gnus-thread-reader-summary-mode-map
                              (kbd "RET"))
                  #'gnus-thread-reader-open))
      (gnus-thread-reader-summary-mode -1)
      (should-not gnus-show-threads)
      (should-not gnus-thread-hide-subtree)
      (should-not (memq #'gnus-thread-reader--summary-policy
                       gnus-summary-generate-hook)))))

(ert-deftest gnus-thread-reader-summary-enable-folds-existing-summary ()
  (with-temp-buffer
    (let ((gnus-newsgroup-threads '(existing-thread))
          folded)
      (cl-letf (((symbol-function 'gnus-summary-hide-all-threads)
                 (lambda (&optional predicate)
                   (should-not predicate)
                   (setq folded t))))
        (gnus-thread-reader-summary-mode 1)
        (should folded)
        ;; Re-enabling must not overwrite the saved native settings.
        (gnus-thread-reader-summary-mode 1)
        (gnus-thread-reader-summary-mode -1)
        (should-not (local-variable-p 'gnus-thread-hide-subtree))))))

(ert-deftest gnus-thread-reader-fetch-preserves-decoded-unicode ()
  (let ((header (make-full-mail-header 1 "Title" "Author" "" "<1@test>"
                                       "" 0 0 "" nil))
        (text "中文與數學，café"))
    (cl-letf (((symbol-function 'gnus-thread-reader--source)
               (lambda () (current-buffer)))
              ((symbol-function 'gnus-thread-reader--checked-header)
               (lambda (&rest _) t))
              ((symbol-function 'gnus-request-article-this-buffer)
               (lambda (&rest _)
                 ;; Backends such as nnrss write already decoded text.
                 (insert "Content-Type: text/plain; charset=gnus-decoded\n"
                         "Content-Transfer-Encoding: 8bit\n\n" text)
                 t)))
      (should (equal (gnus-thread-reader--fetch header) (cons 'plain text))))))

(ert-deftest gnus-thread-reader-fetch-decodes-wire-utf8 ()
  (let ((header (make-full-mail-header 1 "Title" "Author" "" "<1@test>"
                                       "" 0 0 "" nil))
        (text "<p>中文與數學，café</p>"))
    (cl-letf (((symbol-function 'gnus-thread-reader--source)
               (lambda () (current-buffer)))
              ((symbol-function 'gnus-thread-reader--checked-header)
               (lambda (&rest _) t))
              ((symbol-function 'gnus-request-article-this-buffer)
               (lambda (&rest _)
                 (insert "Content-Type: text/html; charset=utf-8\n"
                         "Content-Transfer-Encoding: base64\n\n"
                         (base64-encode-string (encode-coding-string text 'utf-8)))
                 t)))
      (should (equal (gnus-thread-reader--fetch header) (cons 'html text))))))

;;; gnus-thread-reader-test.el ends here
