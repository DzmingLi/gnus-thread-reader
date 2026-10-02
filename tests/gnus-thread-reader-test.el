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

;;; gnus-thread-reader-test.el ends here
