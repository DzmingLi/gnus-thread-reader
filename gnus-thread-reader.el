;;; gnus-thread-reader.el --- Continuous Gnus conversation view -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news, mail

;;; Commentary:

;; Run `gnus-thread-reader-open' in a Gnus summary to read its current
;; conversation as a continuous Outline tree.  Gnus owns all article marks
;; and reply composition.  Visiting a loaded article marks that article read;
;; background loading does not mark other articles read.
;; Only the thread headers already known to the summary are included.

;;; Code:

(require 'thread-reader)
(require 'gnus-sum)
(require 'gnus-art)
(require 'gnus-msg)
(require 'gnus-cache)
(require 'mm-decode)
(require 'mm-util)
(require 'mail-parse)

(defvar gnus-thread-reader-summary-mode)
(defvar-local gnus-thread-reader--summary nil)
(defvar-local gnus-thread-reader--group nil)
(defvar-local gnus-thread-reader--anchor nil)
(defvar-local gnus-thread-reader--anchor-id nil)
(defvar-local gnus-thread-reader--headers nil)
(defvar-local gnus-thread-reader--bodies nil)
(defvar-local gnus-thread-reader--queue nil)
(defvar-local gnus-thread-reader--timer nil)
(defvar-local gnus-thread-reader--generation 0)
(defvar-local gnus-thread-reader--last-visited-id nil)
(defvar-local gnus-thread-reader-focus-ids nil
  "Optional list of article IDs to visit with `N', such as notification targets.")

(defun gnus-thread-reader--source ()
  "Return this view's live summary, checking its group identity."
  (let ((summary gnus-thread-reader--summary)
        (group gnus-thread-reader--group))
    (unless (and (buffer-live-p summary)
                 (with-current-buffer summary
                   (and (derived-mode-p 'gnus-summary-mode)
                        (equal group gnus-newsgroup-name))))
      (user-error "The source Gnus summary has closed; reopen the thread in Gnus"))
    summary))

(defun gnus-thread-reader--checked-header (number expected-id)
  "Check NUMBER still denotes EXPECTED-ID in the source summary."
  (with-current-buffer (gnus-thread-reader--source)
    (let ((header (gnus-summary-article-header number)))
      (unless (and (mail-header-p header)
                   (equal expected-id (mail-header-id header)))
        (user-error "Article identity changed in Gnus; reopen the thread"))
      header)))

(defun gnus-thread-reader--flatten (tree)
  "Return (HEADER PARENT-NUMBER) records from Gnus TREE in display order.
Subject-only synthetic roots are omitted.  Sparse headers remain placeholders.
An explicit stack and identity set also tolerate malformed cyclic trees."
  (let ((stack (list (list tree nil)))
        (seen (make-hash-table :test #'eq)) records)
    (while stack
      (pcase-let ((`(,node ,parent) (pop stack)))
        (when (and (consp node) (not (gethash node seen)))
          (puthash node t seen)
          (let ((header (car node)))
            (when (mail-header-p header)
              (push (list header parent) records)
              (setq parent (mail-header-number header)))
            (dolist (child (reverse (cdr node)))
              (push (list child parent) stack))))))
    (nreverse records)))

(defun gnus-thread-reader--decode-header (value)
  "Decode RFC 2047 header VALUE."
  (mail-decode-encoded-word-string (or value "")))

(defun gnus-thread-reader--author (header)
  "Return HEADER's display author without the Douban transport placeholder."
  (let* ((from (gnus-thread-reader--decode-header (mail-header-from header)))
         (parts (mail-extract-address-components from)))
    (if (and (string-match-p "\\`\\(?:noreply\\|[0-9]+\\)@douban\\.invalid\\'" (or (cadr parts) ""))
             (car parts) (not (string-empty-p (car parts))))
        (car parts)
      from)))

(defun gnus-thread-reader--state (number)
  "Return a display label for NUMBER's current Gnus state."
  (with-current-buffer (gnus-thread-reader--source)
    (let ((mark (gnus-summary-article-mark number)))
      (cond ((eq mark gnus-ticked-mark) "[Ticked]")
            ((eq mark gnus-dormant-mark) "[Dormant]")
            ((eq mark gnus-unread-mark) "[Unread]")
            ((eq mark gnus-expirable-mark) "[Expirable]")
            ((memq number gnus-newsgroup-cached) "[Saved]")
            (t "[Read]")))))

(defun gnus-thread-reader--render ()
  "Render the view, retaining the viewport, point and folded subtrees."
  (let ((windows
         (mapcar
          (lambda (window)
            (let* ((pos (window-start window))
                   (id (get-text-property pos 'thread-reader-id))
                   (start (and id (gethash id thread-reader--positions))))
              (list window id (if start (- pos start) 0) pos
                    (window-hscroll window) (window-vscroll window t))))
          (get-buffer-window-list (current-buffer) nil t))))
    (maphash
     (lambda (id header)
       (let* ((entry (gethash id thread-reader--entries))
              (number (mail-header-number header)))
         (setf (thread-reader-entry-author entry)
               (concat (if (> number 0) (gnus-thread-reader--state number)
                         "[Missing]")
                       " " (gnus-thread-reader--author header)))))
     gnus-thread-reader--headers)
    (thread-reader--render)
    (let ((inhibit-read-only t))
      (maphash
       (lambda (id header)
         (let* ((address (cadr (mail-extract-address-components
                               (gnus-thread-reader--decode-header (mail-header-from header)))))
                (start (gethash id thread-reader--positions))
                (number (mail-header-number header)))
           (when (and start (> number 0)
                      (equal (gnus-thread-reader--state number) "[Read]"))
             (save-excursion
               (goto-char start)
               (add-face-text-property start (line-end-position) 'shadow)))
           (when (and start (string-match "\\`\\([0-9]+\\)@douban\\.invalid\\'" (or address "")))
             (let ((url (concat "https://www.douban.com/people/" (match-string 1 address) "/"))
                   (name (gnus-thread-reader--author header)))
               (save-excursion
                 (goto-char start)
                 (when (search-forward name (line-end-position) t)
                   (make-text-button (- (point) (length name)) (point)
                                     'face 'link 'help-echo url 'follow-link t
                                     'action (lambda (_button) (browse-url url)))))))))
       gnus-thread-reader--headers))
    (dolist (state windows)
      (pcase-let ((`(,window ,id ,offset ,old ,hscroll ,vscroll) state))
        (when (window-live-p window)
          (let* ((start (and id (gethash id thread-reader--positions)))
                 (pos (min (point-max) (if start (+ start offset) old))))
            (when (or (invisible-p pos)
                      (and start (not (equal id (get-text-property pos 'thread-reader-id)))))
              (setq pos (or start old)))
            (set-window-start window pos t)
            (set-window-hscroll window hscroll)
            (set-window-vscroll window vscroll t)))))
    (setq header-line-format
          (format "Gnus · %d articles · %d loading   d read · U unread · ! tick · N next unread · r reply · o original"
                  (hash-table-count gnus-thread-reader--headers)
                  (length gnus-thread-reader--queue)))))

(defun gnus-thread-reader--cancel ()
  "Cancel pending body loading and invalidate old work."
  (cl-incf gnus-thread-reader--generation)
  (when (timerp gnus-thread-reader--timer)
    (cancel-timer gnus-thread-reader--timer))
  (setq gnus-thread-reader--timer nil))

(defun gnus-thread-reader--schedule ()
  "Schedule one body request, yielding between articles."
  (when (and gnus-thread-reader--queue (not gnus-thread-reader--timer))
    (setq gnus-thread-reader--timer
          (run-at-time 0.05 nil #'gnus-thread-reader--load-one
                       (current-buffer) gnus-thread-reader--generation))))

(defun gnus-thread-reader--mime-parts (handle)
  "Extract safe inline text parts from MIME HANDLE as (FORMAT . TEXT).
Alternative bodies are shown once, preferring HTML.  Attachments, signatures
and encrypted data are left to Gnus's native article viewer."
  (let ((type (mm-handle-media-type handle)))
    (cond
     ((equal type "multipart/encrypted") nil)
     ((stringp (car handle))
      (let ((parts (cdr handle)))
        (cond
         ((equal type "multipart/alternative")
          (let ((alternatives (mapcar #'gnus-thread-reader--mime-parts parts)))
            (or (cl-find-if (lambda (items) (assq 'html items)) alternatives)
                (cl-find-if #'identity alternatives))))
         ((member type '("multipart/related" "multipart/signed"))
          (gnus-thread-reader--mime-parts (car parts)))
         (t (mapcan #'gnus-thread-reader--mime-parts parts)))))
     ((and (member type '("text/plain" "text/html"))
           (not (equal (car (mm-handle-disposition handle)) "attachment")))
      (let* ((charset (mail-content-type-get (mm-handle-type handle) 'charset))
             (coding (or (mm-charset-to-coding-system charset) 'utf-8))
             (bytes (mm-get-part handle)))
        (list (cons (if (equal type "text/html") 'html 'plain)
                    (if (eq charset 'gnus-decoded) bytes
                      (decode-coding-string bytes coding)))))))))

(defun gnus-thread-reader--body-from-buffer ()
  "Decode the current raw message, returning (FORMAT . TEXT).
Never run an attachment viewer or fetch remote resources."
  (let ((mm-decrypt-option 'never) (mm-verify-option 'never) handles)
    (unwind-protect
        (let* ((parts (gnus-thread-reader--mime-parts
                       (setq handles (mm-dissect-buffer t))))
               (html (assq 'html parts)))
          (cond
           ((null parts) '(plain . "[Open the original article with o to view this MIME content.]"))
           ((not html) (cons 'plain (mapconcat #'cdr parts "\n\n")))
           (t (cons 'html
                    (mapconcat
                     (lambda (part)
                       (if (eq (car part) 'html) (cdr part)
                         (concat "<p>"
                                 (replace-regexp-in-string
                                  "\n" "<br>" (xml-escape-string (cdr part)))
                                 "</p>"))) parts "\n")))))
      (when handles (mm-destroy-parts handles)))))

(defun gnus-thread-reader--fetch (header)
  "Fetch HEADER through Gnus, including its cache, without reading it."
  (let* ((number (mail-header-number header))
         (gnus-summary-buffer (gnus-thread-reader--source))
         (gnus-newsgroup-name gnus-thread-reader--group))
    (gnus-thread-reader--checked-header number (mail-header-id header))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (unless (gnus-request-article-this-buffer number gnus-newsgroup-name)
        (error "Article unavailable from Gnus"))
      (gnus-thread-reader--body-from-buffer))))

(defun gnus-thread-reader--mark-current-read ()
  "Mark the loaded article at point read once when it is visited.
Prefetched articles and a manual unread mark on the same article are left alone."
  (when-let* ((id (thread-reader--current-id))
              ((not (equal id gnus-thread-reader--last-visited-id)))
              ((gethash id gnus-thread-reader--bodies))
              (header (gethash id gnus-thread-reader--headers))
              (number (mail-header-number header))
              ((> number 0)))
    (gnus-thread-reader--checked-header number (mail-header-id header))
    (setq gnus-thread-reader--last-visited-id id)
    (with-current-buffer (gnus-thread-reader--source)
      (when (eq (gnus-summary-article-mark number) gnus-unread-mark)
        (gnus-summary-mark-article number gnus-read-mark)
        (gnus-set-mode-line 'summary)
        t))))

(defun gnus-thread-reader--visit-at-point ()
  "Update Gnus read state after moving to a loaded article."
  (when (and (derived-mode-p 'gnus-thread-reader-mode)
             (gnus-thread-reader--mark-current-read))
    (gnus-thread-reader--render)))

(defun gnus-thread-reader--load-one (buffer generation)
  "Load one article for BUFFER if GENERATION is current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'gnus-thread-reader-mode)
                 (= generation gnus-thread-reader--generation))
        (setq gnus-thread-reader--timer nil)
        (condition-case err
            (progn
              (gnus-thread-reader--source)
              (let* ((id (pop gnus-thread-reader--queue))
                     (header (gethash id gnus-thread-reader--headers))
                     result failure)
                (condition-case problem
                    (setq result (gnus-thread-reader--fetch header))
                  (error (setq failure (error-message-string problem))))
                ;; Synchronous Gnus backends can yield while waiting for I/O.
                (when (and (buffer-live-p buffer)
                           (= generation gnus-thread-reader--generation))
                  (let ((entry (gethash id thread-reader--entries)))
                    (unless failure (puthash id result gnus-thread-reader--bodies))
                    (setf (thread-reader-entry-body-format entry) (if failure 'plain (car result))
                          (thread-reader-entry-body entry)
                          (if failure (format "[Loading failed: %s. Press g to retry.]" failure)
                            (cdr result)))
                    (unless failure (gnus-thread-reader--mark-current-read))
                    (gnus-thread-reader--render)
                    (gnus-thread-reader--schedule)))))
          (error
           (setq gnus-thread-reader--queue nil
                 header-line-format (concat "Gnus · " (error-message-string err)))))))))

(defun gnus-thread-reader-refresh (&optional refetch &rest _)
  "Reconcile the view with its Gnus summary, retrying failed bodies.
With prefix REFETCH, reload all bodies.  This does not rescan the server;
use Gnus's normal update commands and then refresh or reopen this view."
  (interactive "P")
  (let* ((anchor-id gnus-thread-reader--anchor-id)
         (header (gnus-thread-reader--checked-header gnus-thread-reader--anchor anchor-id))
         (tree (with-current-buffer (gnus-thread-reader--source)
                 (or (gnus-remove-thread anchor-id t)
                     (list header))))
         (records (gnus-thread-reader--flatten tree))
         (headers (make-hash-table :test #'equal)) entries queue)
    (gnus-thread-reader--cancel)
    (when refetch (clrhash gnus-thread-reader--bodies))
    (dolist (record records)
      (pcase-let* ((`(,item ,parent) record)
                   (number (mail-header-number item))
                   (id (number-to-string number))
                   (previous (gethash id gnus-thread-reader--headers))
                   (body (and (or (null previous)
                                  (equal (mail-header-id previous) (mail-header-id item)))
                              (gethash id gnus-thread-reader--bodies))))
        (when (and previous (not (equal (mail-header-id previous) (mail-header-id item))))
          (remhash id gnus-thread-reader--bodies))
        (puthash id (copy-sequence item) headers)
        (when (and (> number 0) (not body)) (push id queue))
        (push (make-thread-reader-entry
               :id id :parent-id (and parent (number-to-string parent))
               :author (gnus-thread-reader--author item)
               :time (or (mail-header-date item) "")
               :placeholder-p (<= number 0)
               :body-format (or (car body) 'plain)
               :body (or (cdr body) (if (> number 0) "Loading article…"
                                     "[Ancestor not available in this Gnus summary.]")))
              entries)))
    (thread-reader--merge (nreverse entries) t)
    (setq gnus-thread-reader--headers headers
          gnus-thread-reader--queue (nreverse queue)
          thread-reader--discussion
          (make-thread-reader-discussion
           :id gnus-thread-reader--anchor-id :url gnus-thread-reader--group
           :title (gnus-thread-reader--decode-header (mail-header-subject header))))
    (gnus-thread-reader--render)
    (gnus-thread-reader--schedule)))

(defun gnus-thread-reader--target ()
  "Return the current real article number after checking its identity."
  (let* ((id (thread-reader-entry-id (thread-reader-current-entry)))
         (header (gethash id gnus-thread-reader--headers))
         (number (and header (mail-header-number header))))
    (unless (and number (> number 0)) (user-error "This is a missing ancestor"))
    (gnus-thread-reader--checked-header number (mail-header-id header))
    number))

(defun gnus-thread-reader--mark (mark)
  "Apply Gnus MARK to the current article, without touching other articles."
  (let ((number (gnus-thread-reader--target)))
    (save-window-excursion
      (with-current-buffer (gnus-thread-reader--source)
        (save-excursion
          (gnus-summary-mark-article number mark)
          (gnus-set-mode-line 'summary))))
    (gnus-thread-reader--render)))

(defun gnus-thread-reader-mark-read ()
  "Mark only this article read in Gnus."
  (interactive) (gnus-thread-reader--mark gnus-read-mark))

(defun gnus-thread-reader-mark-unread ()
  "Mark only this article unread in Gnus."
  (interactive) (gnus-thread-reader--mark gnus-unread-mark))

(defun gnus-thread-reader-tick ()
  "Keep this article for later using Gnus's ticked mark."
  (interactive) (gnus-thread-reader--mark gnus-ticked-mark))

(defun gnus-thread-reader-dormant ()
  "Make this article dormant in Gnus."
  (interactive) (gnus-thread-reader--mark gnus-dormant-mark))

(defun gnus-thread-reader--reveal (id)
  "Reveal ID and its ancestors, updating the saved fold state."
  (let ((parent (thread-reader-entry-parent-id (gethash id thread-reader--entries))))
    (while parent
      (remhash parent thread-reader--collapsed)
      (setq parent (thread-reader-entry-parent-id (gethash parent thread-reader--entries)))))
  (remhash id thread-reader--collapsed)
  (gnus-thread-reader--render)
  (goto-char (gethash id thread-reader--positions)))

(defun gnus-thread-reader-next-unread ()
  "Jump to the next unread article, including replies hidden by folding."
  (interactive)
  (let* ((id (thread-reader--current-id))
         (order (sort (copy-sequence thread-reader--order)
                      (lambda (a b) (< (gethash a thread-reader--positions)
                                       (gethash b thread-reader--positions)))))
         (tail (cdr (member id order)))
         (candidates (append (or tail (unless id order))
                             (and id (cl-subseq order 0 (cl-position id order :test #'equal)))))
         (summary (gnus-thread-reader--source))
         (next (cl-find-if
                (lambda (candidate)
                  (and (or (null gnus-thread-reader-focus-ids)
                           (member candidate gnus-thread-reader-focus-ids))
                       (with-current-buffer summary
                         (gnus-summary-article-unread-p (string-to-number candidate)))))
                candidates)))
    (unless next (user-error "No other unread article in this thread"))
    (gnus-thread-reader--reveal next)))

(defun gnus-thread-reader-original ()
  "Open this article in Gnus's native viewer, with full MIME support."
  (interactive)
  (let ((number (gnus-thread-reader--target))
        (summary (gnus-thread-reader--source)))
    (pop-to-buffer summary)
    (gnus-summary-goto-subject number t)
    (gnus-summary-select-article nil nil nil number)))

(defun gnus-thread-reader-reply (&optional quote)
  "Compose a native Gnus reply to this article; with prefix QUOTE it.
News articles use followup; mail articles use reply-to-author."
  (interactive "P")
  (let ((number (gnus-thread-reader--target))
        (summary (gnus-thread-reader--source)))
    (with-current-buffer summary
      (save-excursion
        (unless (gnus-summary-goto-subject number t)
          (user-error "Article is no longer available in the summary"))
        (let ((type (gnus-request-type gnus-newsgroup-name number)))
          (pcase type
            ('post (if quote (gnus-summary-followup-with-original 1)
                     (gnus-summary-followup nil)))
            ('mail (if quote (gnus-summary-reply-with-original 1)
                     (gnus-summary-reply)))
            (_ (user-error "Unknown reply transport; use the original Gnus article"))))))))

(defun gnus-thread-reader-save ()
  "Make this article persistent using Gnus's native cache."
  (interactive)
  (let ((number (gnus-thread-reader--target)))
    (save-window-excursion
      (with-current-buffer (gnus-thread-reader--source)
        (save-excursion
          (gnus-summary-goto-subject number t)
          ;; Explicit count avoids acting on unrelated process-marked articles.
          (gnus-cache-enter-article 1))))
    (gnus-thread-reader--render)))

(defun gnus-thread-reader-summary ()
  "Return to the source summary at this article without reading it."
  (interactive)
  (let ((number (gnus-thread-reader--target))
        (summary (gnus-thread-reader--source)))
    (pop-to-buffer summary)
    (gnus-summary-goto-subject number t)))

(defvar gnus-thread-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map thread-reader-mode-map)
    (define-key map (kbd "g") #'gnus-thread-reader-refresh)
    (define-key map (kbd "r") #'gnus-thread-reader-reply)
    (define-key map (kbd "d") #'gnus-thread-reader-mark-read)
    (define-key map (kbd "U") #'gnus-thread-reader-mark-unread)
    (define-key map (kbd "!") #'gnus-thread-reader-tick)
    (define-key map (kbd "?") #'gnus-thread-reader-dormant)
    (define-key map (kbd "*") #'gnus-thread-reader-save)
    (define-key map (kbd "N") #'gnus-thread-reader-next-unread)
    (define-key map (kbd "o") #'gnus-thread-reader-original)
    (define-key map (kbd "s") #'gnus-thread-reader-summary)
    (define-key map (kbd "m") #'ignore)
    map))

(define-derived-mode gnus-thread-reader-mode thread-reader-mode "Gnus Thread"
  "Read a Gnus conversation continuously, with Outline folding.
Visiting a loaded article marks it read.  Background loading leaves other
articles unread.
\{gnus-thread-reader-mode-map}"
  (setq-local thread-reader-auto-load-replies nil)
  (setq-local revert-buffer-function #'gnus-thread-reader-refresh)
  (setq gnus-thread-reader--headers (make-hash-table :test #'equal)
        gnus-thread-reader--bodies (make-hash-table :test #'equal))
  (add-hook 'post-command-hook #'gnus-thread-reader--visit-at-point nil t)
  (add-hook 'kill-buffer-hook #'gnus-thread-reader--cancel nil t)
  (add-hook 'change-major-mode-hook #'gnus-thread-reader--cancel nil t))

;;;###autoload
(defun gnus-thread-reader-open ()
  "Open the current Gnus summary article's thread as continuous prose."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Run this command in a Gnus summary"))
  (let* ((summary (current-buffer))
         (group gnus-newsgroup-name)
         (number (gnus-summary-article-number))
         (header (and number (gnus-summary-article-header number))))
    (unless (and (mail-header-p header) (> number 0))
      (user-error "Select a real article in the Gnus summary"))
    ;; Let Gnus complete the thread through any backend that implements
    ;; request-thread.  The reader remains independent of backend names.
    (when (gnus-check-backend-function 'request-thread group)
      (gnus-summary-refer-thread nil)
      (when gnus-thread-reader-summary-mode
        (gnus-summary-maybe-hide-threads)))
    (let ((buffer (generate-new-buffer "*Gnus Thread*")))
      (condition-case err
          (with-current-buffer buffer
            (gnus-thread-reader-mode)
            (setq gnus-thread-reader--summary summary
                  gnus-thread-reader--group group
                  gnus-thread-reader--anchor number
                  gnus-thread-reader--anchor-id (mail-header-id header)
                  thread-reader--url group)
            (gnus-thread-reader-refresh)
            (goto-char (gethash (number-to-string number) thread-reader--positions)))
        (error (kill-buffer buffer) (signal (car err) (cdr err))))
      (pop-to-buffer buffer)
      buffer)))

(defvar gnus-thread-reader-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gnus-thread-reader-open)
    map))

(defvar-local gnus-thread-reader--summary-settings nil)

(defun gnus-thread-reader--summary-policy ()
  "Use Gnus's native collapsed thread display in reader summaries.
Run after group parameters are applied, including when Gnus regenerates
an existing summary.  Gnus performs the folding after kill processing."
  (when gnus-thread-reader-summary-mode
    (setq-local gnus-show-threads t)
    (setq-local gnus-thread-hide-subtree t)))

;;;###autoload
(define-minor-mode gnus-thread-reader-summary-mode
  "Keep conversations collapsed and open them with RET in the reader.
This applies to any Gnus summary, independently of its backend or group."
  :lighter "" :keymap gnus-thread-reader-summary-mode-map
  (if gnus-thread-reader-summary-mode
      (progn
        (unless gnus-thread-reader--summary-settings
          (setq gnus-thread-reader--summary-settings
                (mapcar (lambda (symbol)
                          (list symbol (local-variable-p symbol)
                                (symbol-value symbol)))
                        '(gnus-show-threads gnus-thread-hide-subtree))))
        (add-hook 'gnus-summary-generate-hook
                  #'gnus-thread-reader--summary-policy nil t)
        (gnus-thread-reader--summary-policy)
        (when gnus-newsgroup-threads
          (gnus-summary-maybe-hide-threads)))
    (remove-hook 'gnus-summary-generate-hook
                 #'gnus-thread-reader--summary-policy t)
    (dolist (setting gnus-thread-reader--summary-settings)
      (if (nth 1 setting)
          (set (car setting) (nth 2 setting))
        (kill-local-variable (car setting))))
    (setq gnus-thread-reader--summary-settings nil)))

;;;###autoload
(define-globalized-minor-mode gnus-thread-reader-global-summary-mode
  gnus-thread-reader-summary-mode
  (lambda ()
    (when (derived-mode-p 'gnus-summary-mode)
      (gnus-thread-reader-summary-mode 1)))
  :group 'gnus)

(provide 'gnus-thread-reader)
;;; gnus-thread-reader.el ends here
