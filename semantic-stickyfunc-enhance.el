;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun semantic-stickyfunc-fetch-stickyline ()
  "Make the function at the top of the current window sticky.
Capture its function declaration, and place it in the header line.
If there is no function, disable the header line."
  (save-excursion
    (goto-char (window-start (selected-window)))
    (let* ((noshow (bobp))
           (str
            (progn
              (forward-line -1)
              (end-of-line)
              ;; Capture this function
              (let* ((tag (semantic-stickyfunc-tag-to-stick))
                     param-tags filtered-tags tmp-str)
                ;; TAG is nil if there was nothing of the appropriate type there.
                (if (not tag)
                    ;; Set it to be the text under the header line
                    (if noshow
                        ""
                      (if semantic-stickyfunc-show-only-functions-p ""
                        (buffer-substring (point-at-bol) (point-at-eol))))
                  (setq param-tags (semantic-tag-function-arguments tag))
                  (setq filtered-tags (semantic--tags-out-of-screen param-tags tag)) ;
                  (setq tmp-str (semantic-format-tag-prototype tag nil t))
                  (if (= (length param-tags) (length filtered-tags))
                      tmp-str
                    (string-match (semantic--parameters-regexp tag) tmp-str)
                    (setq tmp-str (replace-match (semantic--replace-match-text tag) t t tmp-str 0))
                    (dolist (v filtered-tags)
                      (setq tmp-str (concat tmp-str
                                            (if (listp v)
                                                (semantic-format-tag-prototype v nil t)
                                              (propertize v 'face 'font-lock-variable-name-face))
                                            (semantic--function-argument-separator))))
                    tmp-str)))))
           (start 0))
      (while (string-match "%" str start)
        (setq str (replace-match "%%" t t str 0)
              start (1+ (match-end 0))))
      ;; In 21.4 (or 22.1) the header doesn't expand tabs.  Hmmmm.
      ;; We should replace them here.
      ;;
      ;; This hack assumes that tabs are kept smartly at tab boundaries
      ;; instead of in a tab boundary where it might only represent 4 spaces.
      (while (string-match "\t" str start)
        (setq str (replace-match "        " t t str 0)))
      str)))

(defun semantic--function-argument-separator ()
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    ",")
   ((eq major-mode 'emacs-lisp-mode)
    " ")
   (t ",")))

(defun semantic--replace-match-text (tag)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    "(")
   ((eq major-mode 'emacs-lisp-mode)
    (concat "(" (propertize (semantic-tag-name tag) 'face 'font-lock-function-name-face) " "))
   (t "(")))

(defun semantic--parameters-regexp (tag)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    "(.*)")
   ((eq major-mode 'emacs-lisp-mode)
    "(.*)")
   (t "(.*)")))

(defun semantic--tags-out-of-screen (tags parent-tag)
  (let ((start-line (line-number-at-pos (window-start))))
    (remove-if (lambda (tag)
                 (>= (line-number-at-pos (if (listp tag)
                                             (semantic-tag-start tag)
                                           (save-excursion
                                             (goto-char (semantic-tag-start parent-tag))
                                             (search-forward tag)
                                             (point))))
                     start-line))
               tags)))
