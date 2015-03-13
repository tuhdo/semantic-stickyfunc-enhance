(require 'cl)
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
                  ;; (message "tmp-str :%s" tmp-str)
                  (if (and (= (length param-tags) (length filtered-tags))
                           (not (eq major-mode 'python-mode)))
                      tmp-str
                    (if (not (eq (semantic-tag-class tag) 'function))
                        tmp-str
                      (string-match (stickyfunc-enhance--parameters-regexp tag) tmp-str)
                      (setq tmp-str (replace-match (stickyfunc-enhance--replace-match-text tag) t t tmp-str 0))
                      (dolist (v filtered-tags)
                        (setq tmp-str (concat tmp-str
                                              (stickyfunc-enhance--function-argument-string v)
                                              (stickyfunc-enhance--function-argument-separator)))))
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

(defun stickyfunc-enhance--function-argument-string (tag)
  (cond
   ((eq major-mode 'python-mode)
    (save-excursion
      (let* ((tag-start (semantic-tag-start tag))
             (next-tag (save-excursion
                         (goto-char tag-start)
                         (semantic-find-tag-by-overlay-next)))
             (next-tag-start (if (not next-tag)
                                 (search-forward ":")
                               (semantic-tag-start next-tag))))
        (string-trim
         (replace-regexp-in-string "\\Ca.*"
                                   ""
                                   (buffer-substring tag-start
                                                     next-tag-start))))))
   (t
    (if (listp tag)
        (semantic-format-tag-prototype tag nil t)
      (propertize tag 'face 'font-lock-variable-name-face)))))

(defun stickyfunc-enhance--function-argument-separator ()
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    ",")
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'python-mode))
    " ")
   (t ",")))

(defun stickyfunc-enhance--replace-match-text (tag)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    "(")
   ((eq major-mode 'emacs-lisp-mode)
    (concat "(" (propertize (semantic-tag-name tag) 'face 'font-lock-function-name-face) " "))
   (t "(")))

(defun stickyfunc-enhance--parameters-regexp (tag)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    "(.*)")
   ((eq major-mode 'emacs-lisp-mode)
    "(.*)")
   (t "(.*)")))

(defun stickyfunc-enhance--tags-out-of-screen (tags parent-tag)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stickyfunc-enhance.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
