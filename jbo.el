;; Under construction

(provide 'jbo)

(add-to-list 'auto-mode-alist '("*jbo*" . jbo-dict-mode))

(defun jbo-lookup ()
  "Prompt user for a valsi to look up, then returns the definition in the minibuffer *jbo*."
  (interactive)
  (jbo-define (read-from-minibuffer "Valsi to look up: ")))

(defun jbo-follow-link ()
  (interactive)
  (let ((link-regex "{\\([a-zA-Z',]+\\)}")
	(curr (current-word)))
    (if (numberp (string-match link-regex curr))
	(jbo-define (replace-regexp-in-string link-regex "\\1" curr)))))

(defun jbo-lookup-at-point ()
  (interactive)
  "Lookup valsi at point"
  (jbo-define (current-word)))

(defun jbo-define (valsi)
  (kill-buffer (get-buffer-create "*jbo*"))
  (let ((jbuff (get-buffer-create "*jbo*")))
    (call-process "jbo" nil jbuff nil "define" valsi)
    (pop-to-buffer jbuff)
    (jbo-dict-mode)))

(defvar jbo-dict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jbo-follow-link)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "Keymap for `jbo-dict-mode'.")

(defvar jbo-dict-font-lock-keywords
  '(("\\(-[a-zA-Z']+-[a-zA-Z'-]*\\)" 0 'font-lock-variable-name-face)
    ("\\([a-z][₁|₂|₃|₄|₅]\\)" 0 'font-lock-constant-face)
    ("{\\([a-zA-Z']+\\)}" 1 'font-lock-constant-face)
    ("^[:blank:]+\\([a-zA-Z']+\\):" 1 'font-lock-constant-face)
    ("^\\([a-zA-Z']+\\) " 0 'font-lock-keyword-face)))

(define-minor-mode jbo-mode
  "Minor mode for interfacing with jbo"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c M-l") 'jbo-lookup-at-point)
	    map)
  :global t)

(defun jbo-dict-mode ()
  "Mode for viewing word definitions from jbo."
  (kill-all-local-variables)
  (setq mode-name "jbo")
  (setq major-mode 'jbo-dict-mode)
  (setq buffer-read-only t)
  (use-local-map jbo-dict-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(jbo-dict-font-lock-keywords nil nil nil nil))
  (run-hooks 'jbo-mode-hook))
