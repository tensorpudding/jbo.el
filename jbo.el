;; jbo.el --- jbo integration with Emacs

(provide 'jbo)
(add-to-list 'auto-mode-alist '("*jbo*" . jbo-dict-mode))

(defun jbo-prettifier (entry)
  "Prettifies the jbo dictionary entry"
  (let* ((index 0)
	 (lede-regex "^[a-zA-Z']+")
	 (link-regex "{[a-zA-Z']+}")
	 (text (replace-regexp-in-string lede-regex 'jbo-ledefier entry)))
    (replace-regexp-in-string link-regex 'jbo-linkfier text)))

(defun jbo-ledefier (word)
  (propertize word 'face 'bold))

(defun jbo-linkfier (word)
  (propertize (substring word 1 -1)
	      'face 'link
;	      'mouse-face 'highlight
;	      'follow-link t
;	      'help-echo "mouse-1: look up this valsi"
	      'keymap jbo-link-keymap))

(defvar jbo-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jbo-lookup-at-point)
;;    (define-key map [Mouse-2] 'jbo-follow-link-with-mouse)
    map))

;;; jbo backend querying

(defun jbo-get-definition (valsi)
  "Query the jbo database for the definition of VALSI. If the definition exists return it, otherwise yield an error."
  (let* ((command-string (concat "jbo define \"" valsi "\""))
	(result (shell-command-to-string* command-string)))
    (if (string-prefix-p "error:" result)
	(error (concat valsi ": not found"))
      result)))

;;; crap

(defun shell-command-to-string* (command-string)
  "Cut off that damn newline at the end"
  (substring (shell-command-to-string command-string) 0 -1))
  
(defun jbo-lookup (name)
  "Prompt user for a valsi to look up, then returns the definition in the minibuffer *jbo*."
  (interactive "sValsi to look up: ")
  (when (or (not (stringp name))
	    (string-equal "" name))
    (error "Need to specify a word."))
  (jbo-lookup-definition name))

;; (defun jbo-follow-link-with-mouse (event)
;;   "Cross-reference valsi under point in *jbo*."
;;   (interactive "e")
;;   (let ((window (posn-window (event-end event)))
;; 	(pos (posn-point (event-end event))))
;;     (with-current-buffer (window-buffer window)
;;       (goto-char pos)
;;       (jbo-lookup-definition (word-at-point)))))

(defun jbo-lookup-at-point ()
  "Lookup valsi at point"
  (interactive)
  (jbo-lookup-definition (current-word)))

(defun jbo-lookup-definition (valsi)
  (let ((entry (jbo-get-definition valsi)))
    (if (not (string= "" entry))
	(progn (jbo-get-buffer-create)
	       (jbo-display-definition (ring-insert jbo-history (jbo-prettifier entry)))))))

(defun jbo-get-buffer-create ()
  (let ((jbuff (get-buffer "*jbo*")))
    (if (not jbuff)
	(progn (pop-to-buffer (get-buffer-create "*jbo*"))
	       (jbo-dict-mode))
      (pop-to-buffer jbuff))))

(defun jbo-previous-definition ()
  (interactive)
  (jbo-get-buffer-create)
  (ring-insert-at-beginning jbo-history (ring-remove jbo-history 0))
  (jbo-display-definition (ring-ref jbo-history 0)))

(defun jbo-next-definition ()
  (interactive)
  (jbo-get-buffer-create)
  (ring-insert jbo-history (ring-remove jbo-history))
  (jbo-display-definition (ring-ref jbo-history 0)))

(defun jbo-display-definition (definition)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert definition)
  (fit-window-to-buffer)
  (setq buffer-read-only t))

(defvar jbo-dict-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "q") 'delete-window)
      (define-key map "\C-c\C-b" 'jbo-previous-definition)
      (define-key map "\C-c\C-f" 'jbo-next-definition)
    map))

(defvar jbo-max-history 100
  "Maximum number of dictionary entries to remember")

(defun jbo-dict-mode ()
  "Major mode for viewing dictionary definitions from jbovlaste, the official Lojban dictionary. 
Commands:
\\{jbo-dict-mode-map}
"
  (kill-all-local-variables)
  (use-local-map jbo-dict-mode-map)
  (setq mode-name "jbo")
  (setq major-mode 'jbo-dict-mode)
  (setq buffer-read-only t)
  (make-local-variable 'jbo-history)
  (setq jbo-history (make-ring jbo-max-history))
  (run-hooks 'jbo-mode-hook))
