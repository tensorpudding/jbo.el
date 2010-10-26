;; jbo.el --- jbo integration with Emacs

(provide 'jbo)

(add-to-list 'auto-mode-alist '("*jbo*" . jbo-dict-mode))

;;; jbo faces

(defgroup jbo nil
  "jbo interaction"
  :prefix "jbo-")

(defface jbo-definition
  (let ((font (cond ((assq :inherit custom-face-attributes)))))
    '((((type tty) (class color))
       (:foreground "white"))
      (((class grayscale) (background light))
       (:foreground "black" ,@font))
      (((class grayscale) (background dark))
       (:foreground "white" ,@font))
      (((class color) (background light))
       (:foreground "black"))
      (((class color) (background dark))
       (:foreground "white"))
      (t (,@font))))
  "Face used for valsi definition body."
  :group 'jbo-dict-faces)

(defface jbo-cmene
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
      '((((class grayscale) (background light))
	 (:foreground "black" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "white" ,@font))
	(((class color) (background light))
	 (:foreground "#993b31"))
	(((class color) (background dark))
	 (:foreground "#ed8274"))
	(t (,@font))))
  "Face used for cmene definition lead."
  :group 'jbo-dict-faces)

(defface jbo-cmavo
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
      '((((class grayscale) (background light))
	 (:foreground "black" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "white" ,@font))
	(((class color) (background light))
	 (:foreground "#318f99"))
	(((class color) (background dark))
	 (:foreground "#74dfed"))
	(t (,@font))))
  "Face used for cmavo definition lead."
  :group 'jbo-dict-faces)

(defface jbo-gismu
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
      '((((class grayscale) (background light))
	 (:foreground "black" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "white" ,@font))
	(((class color) (background light))
	 (:foreground "#5a9931"))
	(((class color) (background dark))
	 (:foreground "#a3ed74"))
	(t (,@font))))
  "Face used for gismu definition lead."
  :group 'jbo-dict-faces)

(defface jbo-lujvo
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
      '((((class grayscale) (background light))
	 (:foreground "black" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "white" ,@font))
	(((class color) (background light))
	 (:foreground "#6f3199"))
	(((class color) (background dark))
	 (:foreground "#bf74ed"))
	(t (,@font))))
  "Face used for gismu definition lead."
  :group 'jbo-dict-faces)

(defface jbo-cmavo-class
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :weight custom-face-attributes) '(:weight italic))
		    (t '(:bold t)))))
      '((((class grayscale) (background light))
	 (:foreground "black" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "white" ,@font))
	(((class color) (background light))
	 (:foreground "#993150"))
	(((class color) (background dark))
	 (:foreground "#ed7496"))
	(t (,@font))))
  "Face used for cmavo classifications."
  :group 'jbo-dict-faces)


(defface jbo-link
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :weight custom-face-attributes) '(:weight italic))
		    (t '(:bold t)))))
      '((((class grayscale) (background light))
	 (:foreground "black" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "white" ,@font))
	(((class color) (background light))
	 (:foreground "black"))
	(((class color) (background dark))
	 (:foreground "white"))
	(t (,@font))))
  "Face used for valsi cross-references."
  :group 'jbo-dict-faces)

;;; jbo prettifiers

(defun jbo-text-former (valsi)
  (let* ((type (jbo-get-type valsi))
	(class (jbo-get-cmavo-class valsi))
	(definition (jbo-get-definition valsi))
	(notes (jbo-get-notes valsi))
	(lede (jbo-lede-former valsi type class)))
    (concat lede definition "\n" notes)))

(defun jbo-lede-former (valsi type class)
  "Returns the lede string \"VALSI (CLASS):\" with proper face properties added."
  (cond ((string= "cmavo" type)
	 (concat (propertize valsi 'face 'jbo-cmavo)
		 " ("
		 (propertize class 'face 'jbo-cmavo-class)
		 "): "))
	((string= "gismu" type)
	 (concat (propertize valsi 'face 'jbo-gismu)
		 ": "))
	((string= "lujvo" type)
	 (concat (propertize valsi 'face 'jbo-lujvo)
		 ": "))
	((string= "cmene" type)
	 (concat (propertize valsi 'face 'jbo-cmene)
		 ": "))
	(t
	 (concat (propertize valsi 'face 'jbo-definition)
		 ":"))))

(defun jbo-linkifier ()
  (let ((index 0)
	(link-regex "{\\([a-zA-Z']+\\)}"))
    (while (numberp (string-match link-regex (buffer-string) index))
      (make-button (+ 1 (match-beginning 1)) (+ 1 (match-end 1))
			     '(help-echo "mouse-1: lookup this valsi"
			       follow-link t
			       mouse-face highlight
			       action jbo-follow-link
			       face jbo-link))
      (setq index (match-end 0)))))

;;; jbo backend querying

(defun jbo-get-definition (valsi)
  "Query the jbo database for the definition of VALSI. If the definition exists return it, otherwise yield an error."
  (let* ((command-string (concat "jbo get -d \"" valsi "\""))
	(result (shell-command-to-string* command-string)))
    (if (string-prefix-p "error:" result)
	(error (concat valsi ": not found"))
      result)))

(defun jbo-get-type (valsi)
  "Query the jbo database for the type of VALSI. If it exists, return it, otherwise yield an error."
  (let* ((command-string (concat "jbo get -t \"" valsi "\""))
	(result (shell-command-to-string* command-string)))
    (if (string-prefix-p "error:" result)
	(error (concat valsi ": not found"))
      result)))

(defun jbo-get-cmavo-class (cmavo)
  "Query the jbo database for the class of CMAVO. If CMAVO is not a cmavo, returns the empty string, otherwise returns the class."
  (let* ((command-string (concat "jbo get -c \"" cmavo "\""))
	 (result (shell-command-to-string* command-string)))
    (if (string-prefix-p "error:" result)
	(error (concat cmavo ": not found"))
      result)))

(defun jbo-get-notes (valsi)
  "Query the jbovlaste database for notes attached to VALSI. If it exists, return it, otherwise yield an error."
  (let* ((command-string (concat "jbo get -n \"" valsi "\""))
	(result (shell-command-to-string* command-string)))
    (if (string-prefix-p "error:" result)
	(error (concat valsi ": not found"))
      result)))

;;; crap

(defun shell-command-to-string* (command-string)
  "Cut off that damn newline at the end"
  (substring (shell-command-to-string command-string) 0 -1))
  

(defun jbo-lookup ()
  "Prompt user for a valsi to look up, then returns the definition in the minibuffer *jbo*."
  (interactive)
  (jbo-lookup-definition (read-from-minibuffer "Valsi to look up: ")))

(defun jbo-follow-link (event)
  "Cross-reference valsi under point in *jbo*."
  (interactive "e")
  (goto-char (posn-point (event-end event)))
  (let ((link-regex "{\\([a-zA-Z']+\\)}")
	(curr (word-at-point)))
    (if (numberp (string-match link-regex curr))
	(jbo-lookup-definition (replace-regexp-in-string link-regex "\\1" curr)))))

(defun jbo-lookup-at-point ()
  "Lookup valsi at point"
  (interactive)
  (jbo-lookup-definition (current-word)))

(defun jbo-lookup-definition (valsi)
  (let ((jbuff (get-buffer-create "*jbo*"))
	(entry (jbo-text-former valsi)))
    (if (not (string= "" entry))
	(progn (pop-to-buffer jbuff)
	       (setq buffer-read-only nil)
	       (erase-buffer)
	       (insert (jbo-text-former valsi))
	       (fit-window-to-buffer)
	       (jbo-linkifier)
	       (jbo-dict-mode)))))

(defvar jbo-dict-mode-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jbo-follow-link)
    (define-key map [mouse-1] 'jbo-follow-link)
    map))

(defvar jbo-dict-mode-map
    (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'delete-window)
    map))

(defun jbo-dict-mode ()
  "Mode for viewing word definitions from jbo."
  (kill-all-local-variables)
  (setq mode-name "jbo")
  (setq major-mode 'jbo-dict-mode)
  (use-local-map jbo-dict-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'jbo-mode-hook))
