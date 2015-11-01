; org-mpw.el - masterpass integration

(require 'org)
(require 'cl)

(defgroup org-mpw nil
  "Masterpass integration"
  :tag "Masterpass"
  :group 'org)

(defcustom org-mpw-name-property "mpw_name"
  "Property with name"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-type-property "mpw_type"
  "Property with password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-command "mpw"
  "Command to execute"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-name nil
  "Name to be used"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-type-default "long"
  "Default type is 'long'"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-long "long"
  "Tag name for 'long' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-max "max"
  "Tag name for 'max' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-med "med"
  "Tag name for 'med' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-basic "basic"
  "Tag name for 'basic' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-short "short"
  "Tag name for 'short' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-pin "pin4"
  "Tag name for 'pin' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-name "name"
  "Tag name for 'name' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-phrase "phrase"
  "Tag name for 'phrase' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-cache 0
  "Number of seconds to keep master pass in cache"
  :type 'integer
  :group 'org-mpw)

(defcustom org-mpw-openssl "openssl"
  "Path to openssl executable"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-openssl-cipher "aes256"
  "Symmetric encryption cipher"
  :type 'string
  :group 'org-mpw)

(defvar org-mpw-cached-pass nil)
(defvar org-mpw-cache-timer nil)

(defun org-mpw-clear-cache ()
  "Clear cache, if set"
  (when org-mpw-cache-timer
    (cancel-timer org-mpw-cache-timer)
    (setq org-mpw-cache-timer nil))
  (setq org-mpw-cached-pass nil))

(defun org-mpw-cache-pass (pass)
  "Cache master pass, if configured"
  (when (> org-mpw-cache 0)
    (when org-mpw-cache-timer
      (cancel-timer org-mpw-cache-timer))
    (setq org-mpw-cached-pass pass)
    (setq org-mpw-cache-timer
	  (run-at-time (format "%d sec" org-mpw-cache) nil 'org-mpw-clear-cache)))
  pass)

(defun org-mpw-get-name ()
  "Return name"
  (save-excursion
    (org-back-to-heading t)
    (or (org-entry-get nil org-mpw-name-property t)
        org-mpw-name)))

(defun org-mpw-get-type-tag ()
  "Return password type set by tag"
  (dolist (tag (org-get-tags))
    (let 
	((value (cond ((equal tag org-mpw-tag-max) "max")
		      ((equal tag org-mpw-tag-long) "long")
		      ((equal tag org-mpw-tag-med) "med")
		      ((equal tag org-mpw-tag-basic) "basic")
		      ((equal tag org-mpw-tag-short) "short")
		      ((equal tag org-mpw-tag-phrase) "phrase")
		      ((equal tag org-mpw-tag-name) "name")
		      ((equal tag org-mpw-tag-pin) "pin"))))
      (if value (return value) nil))))

(defun org-mpw-get-type ()
  "Return Current password type"
  (save-excursion
    (org-back-to-heading t)
    (or (org-mpw-get-type-tag)
	(org-entry-get nil org-mpw-type-property t)
        org-mpw-type-default)))

(defun org-mpw-get-master (name)
  "Read master pass or return cached"
  (if org-mpw-cached-pass 
      (org-mpw-cache-pass org-mpw-cached-pass)
    (org-mpw-cache-pass (read-passwd (format "[%s] Master pass:" name)))))

(defun org-mpw-trim (text)
  "Trim input string"
  (if (string-match "[\r\n\t ]+$" text)
      (replace-match "" nil nil text)
    text))

(defun org-mpw-make-password (name site type count)
  "Call mpw"
  (org-mpw-trim 
   (shell-command-to-string
    (concat
     "echo " (shell-quote-argument (org-mpw-get-master name))
     "|"
     org-mpw-command
     " -u " (shell-quote-argument name)
     " -c " count
     " -t " type
     " -s "
     (shell-quote-argument site)))))

(defun org-mpw-openssl-encrypt (password text)
  "Encrypt with openssl"
  (shell-command-to-string
   (concat
    "echo " (shell-quote-argument text)
    "|"
    org-mpw-openssl
    " enc"
    (concat " -" org-mpw-openssl-cipher)
    " -base64 -A"
    " -pass " (shell-quote-argument (concat "pass:" password)))))

(defun org-mpw-openssl-decrypt (password text)
  "Decrypt with openssl"
  (org-mpw-trim (shell-command-to-string
   (concat
    "echo " (shell-quote-argument text)
    "|"
    org-mpw-openssl
    " enc"
    (concat " -" org-mpw-openssl-cipher)
    " -base64 -A"
    " -d"
    " -pass " (shell-quote-argument (concat "pass:" password))))))

(defun org-mpw-current-password ()
  "Return password for current heading"
  (let ((name (org-mpw-get-name)) (site (org-get-heading t t)))
    (if name
	(org-mpw-make-password name site (org-mpw-get-type) "1")
      nil)))

(defun org-mpw-password (&optional printout)
  "Copy password to kill-ring or print it out"
  (interactive "P")
  (let ((password (org-mpw-current-password)))
    (if password
	(if printout
	    (message "[%s]: %s" (org-get-heading t t) password)
	  (kill-new password))
      (message "No password available!"))))

(defun org-mpw-encrypt (&optional printout)
  "Copy encrypted input to kill-ring or insert it to current buffer"
  (interactive "P")
  (let ((password (org-mpw-current-password)) (input (read-string "Enter text:")))
    (if (and password input)
	(let ((encrypted (org-mpw-openssl-encrypt password input)))
	  ;(message "Encrypt %s with %s" input password)
	  (if encrypted
	      (if printout
		  (insert encrypted)
		(kill-new encrypted))
	    nil))
      (message "No password available!"))))

(defun org-mpw-decrypt (&optional printout)
  "Copy decrypted current word to kill-ring or print it out"
  (interactive "P")
  (let ((password (org-mpw-current-password)) (input (current-word)))
    (if (and password input)
	(let ((decrypted (org-mpw-openssl-decrypt password input)))
	  ;(message "Decrypt %s with %s" input password)
	  (if decrypted
	      (if printout
		  (message "Decrypted: %s" decrypted)
		(kill-new decrypted))
	    nil))
      (message "No password available!"))))

(defun org-mpw-encrypt-replace-region (&optional arg)
  (interactive "P")
  (if (and transient-mark-mode mark-active
           (not (eq (region-beginning) (region-end))))
      (let (
	    (input (filter-buffer-substring (region-beginning) (region-end) t))
	    (password (org-mpw-current-password)))
	(when (and input password)
	  (insert (org-mpw-openssl-encrypt password input))))))

(defun org-mpw-keepass-csv-to-org (&optional arg)
  (interactive "P")
  (let (
       (lines (split-string (car kill-ring) "\n"))
       (items (list ()))
       (index 0)
       (name (org-mpw-get-name)))
    (while (< index (length lines))
      (setq data (org-mpw-trim (nth index lines)))
      (when (> (length data) 0)
	;(message "Step: %d %s %s" index data(substring data -1))
	(while (not (equal "\"" (substring data -1)))
	  (setq index (1+ index))
	  (setq data (concat data "\n" (org-mpw-trim (nth index lines)))))
	;(message "Line: %s" data)
	(let (
	      (parts (split-string (substring data 1 -1) "\",\""))
	      (item "* "))
	  (let (
		(title (nth 0 parts))
		(user (nth 1 parts))
		(pass (org-mpw-trim (nth 2 parts)))
		(url (nth 3 parts))
		(memo (nth 4 parts))
		(password (org-mpw-make-password name (nth 0 parts) org-mpw-type-default "1")))
	    (setq item (concat item title "\n "))
	    (when (not (= 0 (length user)))
	      (setq item (concat item " Username: " user)))
	    (when (not (= 0 (length pass)))
	      (let ((encrypted (org-mpw-openssl-encrypt password pass)))
		;(message "Pass: [%s %s %s]" encrypted pass password)
		(setq item (concat item " Password: " encrypted))))
	    (setq item (concat item "\n"))
	    (when (not (or (= 0 (length url)) (equal url "http://")))
	      (setq item (concat item "  [[" url "]]\n")))
	    (when (not (= 0 (length memo)))
	      (setq item (concat item (mapconcat 
				       (function (lambda (x) (concat "  " x "\n")))
				       (split-string memo "\n") ""))))
	    (insert item))))
      (setq index (1+ index)))))

(provide 'org-mpw)
