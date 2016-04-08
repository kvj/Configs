(require 'neotree)

(defgroup ttt nil
  "TTT config"
  :tag "TTT")

(defcustom ttt-auto-hide-pattern nil
  "Auto-hide outline pattern"
  :type 'string
  :group 'ttt)

(defun save-buffer-if-visiting-file (&optional args)
  "Save buffer on auto-save"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))

(defun ttt-hide-marked ()
  (interactive)
  (if ttt-auto-hide-pattern
      (save-excursion
	(goto-char 1)
	(while (re-search-forward ttt-auto-hide-pattern nil t)
	  (hide-subtree)))))

(defcustom neo-archive-tmpl "${d}${f}.archive"
  "*The banner message of neotree window."
  :type 'string
  :group 'neotree)

(defun neotree-make-archive ()
  "Make archive according to template"
  (interactive)
  (let* ((filename (neo-buffer--get-filename-current-line)))
    (catch 'end
      (if (null filename) (throw 'end nil))
      (if (not (file-exists-p filename)) (throw 'end nil))
      (if (not (yes-or-no-p (format "Do you really want to archive %S?" filename)))
          (throw 'end nil))
      (if (file-directory-p filename)
	  (throw 'end nil)) ; Directory - ignore
      (let* ((newfile 
	      (expand-file-name 
	       (format-time-string 
		(replace-regexp-in-string 
		 "${f}" 
		 (file-name-nondirectory filename) 
		 (replace-regexp-in-string
		  "${d}"
		  (file-name-directory filename) neo-archive-tmpl))))))
	(if (not (file-exists-p (file-name-directory newfile)))
	    (make-directory (file-name-directory newfile) t))
	(if (file-exists-p newfile)
	    (if (not (yes-or-no-p (format "Do you want to override %S?" newfile)))
		(throw 'end nil)))
	(copy-file filename newfile t)
	(message "Archived to: %S" newfile)))))

(define-minor-mode ttt-minor-mode
  "Toggle minor mode for .ttt files"
  :lighter " ttt"
  :group 'ttt
  :keymap '(([C-M-t] . ttt-hide-marked)
	    ([C-t] . outline-toggle-children))
  (setq-local outline-regexp "\\s-*")
  (setq-local outline-heading-end-regexp "\\(:\\|;\\)\n")
  (outline-minor-mode)
  (ttt-hide-marked)
  (add-hook 'auto-save-hook 'save-buffer-if-visiting-file :local t))

(define-key neotree-mode-map (kbd "a") 'neotree-make-archive)

(provide 'ttt)

