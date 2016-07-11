(put 'upcase-region 'disabled nil)
(menu-bar-mode 0)
(windmove-default-keybindings)
(column-number-mode t)
(global-hl-line-mode t)
(blink-cursor-mode t)
(setq ps-paper-type 'a4)
(setq initial-buffer-choice t)
(setq tab-width 2)
(setq visible-bell t)
(setq make-backup-files nil)
(transient-mark-mode t)
(setq search-highlight t)
(setq query-replace-highlight t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-major-mode 'text-mode)
(set-default 'truncate-lines nil)
;(setq debug-on-error t)
(setq speedbar-use-images nil)
(let ((default-directory "~/.emacs.d/site-lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path)
      )
(add-to-list 'load-path (concat config-dir "lisp/"))

(set-language-environment 'utf-8)
(set-locale-environment "C")
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq global-auto-revert-mode t)
(global-auto-revert-mode t)
(global-visual-line-mode t)
(global-set-key (kbd "C-q") 'other-window)
(global-set-key (kbd "C-d") 'switch-to-buffer)
(global-set-key (kbd "C-f") 'delete-window)
(global-set-key (kbd "C-t") 'outline-toggle-children)
(setq inhibit-splash-screen t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(if (getenv "TMUX")
  (progn
	(let ((x 2) (tkey ""))
	  (while (<= x 8)
			 (if (= x 2) (setq tkey "S-"))
			 (if (= x 3) (setq tkey "M-"))
			 (if (= x 4) (setq tkey "M-S-"))
			 (if (= x 5) (setq tkey "C-"))
			 (if (= x 6) (setq tkey "C-S-"))
			 (if (= x 7) (setq tkey "C-M-"))
			 (if (= x 8) (setq tkey "C-M-S-"))
			 (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
			 (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
			 (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
			 (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
			 (setq x (+ x 1))))))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(require 'move-text)
(move-text-default-bindings)

(setq auto-save-interval 100)
(setq neo-theme 'ascii)
(setq neo-keymap-style 'concise)
(setq neo-window-width 35)
(setq neo-smart-open t)

(defvar k-auto-save nil)

(defun k-save-buffer-if-visiting-file (&optional args)
  "Save buffer on auto-save"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))

(when k-auto-save
  (add-hook 'auto-save-hook 'k-save-buffer-if-visiting-file :local t))

(setq neo-archive-tmpl "${d}.archive/${f}.%y%m%d")
(setq ttt-auto-hide-pattern "^\\S-.*;$")
(require 'ttt)
(add-to-list 'auto-mode-alist 
	     '("\\.ttt\\(\\.[0-9]+\\)?$" . ttt-minor-mode))
(global-set-key (kbd "C-M-t") 'ttt-hide-marked)
