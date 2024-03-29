(defvar k-org-auto-open-agenda-key nil)
(defvar k-org-goto-zero t)
(defvar k-org-goto-narrow t)
(defvar k-org-agenda-single-line t)
(defvar k-org-compact-agenda nil)
(defvar k-org-mobile t)

; Files and folders
(setq org-agenda-files
      (list org-directory))

(setq org-agenda-file-regexp "^[a-z].*\.org$")

; Custom org configuration
(setq org-modules (quote (org-info org-timer)))
(setq org-log-into-drawer t)
(setq org-log-done 'time)

; Habits
;(setq org-habit-done-word "#")
;(setq org-habit-show-habits-only-for-today nil)
;(setq org-habit-preceding-days 7)
;(setq org-habit-following-days 2)
;(setq org-habit-graph-column 40)

; Custom keywords
;(setq org-todo-keywords
;      '((sequence "N(n)" "T(t)" "H(h)" "X(x)" "|" "A(a)" "#(d)")))

; Refile targets - two levels in current file + first level in others
(setq org-refile-targets (quote (
	(nil :maxlevel . 3)
	(org-agenda-files :maxlevel . 2)
)))

(require 'org)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key "\C-cc" 'org-capture)

; Custom speed commands
(setq org-speed-commands-user
      '(
	("c" . org-schedule)
	("y" . org-deadline)
	("s" . org-save-all-org-buffers)
	("z" . org-narrow-to-subtree)
	("x" . widen)
	("q" . org-cycle)
        ("X" . org-clock-cancel)
	("w" . org-global-cycle)
	("o" . delete-other-windows)
	("." . (lambda ()
		 (org-agenda nil "w")))
	("k" . org-capture)))
(setq org-use-speed-commands t)

; Custom agenda view
(setq org-agenda-custom-commands 
      '(("w" "Main" ((agenda "Today"
			     ((org-agenda-overriding-header "Today")
			      (org-agenda-span 'day)
			      (org-agenda-sorting-strategy '(time-up priority-down))))
		     (tags-todo "+PRIORITY=\"A\"+BLOCKED=\"\""
			   ((org-agenda-overriding-header "Focus")
			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
			    (org-agenda-sorting-strategy '(priority-down effort-up)))))
	 ((org-agenda-compact-blocks t)))
	("q" "All TODOs" ((todo "TODO"
				((org-agenda-overriding-header "All TODOs")
				 (org-agenda-sorting-strategy '(priority-down)))))
	 ((org-agenda-compact-blocks t)))))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-deadline-warning-days 0)
(setq org-scheduled-delay-days 0)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-use-tag-inheritance t)

; Compact/narrow agenda view
(setq org-agenda-prefix-format
      '((agenda  . "%-10:c%?-12t% s")
	(timeline  . "% s")
	(todo  . "%-10:c")
	(tags  . "%-10:c")
	(search . "%-10:c")))
(when k-org-compact-agenda
  (setq org-agenda-prefix-format
	'((agenda  . "  %?-12t% s")
	  (timeline  . "% s")
	  (todo  . "  ")
	  (tags  . "  ")
	  (search . "  ")))
  )
(setq org-agenda-current-time-string "<-")
(setq org-agenda-time-grid '((daily today)
			     (0800 1200 1600 2000)
			     ". . . ."
			     "-------"))
;(setq org-agenda-entry-text-maxlines 20)

; Custom capture templates
(setq k-org-capture-loc
      (list 'file+headline (concat org-directory "main.org") "Inbox"))
(setq k-org-capture-journal-loc
      (list 'file+headline (concat org-directory "main.org") "Journal"))
(setq org-capture-templates
      (list
       (list "p" "Todo" 'entry k-org-capture-loc
	     "** TODO %?")
       (list "a" "[#A] Todo" 'entry k-org-capture-loc
	     "** TODO [#A] %?")
       (list "j" "Journal entry" 'entry k-org-capture-journal-loc
	     "** %? %T")))

(when k-org-mobile
  (setq org-mobile-directory (concat org-directory ".mobile/"))
  (setq org-mobile-inbox-for-pull (concat org-directory "mobile.org")))

; Enable git push on save
;(when (> k-org-git-save-push-sec 0)
;  (message "Git: Will auto-push on save")
;  (org-add-hook 'after-save-hook 'k-org-git-auto-save))

; Translate org ID to rfloc
;(defun k-org-id-to-rfloc (id)
;  (when id
;    (let ((marker (org-id-find id)))
;      (when marker
;	(list id (car marker) id (cdr marker))))))

; Custom shortcuts
(add-hook 'org-mode-hook
  '(lambda ()
     (org-defkey org-mode-map "\M-;" 'org-timer-start)
     (org-defkey org-mode-map "\M--" 'org-timer-item)
     (org-defkey org-mode-map "\M-:" 
		 (lambda()
		   (interactive)
		   (org-timer-pause-or-continue t)))))


; Custom agenda shortcuts
; Not used: g, G, n
(add-hook 'org-agenda-mode-hook
  '(lambda ()
     (org-defkey org-agenda-mode-map "x" 
		 (lambda () 
		   (interactive) 
		   (org-save-all-org-buffers)
		   (org-agenda-exit)
		   (save-buffers-kill-terminal)))
     (org-defkey org-agenda-mode-map "D" 'org-agenda-kill)
     (org-defkey org-agenda-mode-map "r"
		 (lambda () 
		   (interactive)
		   (org-save-all-org-buffers)
		   (org-agenda-redo t)))
     (org-defkey org-agenda-mode-map "k"
		 (lambda ()
		   (interactive)
		   (org-agenda-capture 1)))
     (org-defkey org-agenda-mode-map "K" 'org-agenda-capture)
     (org-defkey org-agenda-mode-map "A" 'org-agenda)
     ;(org-defkey org-agenda-mode-map "h" 'k-org-git-dispatcher)
     (org-defkey org-agenda-mode-map "c" 'org-agenda-schedule)
     (org-defkey org-agenda-mode-map "y" 'org-agenda-deadline)
     (org-defkey org-agenda-mode-map "i" 'org-agenda-refile)
     (org-defkey org-agenda-mode-map "p" 
		 (lambda () 
		   (interactive) 
		   (org-capture nil "p")))))

; Capture window custom shortcuts
;(add-hook 'org-capture-mode-hook
;  '(lambda ()
;     (org-defkey org-capture-mode-map "\\"
;		 (lambda () 
;		   (interactive) 
;		   (org-capture-finalize nil)))
;     (org-defkey org-capture-mode-map "|" 'org-capture-kill)))

; Auto-open agenda view
(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (when k-org-auto-open-agenda-key
	       (org-agenda nil k-org-auto-open-agenda-key))))

; Auto-save org files
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
	    (auto-save-mode)))

; Cursor position and window size
(add-hook 'org-agenda-after-show-hook
	  (lambda ()
	    (when k-org-goto-zero
	      (org-back-to-heading t)
	      (goto-char (point-at-bol)))
	    (when k-org-goto-narrow
	      (org-narrow-to-subtree))))

; Force single line agenda
(defun k-org-agenda-make-single-line (width)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\(.+?\\)\\(:[[:alnum:]_@#%:]+:\\)?\\)[ \t]*$" nil t)
      (let ((l (- (match-end 1) (match-beginning 1))))
	(when (> l width)
	  (goto-char (match-end 2))
	  (delete-region (- (match-end 2) (- l width) 3) (match-end 2))
	  (insert "..."))))))

; Make single lines in agenda buffer
(add-hook 'org-agenda-finalize-hook
	  (lambda ()
	    (when (and (< org-agenda-tags-column 0) k-org-agenda-single-line)
	      (k-org-agenda-make-single-line (- org-agenda-tags-column)))))

; Refresh agenda after capture
(add-hook 'org-capture-after-finalize-hook
	  (lambda ()
	    (org-agenda-redo t)))

; End
