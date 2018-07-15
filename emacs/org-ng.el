(defvar k-org-capture-inbox "inbox/m_desktop.org")
(defvar k-org-capture-main-id "Main_Journal")
(defvar k-org-capture-journal "journal.org")
(defvar k-org-auto-open-agenda-key nil)
(defvar k-org-goto-zero t)
(defvar k-org-goto-narrow nil)
(defvar k-org-agenda-filter nil)

; Files and folders
(setq org-agenda-files
      (list org-directory
	    (concat org-directory "calendar")
	    (concat org-directory "inbox")
	    (concat org-directory "projects")))

(setq org-agenda-file-regexp "^[a-z].*\.org$")

; Custom org configuration
(setq org-modules (quote (org-info org-habit org-timer)))
(setq org-log-into-drawer nil)
(setq org-log-done 'time)

; Habits
(setq org-habit-done-word "#")
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-preceding-days 7)
(setq org-habit-following-days 2)
(setq org-habit-graph-column 40)

; Custom keywords
(setq org-todo-keywords
      '((sequence "T(t)" "N(n)" "|" "A(a)" "#(d)")))

; Refile targets - two levels in current file + first level in others
(setq org-refile-targets (quote (
	(nil :maxlevel . 3)
	(org-agenda-files :maxlevel . 2)
)))

(require 'org)
(require 'cl)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Custom speed commands
(setq org-speed-commands-user
      '(
	("c" . org-schedule)
	("y" . org-deadline)
	("s" . org-save-all-org-buffers)
	("z" . org-narrow-to-subtree)
	("x" . widen)
	("q" . org-cycle)
	("w" . org-global-cycle)
	("o" . delete-other-windows)
	("." . (lambda ()
		 (org-agenda nil "w")))
	("e" . (lambda ()
		 (org-agenda nil "t" 'buffer)))
	("k" . org-capture)))
(setq org-use-speed-commands t)

; Custom agenda view
(setq org-agenda-custom-commands 
      '(("w" "Main" ((agenda
		      "Today"
		      ((org-agenda-overriding-header "Today")
		       (org-agenda-span 'day)
		       (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))))
		     (tags-todo
		      "-ALLTAGS=\"\""
		      ((org-agenda-overriding-header (format "%s / %s" "Next " k-org-agenda-filter))
		       (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
		       (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
		     (tags-todo
		      "-ALLTAGS<>\"\""
		      ((org-agenda-overriding-header (format "%s / %s" "Inbox " k-org-agenda-filter))
		       (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up)))))
	 ((org-agenda-tag-filter-preset k-org-agenda-filter)
	  (org-agenda-compact-blocks t)))
	("r" "All TODOs" ((todo
			   ""
			   ((org-agenda-dim-blocked-tasks t)
			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))))
	("c" "Closed" ((tags
			"+CLOSED<\"<-3d>\""
			((org-agenda-dim-blocked-tasks nil)))))))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)

; Compact/narrow agenda view
(setq org-agenda-prefix-format
      '((agenda  . "%-10:c%?-12t% s")
	(timeline  . "% s")
	(todo  . "%-10:c")
	(tags  . "%-10:c")
	(search . "%-10:c")))
(setq org-agenda-current-time-string "<-")
;(setq org-agenda-time-grid '((daily today)
;			     (1000 1200 1400 1600 1800 2000)
;			     ". . . ."
;			     "-------"))
(setq org-agenda-entry-text-maxlines 20)

; Capture to entry set by k-org-capture-main-id
(defun k-org-capture-main ()
  (pcase (org-id-find k-org-capture-main-id)
    (`(,path . ,pos)
     (set-buffer (org-capture-target-buffer path))
     (widen)
     (org-capture-put-target-region-and-position)
     (goto-char pos))
    (_ (error "Capture target id not found: '%s'" k-org-capture-main-id))))

; Custom capture templates
(setq org-capture-templates
      '(
	("p" "Todo" entry
	 (function (lambda ()
		     (k-org-capture-main)))
	 "* T %?")
	("j" "Journal entry" entry
	 (file+datetree (concat org-directory k-org-capture-journal))
	 "* %U %?")))

; Enable git push on save
(when (> k-org-git-save-push-sec 0)
  (message "Git: Will auto-push on save")
  (org-add-hook 'after-save-hook 'k-org-git-auto-save))

; Translate org ID to rfloc
(defun k-org-id-to-rfloc (id)
  (when id
    (let ((marker (org-id-find id)))
      (when marker
	(list id (car marker) id (cdr marker))))))

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
		   (org-agenda-redo)))
     (org-defkey org-agenda-mode-map "c" 'org-agenda-schedule)
     (org-defkey org-agenda-mode-map "h" 'k-org-git-dispatcher)
     (org-defkey org-agenda-mode-map "y" 'org-agenda-deadline)
     (org-defkey org-agenda-mode-map "i" 'org-agenda-refile)
     (org-defkey org-agenda-mode-map "p" 
		 (lambda () 
		   (interactive) 
		   (org-capture nil "p")))))

; Capture window custom shortcuts
(add-hook 'org-capture-mode-hook
  '(lambda ()
     (org-defkey org-capture-mode-map "\\"
		 (lambda () 
		   (interactive) 
		   (org-capture-finalize nil)))
     (org-defkey org-capture-mode-map "|" 'org-capture-kill)))
(setq org-confirm-babel-evaluate nil)

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

; End
