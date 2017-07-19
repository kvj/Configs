(defvar k-org-capture-inbox "inbox/m_desktop.org")
(defvar k-org-capture-inbox-main "main.org")
(defvar k-org-agenda-refile-id "Main_Journal")
(defvar k-org-auto-open-agenda-key nil)
(defvar k-org-goto-zero t)
(defvar k-org-goto-narrow nil)

; Files and folders
(setq org-agenda-files
      (list org-directory
	    (concat org-directory "calendar")
	    (concat org-directory "inbox")))

(setq org-agenda-file-regexp "^[a-z].*\.org$")

; Custom org configuration
(setq org-log-into-drawer t)
(setq org-log-done 'time)

; Custom keywords
(setq org-todo-keywords
      '((sequence "T(t)" "|" "A(a)" "#(d)")))

; Refile targets - two levels in current file + first level in others
(setq org-refile-targets (quote (
	(nil :maxlevel . 2)
	(org-agenda-files :level . 1)
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
		 (when k-org-auto-open-agenda-key
		   (org-agenda nil k-org-auto-open-agenda-key))))
	("k" . org-capture)))
(setq org-use-speed-commands t)

; Custom agenda view
(setq org-agenda-custom-commands 
      '(
	("w" "Main"
	 (
	  (agenda "Today" (
			   (org-agenda-overriding-header "Today")
			   (org-agenda-ndays 1)
			   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))))
	  (todo "" (
		    (org-agenda-overriding-header "Tasks")
		    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
		    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up)))))
	 ((org-agenda-compact-blocks t)))
	("c" "Closed" ((tags "+CLOSED<\"<-3d>\"")))))
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

; Compact/narrow agenda view
(setq org-agenda-prefix-format
      '((agenda  . "%-10:c%?-12t% s")
	(timeline  . "% s")
	(todo  . "%-10:c")
	(tags  . "%-10:c")
	(search . "%-10:c")))
(setq org-agenda-current-time-string "> - - -")
(setq org-agenda-time-grid '(
			     (daily today)
			     ". . . ."
			     (1000 1200 1400 1600 1800 2000)))
(setq org-agenda-entry-text-maxlines 20)

; Custom capture templates
(setq org-capture-templates
      '(
	("n" "Note" entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* # %? %T")
	("p" "Todo" entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* T %?")
	("m" "Note (backup inbox)" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* # %? %T")
	("i" "Todo (backup inbox)" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* T %?")))

; Integration with Git
(defvar k-org-git-branch "master")
(defvar k-org-git-auto-push-min 0)
(defvar k-org-git-save-push-sec 20)
(defvar k-org-git-save-push-timer nil)

(defun k-org-git-auto-save ()
  (when k-org-git-save-push-timer
    (cancel-timer k-org-git-save-push-timer))
  (message "Git: Will auto-push")
  (setq k-org-git-save-push-timer
	(run-with-idle-timer
	 k-org-git-save-push-sec nil (lambda()
				       (k-org-git-push)
				       (setq k-org-git-save-push-timer nil)))))

(when (> k-org-git-save-push-sec 0)
  (message "Git: Will auto-push on save")
  (org-add-hook 'after-save-hook 'k-org-git-auto-save))

(defun k-org-git (cmd msg dir)
  "Dispatch git pull/push etc commands."
  (when msg
    (message msg))
  (= (call-process-shell-command
      (concat
       "GIT_DIR=" dir ".git"
       " "
       "GIT_WORK_TREE=" dir
       " "
       cmd) nil "*scratch*") 0))

(defun k-org-git-pull ()
  (if (k-org-git (concat
		  "git pull --no-edit origin"
		  " " k-org-git-branch) "Git: Pulling..." org-directory)
      (run-with-idle-timer 5 nil 'org-agenda-redo t))
  (message "Git: No changes received"))

(defun k-org-git-pull-config ()
  (k-org-git
   "git pull --no-edit origin master"
   "Git: Pulling config..."
   (concat config-dir "../")))

(defun k-org-git-commit (msg)
  (org-save-all-org-buffers)
  (k-org-git "git commit -a -m \"`date` - `hostname`\"" msg org-directory))

(defun k-org-git-reset ()
  (org-save-all-org-buffers)
  (when (k-org-git "git reset --hard HEAD" "Reverting changes back" org-directory)
    (org-agenda-redo t)))

(defun k-org-git-push (&optional force)
  (if (or (k-org-git-commit nil) force)
      (let ()
	(k-org-git (concat
		    "git pull --no-edit origin"
		    " " k-org-git-branch) nil org-directory)
	(k-org-git (concat
		    "git push origin"
		    " " k-org-git-branch) "Git: Pushing..." org-directory)
	(message "Git: Pushed"))
    (message "Git: No changes to push")))

(defun k-org-git-dispatcher ()
  (interactive)
  (message "Git: ([h] pull/[j] push/[c] commit/[r] reset/[s] pull config/[q] quit)")
  (let ((a (read-char-exclusive)))
    (case a
	  (?j (run-with-idle-timer 5 nil 'k-org-git-push t))
	  (?h (k-org-git-pull))
	  (?s (k-org-git-pull-config))
	  (?c (k-org-git-commit "Saving changes..."))
	  (?r (k-org-git-reset))
	  (?q (message "Abort"))
	  (otherwise (error "Invalid key")))))

; Translate org ID to rfloc
(defun k-org-id-to-rfloc (id)
  (when id
    (let ((marker (org-id-find id)))
      (when marker
	(list id (car marker) id (cdr marker))))))

; Copy/move selected outline to Journal
(defun k-org-copy-refile (refile-keep)
  (let ((rfloc (k-org-id-to-rfloc k-org-agenda-refile-id)) (org-refile-keep refile-keep))
    (if rfloc
	(org-agenda-refile nil rfloc)
      (message "k-org-agenda-refile-id is invalid ID"))))

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
(add-hook 'org-agenda-mode-hook
  '(lambda ()
     (org-defkey org-agenda-mode-map "x" 
		 (lambda () 
		   (interactive) 
		   (org-save-all-org-buffers)
		   (org-agenda-exit)
		   (save-buffers-kill-terminal)))
     (org-defkey org-agenda-mode-map "D" 'org-agenda-kill)
     (org-defkey org-agenda-mode-map "g" 
		 (lambda () 
		   (interactive)
		   (k-org-copy-refile t)))
     (org-defkey org-agenda-mode-map "G"
		 (lambda () 
		   (interactive)
		   (k-org-copy-refile nil)))
     (org-defkey org-agenda-mode-map "r"
		 (lambda () 
		   (interactive)
		   (org-save-all-org-buffers)
		   (org-agenda-redo)))
     (org-defkey org-agenda-mode-map "c" 'org-agenda-schedule)
     (org-defkey org-agenda-mode-map "h" 'k-org-git-dispatcher)
     (org-defkey org-agenda-mode-map "y" 'org-agenda-deadline)
     (org-defkey org-agenda-mode-map "p" 
		 (lambda () 
		   (interactive) 
		   (org-capture nil "p")))
     (org-defkey org-agenda-mode-map "i" 
		 (lambda () 
		   (interactive) 
		   (org-capture nil "i")))
     (org-defkey org-agenda-mode-map "n" 
		 (lambda () 
		   (interactive) 
		   (org-capture nil "n")))))

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
	       (org-agenda nil k-org-auto-open-agenda-key))
	     (when (> k-org-git-auto-push-min 0)
	       (run-at-time (* k-org-git-auto-push-min 60) (* k-org-git-auto-push-min 60)
			    (lambda ()
			      (run-with-idle-timer 30 nil 'k-org-git-push))))))

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
