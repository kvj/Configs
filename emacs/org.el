(setq mark-diary-entries-in-calendar nil)
(setq org-agenda-files
      (list org-directory
	    (concat org-directory "calendar")
	    (concat org-directory "inbox")
	    (concat org-directory "knowledge")))
(setq org-agenda-file-regexp "^[a-z].*\.org$")
(setq org-agenda-include-all-todo nil)
(setq org-agenda-include-diary nil)
(setq org-deadline-warning-days 2)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-startup-folded t)
(setq org-use-property-inheritance t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-start-on-weekday nil)
(setq org-hierarchical-todo-statistics t)
(setq org-enforce-todo-dependencies nil)
(setq org-enforce-todo-checkbox-dependencies nil)
(setq org-log-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-drawers '("PROPERTIES" "CLOCK" "PATH" "LOGBOOK"))
(setq org-habit-done-word "M")
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-modules (quote (org-info org-habit org-timer)))
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-preceding-days 7)
(setq org-habit-following-days 2)
(setq org-habit-graph-column 43)
(defvar k-org-capture-inbox "inbox/m_desktop.org")
(defvar k-org-capture-inbox-main "main.org")
(defvar k-org-agenda-refile-id nil)
(setq k-org-agenda-refile-id "Main_Journal")
(setq org-todo-keywords
      '((sequence "T(t)" "N(n)" "W(w@/@)" "|" "A(a)" "#(d)" "X(x@)")
	(sequence "H(h)" "|" "M(m@)")))
(setq org-refile-use-outline-path file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets (quote (
	(nil :maxlevel . 2)
	(org-agenda-files :level . 1)
)))
(setq org-refile-allow-creating-parent-nodes "confirm")
(setq org-todo-keyword-faces '(
	("N" :foreground "brightblue" :weight: bold)
	("W" :foreground "white" :weight: bold)
	("A" :foreground "magenta" :weight: bold)
	("X" :foreground "gray" :weight: bold)
	("H" :foreground "green")
))

(setq org-capture-use-agenda-date nil)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-use-tag-inheritance nil)
(setq org-tags-exclude-from-inheritance '("pin"))
(setq org-agenda-start-with-log-mode nil)
(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-on-weekday 1)
(setq org-cycle-separator-lines 0)
(setq org-catch-invisible-edits 'error)
(setq org-agenda-current-time-string "> - - -")
(setq org-agenda-time-grid '(
			     (daily today)
			     ". . . ."
			     (1000 1200 1400 1600 1800 2000)))
(setq org-agenda-entry-text-maxlines 20)
(setq org-use-speed-commands t)
(setq org-return-follows-link t)

(require 'org)
(require 'cl)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-speed-commands-user
      '(
	("c" . org-schedule)
	("y" . org-deadline)
	("s" . org-save-all-org-buffers)
	("z" . org-narrow-to-subtree)
	("x" . widen)
	("q" . org-cycle)
	("." . (lambda ()
		 (org-agenda nil "w")))
	("e" . (lambda ()
		 (when k-org-auto-open-agenda-key
		   (org-agenda nil k-org-auto-open-agenda-key))))
	("k" . org-capture)))

(setq org-agenda-custom-commands 
      '(
	("w" "Office"
	 (
	  (agenda "Today" (
			   (org-agenda-overriding-header "Today")
			   (org-agenda-ndays 1)
			   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))))
	  (todo "" (
		    (org-agenda-overriding-header "Tasks")
		    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
		    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
	  (tags "+pin" (
			(org-agenda-overriding-header "Pin")
			(org-agenda-remove-tags t)
			(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up)))))
	 ((org-agenda-compact-blocks t)))
	("a" "Tasks" (
		      (todo "" (
				(org-agenda-overriding-header "Tasks")
				(org-agenda-files (list (concat org-directory "main.org")))
				(org-agenda-sorting-strategy '(category-up todo-state-down priority-down effort-up))))))
	("c" "Closed" ((tags "+CLOSED<\"<-3d>\"")))))

(setq org-agenda-prefix-format
      '((agenda  . "%-10:c%?-12t% s")
	(timeline  . "% s")
	(todo  . "%-22:c")
	(tags  . "%-22:c")
	(search . "%-10:c")))
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(setq org-capture-templates
      '(
	("k" "Schedule smth." entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* %? %^t")
	("n" "Note" entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* # %? %T")
	("p" "Todo" entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* T %?")
	("m" "Note (backup inbox)" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* # %? %T")
	("i" "Todo (backup inbox)" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* T %?")
	("t" "Todo (Schedule)" entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* T %?\n  SCHEDULED: %^t")
	("e" "Event" entry (file+headline (concat org-directory k-org-capture-inbox-main) "Journal") "* A %? %^t")))

(setq org-hide-block-startup t)
(setq org-clock-persist nil)
(org-clock-persistence-insinuate)

(setq org-agenda-window-setup 'current-window)
(setq org-clone-delete-id t)
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (org . t)
         (plantuml . t))))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
;(setq org-plantuml-jar-path "c:/home/download/plantuml.jar")
(setq org-babel-results-keyword "results")

(defvar k-org-git-branch "master")
(defvar k-org-git-auto-push-min 0)
(defun k-org-git (cmd msg dir)
  "Dispatch git pull/push etc commands."
  (when msg
    (message msg))
  (message (shell-command-to-string
	    (concat
	     "GIT_DIR=" dir ".git"
	     " "
	     "GIT_WORK_TREE=" dir
	     " "
	     cmd))))

(defun k-org-git-pull ()
  (k-org-git (concat
	      "git pull --ff-only --no-edit origin"
	      " " k-org-git-branch) "Pulling from git..." org-directory)
  (run-with-idle-timer 5 nil 'org-agenda-redo t))

(defun k-org-git-pull-config ()
  (k-org-git
   "git pull --ff-only --no-edit origin master"
   "Pulling config from git..."
   (concat config-dir "../")))

(defun k-org-git-push ()
  (org-save-all-org-buffers)
  (k-org-git "git commit -a -m \"`date` - `hostname`\"" nil org-directory)
  (k-org-git (concat
	      "git push origin"
	      " " k-org-git-branch) "Pushing to git..." org-directory))

(defun k-org-git-dispatcher ()
  (interactive)
  (message "Git: ([h] pull/[j] push/[c] pull config/[q] quit)")
  (let ((a (read-char-exclusive)))
    (case a
	  (?j (run-with-idle-timer 5 nil 'k-org-git-push))
	  (?h (k-org-git-pull))
	  (?c (k-org-git-pull-config))
	  (?q (message "Abort"))
	  (otherwise (error "Invalid key")))))

(defun k-org-id-to-rfloc (id)
  (when id
    (let ((marker (org-id-find id)))
      (when marker
	(list id (car marker) id (cdr marker))))))
(defun k-org-copy-refile (refile-keep)
  (let ((rfloc (k-org-id-to-rfloc k-org-agenda-refile-id)) (org-refile-keep refile-keep))
    (if rfloc
	(org-agenda-refile nil rfloc)
      (message "k-org-agenda-refile-id is invalid ID"))))
(add-hook 'org-mode-hook
  '(lambda ()
     (org-defkey org-mode-map "\M-;" 'org-timer-start)
     (org-defkey org-mode-map "\M--" 'org-timer-item)
     (org-defkey org-mode-map "\M-:" 
		 (lambda()
		   (interactive)
		   (org-timer-pause-or-continue t)))))

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
(add-hook 'org-capture-mode-hook
  '(lambda ()
     (org-defkey org-capture-mode-map "\\"
		 (lambda () 
		   (interactive) 
		   (org-capture-finalize nil)))
     (org-defkey org-capture-mode-map "|" 'org-capture-kill)))
(setq org-confirm-babel-evaluate nil)
(defvar k-org-auto-open-agenda-key nil)
(defvar k-org-goto-zero nil)
(defvar k-org-goto-narrow nil)
(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (when k-org-auto-open-agenda-key
	       (org-agenda nil k-org-auto-open-agenda-key))
	     (when (> k-org-git-auto-push-min 0)
	       (run-at-time (* k-org-git-auto-push-min 60) (* k-org-git-auto-push-min 60)
			    (lambda ()
			      (run-with-idle-timer 30 nil 'k-org-git-push))))))
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
	    (auto-save-mode)))
(add-hook 'org-agenda-after-show-hook
	  (lambda ()
	    (when k-org-goto-zero
	      (org-back-to-heading t)
	      (goto-char (point-at-bol)))
	    (when k-org-goto-narrow
	      (org-narrow-to-subtree))))
