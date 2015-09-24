(setq mark-diary-entries-in-calendar nil)
(setq org-agenda-files (directory-files org-directory t "^[a-z].*\.org$"))
(setq org-agenda-include-all-todo nil)
(setq org-agenda-include-diary nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-deadline-warning-days 2)
(setq org-agenda-skip-scheduled-if-done t)
;(setq org-popup-calendar-for-date-prompt nil) ; Add to mobile
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-startup-folded t)
(setq org-use-property-inheritance t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;(setq org-agenda-span 3)
(setq org-agenda-start-on-weekday nil)
;(setq org-agenda-scheduled-leaders '("S:", "Sched.%2dx:"))
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
(defvar k-org-capture-inbox "main.org")
(setq org-todo-keywords
           '((sequence "T(t)" "N(n)" "?(w@/@)" "|" "A(a)" "#(d)" "X(x@)")
	         (sequence "H(h)" "|" "M(m)")
	        )
)
(setq org-refile-use-outline-path file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets (quote (
	(nil :maxlevel . 9)
	(org-agenda-files :level . 1)
)))
(setq org-refile-allow-creating-parent-nodes "confirm")
(setq org-todo-keyword-faces '(
	("N" :foreground "brightblue" :weight: bold)
	("?" :foreground "white" :weight: bold)
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
(setq org-agenda-current-time-string ">> - - - - - - -")
(setq org-use-speed-commands t)

(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-custom-commands 
      '(
	("w" "Office"
	 (
	  (agenda "Today" (
			   (org-agenda-overriding-header "Today")
			   (org-agenda-ndays 1)
			   (org-agenda-sorting-strategy 
			    '(habit-down time-up todo-state-up priority-down))))
	  (alltodo "Tasks" (
			    (org-agenda-overriding-header "Tasks")
			    (org-agenda-files (directory-files org-directory t "^m.*\.org$"))
			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
	  (todo "N" (
		     (org-agenda-overriding-header "Next tasks")
		     (org-agenda-files (directory-files org-directory t "^p_.*\.org$"))
		     (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))) 
	 ((org-agenda-compact-blocks t)))))

(setq org-agenda-todo-ignore-scheduled t)
(defvar k-org-capture-inbox "main.org")
(setq org-capture-templates
	'(
	  ("n" "Note" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* # %? %T")
	  ("p" "Todo" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* T %?")
	  ("t" "Todo (Schedule)" entry (file+headline (concat org-directory k-org-capture-inbox) "Journal") "* T %?\n  SCHEDULED: %^t")
	  )
	)

(setq org-hide-block-startup t)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(setq org-agenda-window-setup 'current-window)
(setq org-clone-delete-id t)
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (org . t)
         (plantuml . t))
 )
)
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
;(setq org-plantuml-jar-path "c:/home/download/plantuml.jar")
(setq org-babel-results-keyword "results")

(add-hook 'org-mode-hook
  '(lambda ()
     
     (org-defkey org-mode-map "\M-p" 'org-mpw-password)
     (org-defkey org-mode-map "\M-o" 
		 (lambda()
		   (interactive)
		   (org-mpw-password t)))
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
		   (org-agenda-exit)
		   (save-buffers-kill-terminal)))
     (org-defkey org-agenda-mode-map "D" 'org-agenda-kill)
     (org-defkey org-agenda-mode-map "p" 
		 (lambda () 
		   (interactive) 
		   (org-capture nil "p")))
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
(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (if k-org-auto-open-agenda-key
		 (org-agenda nil k-org-auto-open-agenda-key))))
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
	    (auto-save-mode)))
(add-hook 'org-agenda-after-show-hook
	  (lambda ()
	    (if k-org-goto-zero
		(goto-char (point-at-bol)))))

(setq org-mpw-name "Konstantin")

(require 'org-mpw)
