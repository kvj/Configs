(setq mark-diary-entries-in-calendar t)
(setq org-agenda-files (directory-files org-directory t ".*.org$"))
(setq org-agenda-include-all-todo nil)
(setq org-agenda-include-diary nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-deadline-warning-days 2)
(setq org-agenda-skip-scheduled-if-done t)
;(setq org-blank-before-new-entry (quote ((heading . auto) (plain-list-item))))
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-startup-folded t)
(setq org-use-property-inheritance t)
(setq org-log-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-span 3)
(setq org-agenda-start-on-weekday nil)
(setq org-hierarchical-todo-statistics t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-into-drawer nil)
(setq org-drawers '("PROPERTIES" "CLOCK" "PATH"))
(setq org-habit-done-word "MADE")
(setq org-modules (quote (org-docview org-info org-habit org-mew org-mhe org-timer)))
(defvar k-org-capture-inbox "main.org")
(setq org-todo-keywords
           '((sequence "TODO(t)" "WAIT(w@/@)" "|" "DONE(d@/@)" "CANCEL(c@)")
             (sequence "|" "APPT(a)" "NOTE(n)" "JOURN(j)")
	         (sequence "HABIT(h)" "|" "MADE(m)")
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
	("TODO" . org-warning) 
	("WAIT" :foreground "gray" :weight: bold)
	("APPT" :foreground "magenta" :weight: bold)
        ("CANCEL" :foreground "forest green" :weight: bold)
        ("HABIT" :foreground "blue")
))

(setq org-capture-use-agenda-date nil)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-use-tag-inheritance nil)
(setq org-agenda-start-with-log-mode t)
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;(setq org-default-notes-file (concat org-directory "zinbox.org"))

(setq org-agenda-custom-commands '(
	("w" "Office"
		(
			(agenda "Today" (
				(org-agenda-overriding-header "Today")
				(org-agenda-ndays 1)
				(org-agenda-sorting-strategy '(habit-down time-up todo-state-up priority-down))
			))
			(alltodo "Other tasks" (
				(org-agenda-overriding-header "Other tasks")
				(org-agenda-files (list (concat org-directory "main.org")))
				(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
			))
			(alltodo "Inbox" (
				(org-agenda-overriding-header "Inbox")
				(org-agenda-files (directory-files org-directory t "i_.*.org$"))
				(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
			))
		) (
			(org-agenda-compact-blocks t)
		)
	)
))
(setq org-agenda-todo-ignore-scheduled t)
(defvar k-org-capture-inbox "main.org")
(setq org-capture-templates
	'(
		("t" "Todo" entry (file+headline (concat org-directory "main.org") "Calendar") "* TODO %?")
		("a" "Appontment" entry (file+headline (concat org-directory "main.org") "Calendar") "* APPT %?")
		;("b" "Bug" entry (file+headline (concat org-directory "main.org") "Journal") "* BUG %?\n  %u")
		("n" "Note" entry (file+headline (concat org-directory "main.org") "Journal") "* %T %?")
		("p" "Todo (inbox)" entry (file+headline (concat org-directory k-org-capture-inbox) "Inbox") "* TODO %?")
		;("j" "Journal" entry (file+datetree (concat org-directory "journal.org")) "* JOURN %<%H:%M> %?")
	)
)

(setq org-hide-block-startup t)
(setq org-clock-persist t)
;(require 'org-crypt)

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
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(setq org-babel-results-keyword "results")

(add-hook 'org-mode-hook
  '(lambda ()
   (setq org-file-apps
     (append '(("\\.png\\'" . default)) org-file-apps ))))

(add-hook 'org-agenda-mode-hook
  '(lambda ()
	(org-defkey org-agenda-mode-map "x" 
				(lambda () 
				  (interactive) 
				  (org-agenda-exit)
				  (save-buffers-kill-terminal)))
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
	(org-defkey org-capture-mode-map "|"
				(lambda () 
				  (interactive) 
				  (org-capture-kill)))))
(setq org-confirm-babel-evaluate nil)
(defvar k-org-auto-open-agenda-key nil)
(add-hook 'emacs-startup-hook
		  '(lambda ()
			 (if k-org-auto-open-agenda-key
			   (org-agenda nil k-org-auto-open-agenda-key))))
(add-hook 'org-agenda-mode-hook
		  (lambda ()
			(add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
			(auto-save-mode)))
