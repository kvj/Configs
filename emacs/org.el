(setq mark-diary-entries-in-calendar t)
(setq org-agenda-files (directory-files org-directory t ".*.org$"))
;(setq org-agenda-files (list 
;			(concat org-directory "main.org") 
;			(concat org-directory "dates.org") 
;			(concat org-directory "zinbox.org"))
;)
(setq org-agenda-include-all-todo nil)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-deadline-warning-days 2)
(setq org-agenda-skip-scheduled-if-done t)
;(setq org-blank-before-new-entry (quote ((heading . auto) (plain-list-item))))
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
;(setq org-log-done 'note)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-startup-folded t)
(setq org-use-property-inheritance t)
(setq org-log-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-span 3)
(setq org-agenda-start-on-weekday nil)
;(setq org-startup-indented t)
;(setq org-display-custom-times t)
(setq org-hierarchical-todo-statistics t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-into-drawer nil)
(setq org-drawers '("PROPERTIES" "CLOCK" "PATH"))
(setq org-habit-done-word "MADE")
(setq org-modules (quote (org-crypt org-docview org-info org-habit org-mew org-mhe org-remember org-mobile org-timer)))
(setq org-todo-keywords
           '((sequence "BUG(b)" "TODO(t)" "REPORT(r@)" "WAIT(w@/@)" "|" "DONE(d@/@)" "CANCEL(c@)")
             (sequence "APPT(a)" "|" "FINISH(f@)" "CANCEL(c@)")
             (sequence "|" "NOTE(n)" "JOURN(j)")
	         (sequence "HABIT(h)" "|" "MADE(m)")
	        )
)
(setq org-refile-use-outline-path file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets (quote (
	(nil :maxlevel . 9)
	(org-agenda-files :level . 1)
	;("_journal.org" :level . 3)
)))
(setq org-refile-allow-creating-parent-nodes "confirm")
(setq org-todo-keyword-faces '(
	("TODO" . org-warning) 
	("BUG" . org-warning) 
	("WAIT" :foreground "gray" :weight: bold)
	("REPORT" :foreground "orange" :weight: bold)
	("APPT" :foreground "magenta" :weight: bold)
        ("CANCEL" :foreground "forest green" :weight: bold)
        ("HABIT" :foreground "blue")
))
(require 'org)
;(setq org-google-weather-format "%c %l%s - %h%s")
;(require 'org-google-weather)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cr" 'remember)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(org-remember-insinuate)

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
;(define-key global-map [(control meta ?r)] 'remember)

(setq org-default-notes-file (concat org-directory "zinbox.org"))

;; mobile org support
;(setq org-mobile-directory "~/MobileOrg/")
;(setq org-mobile-files (directory-files org-directory t "^[a-z].*.org$"))
;(setq org-mobile-inbox-for-pull (concat org-directory "zinbox.org"))
;(setq org-mobile-agendas (list "mm" "a"))
;(setq org-agenda-overriding-columns-format "%55ITEM %8TODO %14CLOSED")
;(setq org-mobile-files (quote ("main.org")))

(setq org-agenda-custom-commands '(
;				   ("w" todo "TODO")
;				   ("h" "Now" (
;				   		(tags-todo "+TODO=TODO")
;				   			(agenda (org-agenda-ndays 3) (org-agenda-show-all-dates nil)
;				   			)
;				   		)
;				   )
;				   ("W" agenda "Agenda 21" ((org-agenda-ndays 21)
;						   (org-agenda-show-all-dates nil)))
	("w" "Main"
		(
			(agenda "Today" (
				(org-agenda-overriding-header "Today")
				(org-agenda-ndays 1)
				(org-agenda-sorting-strategy '(habit-down time-up todo-state-up priority-down))
			))
			(alltodo "Other tasks" (
				(org-agenda-overriding-header "Other tasks")
				(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
			))
			(tags "+CATEGORY=\"Inbox\"" (
				(org-agenda-overriding-header "Inbox")
				(org-agenda-files (list (concat org-directory "zinbox.org")))
				(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
			))
		) (
;			(org-agenda-overriding-header "Today")
			(org-agenda-filter-preset '("-home"))
			(org-agenda-compact-blocks t)
		)
	)
	("mm" "Main"
		(
			(agenda "Agenda" (
				(org-agenda-overriding-header "Today")
				(org-agenda-ndays 1)
				(org-agenda-sorting-strategy '(habit-down time-up todo-state-up priority-down))
			))
			(alltodo "Other tasks" (
				(org-agenda-overriding-header "Other tasks")
				(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
			))
			(tags "+CATEGORY=\"Inbox\"" (
				(org-agenda-overriding-header "Inbox")
				(org-agenda-files (list (concat org-directory "zinbox.org")))
				(org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
			))
		) (
;			(org-agenda-overriding-header "Today")
			(org-agenda-compact-blocks t)
		)
	)
;	("w" agenda "Today"
;		((org-agenda-ndays 1)
;		(org-agenda-sorting-strategy '(todo-state-up priority-down effort-down))
;		(org-agenda-overriding-header "Today"))
;	)
	("r" "Review" 
		(
			(tags "+CLOSED<\"<-7d>\"" (
				(org-agenda-sorting-strategy '(todo-state-up))
				(org-agenda-overriding-header "Review")
			))
		)
	)
))
(setq org-agenda-todo-ignore-scheduled t)
(setq org-capture-templates
	'(
		("t" "Todo" entry (file+headline (concat org-directory "zinbox.org") "Inbox") "* TODO %?\n  %u")
		("a" "Appontment" entry (file+headline (concat org-directory "zinbox.org") "Inbox") "* APPT %?\n  %u")
		("b" "Bug" entry (file+headline (concat org-directory "zinbox.org") "Inbox") "* BUG %?\n  %u")
		;("j" "Journal" entry (file+datetree (concat org-directory "journal.org")) "* JOURN %<%H:%M> %?")
		;;("p" "Pinned" entry (file+datetree+prompt (concat org-directory "journal.org")) "* PIN %?")
	)
)

;(defun org-mobile-pullpush nil nil 
;  (interactive)
;  (message "MobileOrg pull&push...")
;  (org-mobile-pull)
;  (org-mobile-push)
;  (message "MobileOrg pull&push done")
;)

;(run-at-time "30 sec" 1800 'org-mobile-pullpush)

;(define-key global-map "\C-cm" 'org-mobile-pullpush)

;(custom-set-variables
; '(org-remember-store-without-prompt t)
; '(org-remember-templates
;   (quote (
;	("ToDo" ?t "* TODO %?\n  %u" "main.org" "Tasks")
;	("Note" ?n "* NOTE %?\n  %u")
;	("Appt" ?a "* APPT %?\n  %u" "main.org" "Tasks")
;	("Bug" ?b "* BUG %?\n  %u" "main.org" "Tasks")
;	("Journal" ?j "* JOURN %?\n  %U" (file+datetree+prompt "_journal.org"))
;	("Password" ?p "* %?\n  Username: \n  Password: " "zinbox.org")
;	("Outline" ?o "* %?" "zinbox.org")
;   )))
; '(remember-annotation-functions (quote (org-remember-annotation)))
; '(remember-handler-functions (quote (org-remember-handler)))
;)
(setq org-hide-block-startup t)
(require 'org-crypt)
;(org-crypt-use-before-save-magic)
;(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;(setq org-crypt-key "0B3A72C2")
;(setq org-crypt-disable-auto-save 'encrypt)

;(add-hook 'after-init-hook '(lambda () (org-agenda-list)))

(setq org-agenda-window-setup 'current-window)
(setq org-clone-delete-id t)
;(require 'org-location-google-maps)
;(require 'ob)
;(require 'ob-exp)
;(require 'ob-plantuml)
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
;(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(setq org-babel-results-keyword "results")
(add-hook 'org-mode-hook
  '(lambda ()
   (setq org-file-apps
     (append '(("\\.png\\'" . default)) org-file-apps ))))
(setq org-confirm-babel-evaluate nil)
;(org-current-export-file "no-export")
