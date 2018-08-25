; Integration with Git
(defvar k-org-git-branch "master")
(defvar k-org-git-auto-push-min 0)
(defvar k-org-git-save-push-sec 20)
(defvar k-org-git-refresh-sec 10)

; Internal
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
      (run-with-idle-timer k-org-git-refresh-sec nil 'org-agenda-redo t))
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
	  (?j (run-with-idle-timer k-org-git-refresh-sec nil 'k-org-git-push t))
	  (?h (k-org-git-pull))
	  (?s (k-org-git-pull-config))
	  (?c (k-org-git-commit "Saving changes..."))
	  (?r (k-org-git-reset))
	  (?q (message "Abort"))
	  (otherwise (error "Invalid key")))))

; Enable auto-push
(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (when (> k-org-git-auto-push-min 0)
	       (run-at-time (* k-org-git-auto-push-min 60) (* k-org-git-auto-push-min 60)
			    (lambda ()
			      (run-with-idle-timer 30 nil 'k-org-git-push))))))
