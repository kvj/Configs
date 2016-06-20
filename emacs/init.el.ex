(setq config-dir "~/Dev/Configs/emacs/")
(setq org-directory "~/ownCloud/org/")

;(setq k-org-capture-inbox "m_phone.org") ; Failsafe inbox file
;(setq k-org-auto-open-agenda-key "w") ; Auto-open agenda on start
;(setq k-org-goto-zero t) ; Jump to outline first char
;(setq org-popup-calendar-for-date-prompt nil) ; Do not popup calendar on date input
(load (concat config-dir "init.el"))
