;(setq config-dir "~/Dev/Configs/emacs/")
;(setq org-directory "~/Documents/org/")

;(setq k-org-capture-inbox "inbox/m_phone.org") ; Failsafe inbox file
;(setq k-org-auto-open-agenda-key "w") ; Auto-open agenda on start
;(setq org-agenda-tags-column -50)
(setq k-org-goto-zero t) ; Jump to outline first char
(setq k-auto-save t)

(load (concat config-dir "init.el"))
;(setq org-popup-calendar-for-date-prompt nil) ; Do not popup calendar on date input
