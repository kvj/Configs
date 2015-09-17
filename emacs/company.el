(setq company-minimum-prefix-length 2)
;(setq company-auto-complete t)
(setq company-show-numbers t)
;(setq company-begin-commands '(self-insert-command))

;(require 'company)
;(global-company-mode t)

(add-hook 'after-init-hook 'global-company-mode)

(defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode)))
;(global-set-key "\t" 'indent-or-complete)
