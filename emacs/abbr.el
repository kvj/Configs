(add-hook 'org-mode-hook 
		  (lambda () (abbrev-mode 1)))
; Work is in progress
(define-abbrev-table 
  'org-mode-abbrev-table '(
			   ("0i" "- [ ]" nil 0)
			   ("0h" "**" nil 0)
			   ))
