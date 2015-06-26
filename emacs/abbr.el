(add-hook 'org-mode-hook 
		  (lambda () (abbrev-mode 1)))
; Work is in progress
(define-abbrev-table 
  'org-mode-abbrev-table '(
						   ("fr" "- []" nil 0)))
