(use-package org-roam
	     :ensure t
	     :hook
	     (after-init . org-roam-mode)
	     :custom
	     (org-roam-directory "~/Dropbox/org/.roam/")
	     :bind (:map org-roam-mode-map
			 (("C-c n l" . org-roam)
			  ("C-c n f" . org-roam-find-file)
			  ("C-c n g" . org-roam-graph))
			 :map org-mode-map
			 (("C-c n i" . org-roam-insert))
			 (("C-c n I" . org-roam-insert-immediate))))
