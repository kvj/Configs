(load (concat config-dir "common.el"))
(when (boundp 'org-directory)
  (load (concat config-dir "org-ng.el")))
(load (concat config-dir "abbr.el"))
