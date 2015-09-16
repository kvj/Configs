; org-mpw.el - masterpass integration

(require 'org)
(require 'cl)

(defgroup org-mpw nil
  "Masterpass integration"
  :tag "Masterpass"
  :group 'org)

(defcustom org-mpw-name-property "mpw_name"
  "Property with name"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-type-property "mpw_type"
  "Property with password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-name nil
  "Name to be used"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-type-default "long"
  "Default type is 'long'"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-long "long"
  "Tag name for 'long' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-max "max"
  "Tag name for 'max' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-med "med"
  "Tag name for 'med' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-basic "basic"
  "Tag name for 'basic' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-short "short"
  "Tag name for 'short' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-pin "pin4"
  "Tag name for 'pin' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-name "name"
  "Tag name for 'name' password type"
  :type 'string
  :group 'org-mpw)

(defcustom org-mpw-tag-phrase "phrase"
  "Tag name for 'phrase' password type"
  :type 'string
  :group 'org-mpw)

(defun org-mpw-get-name ()
  "Return name"
  (save-excursion
    (org-back-to-heading t)
    (or (org-entry-get nil org-mpw-name-property t)
        org-mpw-name)))

(defun org-mpw-get-type-tag ()
  "Return password type set by tag"
  (dolist (tag (org-get-tags))
    (let 
	((value (cond ((equal tag org-mpw-tag-max) "max")
		      ((equal tag org-mpw-tag-long) "long")
		      ((equal tag org-mpw-tag-med) "med")
		      ((equal tag org-mpw-tag-pin) "pin"))))
      (if value (return value) nil))))

(defun org-mpw-get-type ()
  "Return Current password type"
  (save-excursion
    (org-back-to-heading t)
    (or (org-mpw-get-type-tag)
	(org-entry-get nil org-mpw-type-property t)
        org-mpw-type-default)))

(defun org-mpw-make-password (name site type count)
  "Call mpw"
  (print (format "Password: %s %s %s %s" name site type count) t))

(defun org-mpw-password (&optional var)
  "Print password"
  (interactive "P")
  (let ((name (org-mpw-get-name)))
    (if name 
	(org-mpw-make-password name (org-get-heading t t) (org-mpw-get-type) "0"))))

(provide 'org-mpw)
