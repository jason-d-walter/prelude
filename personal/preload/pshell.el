;;; Package -- Summary
;;; Commentary:
;;; Code:
(require 'cl-lib)

(defun my-top-dir ()
  "Return the top directory if it exists otherwise use the buffer directory."
    (cond ((fboundp 'default-directory) (default-directory))
          ('t (with-current-buffer "*Messages*" default-directory))))

(defun my-buffer-file-name ()
  "Return the current buffer name or toplevel directory."
  (if (buffer-file-name)
      (buffer-file-name)
    (my-top-dir)))

(defun my-parent-directory (dir)
  "Return the parent directory from the given DIR."
  (let ((parent (file-name-directory (directory-file-name dir))))
    (unless (equal parent dir)
      parent)))

(defvar my-root_indicators ".git$\\|.pshell_info$\\|.p4config.txt$\\|.projectile\\|GenerateProjectFiles.bat$")
(defvar my-pshell-info-file ".pshell_info$")
(defvar my-editor-config-file ".editorconfig$")

(cl-defun my-project-dir (&optional (cur-dir (file-name-directory (my-buffer-file-name))))
  "Return the project directory based on CUR-DIR current buffer."
  (let ((pshell-files (directory-files cur-dir t my-root_indicators))
        (parent-dir (my-parent-directory (expand-file-name cur-dir))))
    (if pshell-files
        cur-dir
      (when parent-dir
        (my-project-dir parent-dir)))))

(defun my-get-pshell-info-recur (cur-dir)
  "Search for the .pshell_file using CUR-DIR."
  (let ((pshell-files (directory-files cur-dir t ".pshell_info$"))
        (parent-dir (my-parent-directory (expand-file-name cur-dir))))
    (if pshell-files
        (car pshell-files)
      (when parent-dir
        (my-get-pshell-info-recur parent-dir)))))

(defun my-get-pshell-info ()
  "Search for the .pshell_info file."
  (my-get-pshell-info-recur
   (file-name-directory
    (my-buffer-file-name))))

(defun my-parse-pshell-info ()
  "Parse the .pshell_info file."
  (let ((pshell-file (my-get-pshell-info)))
    (when pshell-file
      (with-temp-buffer
        (insert-file-contents pshell-file)
        (mapcar
         (lambda (arg)
           (split-string arg "\s*=\s*"))
         (split-string (buffer-string) "\n" t))))))

(defun my-pshell-parsed-get-item (name parsed-list)
  "Get the NAME item out of the PARSED-LIST from pshell_info file."
  (delq nil
        (mapcar (lambda (x) (if (equal (car x) name) (car (cdr x))))
                parsed-list)))

(defun my-is-git ()
  "Return non-nil if we are in an git project."
  (let ((projectdir (my-project-dir)))
    (if projectdir
        (let ((gitdir (directory-files projectdir t ".git")))
          (when gitdir 't))
      'nil)))

(defun my-is-unreal ()
  "Return non-nil if we are in an Unreal project."
  (let ((projectdir (my-project-dir)))
    (if (getenv "flow_sid")
        t
      (if projectdir
          (let ((unrealfile (directory-files projectdir t "GenerateProjectFiles.bat")))
            (when unrealfile 't))
        'nil))))

(defun my-has-editorconfig ()
  "Determines if the project dir has an editor config."
  (let ((projectdir (my-project-dir)))
    (if projectdir
      (let ((editorconfig (directory-files projectdir t my-editor-config-file)))
        (when editorconfig 't))
      'nil)))

(defun my-tab-settings ()
  "Get the tab settings."
  (let ((tab-settings
         (my-pshell-parsed-get-item "TAB_SETTINGS" (my-parse-pshell-info))))
    (if (my-is-unreal)
        (cons "tab" '("4"))
      (if tab-settings
          (split-string (car tab-settings) ",")
        (cons "space" '("4"))))))

(provide 'pshell)

;;; pshell.el ends here
