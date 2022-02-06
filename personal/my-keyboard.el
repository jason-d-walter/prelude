;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'pshell)

(define-key global-map [(f5)] 'my-compile)
(define-key global-map [(f7)] 'ushell-run-command)

(global-set-key (kbd "<f3>") #'projectile-ripgrep)
(global-set-key (kbd "C-<f3>") #'deadgrep)

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cp" 'ispell-word)
(global-set-key "\C-cr" 'insert-aw-copyright)
(global-set-key "\C-cf"  'toggle-frame-fullscreen)
(global-set-key "\C-xri" 'string-insert-rectangle)
(global-set-key "\C-csf" 'cscope-find-this-file)
(global-set-key "\C-co" 'org-open-file)
(global-set-key "\C-ct" 'todo-list)

;; SCM configuratoin
;; Perforce vs .git for C-x g
(defun my-set-p4-scm ()
  "Set the SCM to be perforce."
  (require 'p4)
  (global-set-key (kbd "C-x g") 'p4-opened))

(defun my-set-git-scm ()
  "Set the SCM to be git."
  (global-set-key (kbd "C-x g") 'magit-status))

(defun my-query-pshell-info-scm ()
  "Look for a .pshell file and sees if an SCM is defined."
  (car (my-pshell-parsed-get-item
        "BUILDAPP"
        (my-parse-pshell-info))))

(defun my-pick-scm-keybinding (scm)
  "If SCM is git then use git keybindings otherwise p4."
  (if (string= (downcase scm) "git")
      (my-set-git-scm)
    (my-set-p4-scm)))

(if (getenv "SCM")
    (my-pick-scm-keybinding (getenv "SCM"))
  (if (my-is-git)
      (my-pick-scm-keybinding "git")
    (if (my-query-pshell-info-scm)
        (my-pick-scm-keybinding (my-query-pshell-info-scm))
      (my-set-p4-scm))))

;; Setup multiple-cursors

(global-set-key (kbd "C-c C-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-=") 'er/expand-region)

(provide 'my-keyboard)
;;; keybindings.el ends here
