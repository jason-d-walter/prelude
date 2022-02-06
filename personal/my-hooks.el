;;; package -- Summary
;;; Commentary:
;;; Code:

(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'smartparens-config)
(require 'pshell)

(defun my-apply-spacing ()
  "Apply old way of setting tab / space settings."
  (let ((tab-settings (my-tab-settings)))
    (let ( (is-tab (if (string= (car tab-settings) "tab")
                       't nil))
           (spacing (string-to-number (car (last tab-settings)))))
      (defvar c-basic-indent spacing)
      (defvar c-basic-offset spacing)

      (defvar js-indent-level spacing)
      (defvar ruby-indent-level spacing)
      (defvar web-mode-code-indent-offset spacing)
      (defvar web-mode-css-indent-offset spacing)
      (defvar web-mode-markup-indent-offset spacing)
      (defvar tab-width spacing)
      (defvar python-indent spacing)
      (defvar default-tab-width spacing)
      (defvar sgml-basic-offset spacing)
      (defvar lua-indent-level spacing)
      (defvar css-indent-offset spacing)
      (setq indent-tabs-mode is-tab)
      )))

(defun standard-hook()
  "Apply a standard set of mode hooks."
  (if (my-has-editorconfig)
      (editorconfig-mode 1)
    (my-apply-spacing))
  (if indent-tabs-mode
      (my-buffer-whitespace '("tab"))
    (my-buffer-whitespace '("space"))
    )
  (rainbow-delimiters-mode)
  (smartparens-mode)
  )

(defun my-c-hook ()
  "Hook for c and c++ editing."
  (font-lock-mode)
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace 0)

  (standard-hook)
  (company-mode)
  (subword-mode)
;;  (yas-minor-mode)
;;  (lsp)
  (flycheck-mode))

(add-hook 'c-mode-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c-hook)

(defun python-hook-handler ()
  "My hook handler for python."
  (font-lock-mode)
  ;;(defvar python-guess-indent nil)
  (standard-hook))

(add-hook
 'python-mode-hook
 (lambda ()
   (python-hook-handler)))

(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode)
             (standard-hook)
             ;; The following line forces text-mode to always use tab insert function which
             ;; then should reflect the settings above.
             ;; See:
             ;; https://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
             ;;         (setq indent-line-function 'insert-tab)
             ))

;; Lua mode
;;
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;; If we encounter an sjson file then enter lua mode too
;;
(add-to-list 'auto-mode-alist '("\\.sjson$" . lua-mode))

;; Bind lua to the interpreter mode.
;;
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
