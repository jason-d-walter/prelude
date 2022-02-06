;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'compile)
(require 'pshell)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal"))

(require 'my-ushell)

(setq compilation-read-command nil)
(setq compilation-scroll-output t)

(defun command-separator ()
  "Return the command separator for this platform."
  (if (eq system-type 'windows-nt)
      "&"
    ";"))

(defun priv-pshell-based-build ()
  "Use the pshell way to get the build command."
    (let ((build-app
         (car (my-pshell-parsed-get-item
                "BUILDAPP"
                (my-parse-pshell-info)))))
    (setenv "PSHELL_BUFFER_NAME" (buffer-file-name))
    (if build-app
        (concat "cd " (my-project-dir) (command-separator) " " build-app)
      "")))

(defun my-compile ()
  "Retreives the command to build the application."
  (interactive)
  (if (my-is-unreal)
      (ushell-build)
    (compile (priv-shell-based-build))))

;;(setq compile-command '(my-compile-settings))

(defvar app-buffer nil
  "Buffer for communication with APP.")

(defun app-show-buffer ()
  "Make sure `app-buffer' is displayed."
  (interactive)
  (display-buffer app-buffer))

(defun my/fix-split ( list-guy )
  "Given a LIST-GUY of command options join those command options together.  That are quoted."
  (let ((cmd (car list-guy))
        (rest (cdr list-guy))
        (dostrip (string-prefix-p "\"" (car list-guy))))
    (while dostrip
      (if dostrip
          (setq cmd (concat cmd " " (car rest))))
      (setq dostrip (not (string-suffix-p "\"" (car rest))))
      (setq rest (cdr rest)))
    (cons (string-trim cmd "\"" "\"") rest)))

(defun my/start-process (app-buffer command-args-list)
  "Given an APP-BUFFER and COMMAND-ARGS-LIST of command line arguments launch `start-process` with those arguments."
  (let ((default-directory (my-top-dir)))
    (apply 'start-process
           app-buffer
           app-buffer
           command-args-list)))

(defun launch-app ()
  "Run the current app."
  (interactive)
  (let ((run-app
         (car (my-pshell-parsed-get-item
               "RUNAPP"
               (my-parse-pshell-info)))))
    (when run-app
      (setq app-buffer
            (concat "*APP*:" (getenv "PROJECT")))
      (if (not (get-buffer app-buffer))
          (generate-new-buffer app-buffer))
      (app-show-buffer)
      (let ((command-args
             (my/fix-split (split-string (concat (my-project-dir) run-app)))))
        (my/start-process app-buffer command-args)))))

(provide 'my-compile-settings)

;;; my-compile-settings ends here
