;;; Package -- Summary ushell
;;; Commentary:
;;;
;;; You must have an environment variable set (USHELL_DIR) to specify the path of
;;; to ushell.
;;;  Ex: (setenv "USHELL_DIR" "d:\\work\ushell")
;;;
;;; Then assign a uproject by using the command:
;;; (ushell-launch-session-with-project "d:\work\UE4Dev\QAGame\QAGame.uproject")
;;;
;;; Then override compile-command with your favorite incandation.
;;;
;;; This will only work on Windows for the moment but minor modification should
;;; support other platfoms.
;;;
;;; Code:

(require 'pshell)

(defun ushell-get-ushell-dir ()
  "Gets the directory for ushell."
  (if (getenv "USHELL_DIR")
      (getenv "USHELL_DIR")
    (error "No USHELL_DIR set.  Please set one")))

(defun ushell-extract-key-value-pair (str)
  "Given a STR with a possible env variable assignment, extract that key value as list."
  (let ((tuple (split-string str "\s*=\s*")))
    (if (car (cdr tuple))
        (list (car (cdr (split-string (car tuple) " "))) (car (cdr tuple)))
      nil)))

(defun ushell-set-env-from-cmd-line ( str )
  "Set an environment variable from STR which represents a cmd shell expression."
  (let ((keyvalue (ushell-extract-key-value-pair str)))
    (when keyvalue
      (setenv (car keyvalue)
              (car (cdr keyvalue))))))

(defun ushell-read-cookie-file ( ushellCookie )
  "Read the USHELLCOOKIE cookie file and inject environment into Emacs."
  (with-temp-buffer
    (insert-file-contents ushellCookie)
    (mapcar 'ushell-set-env-from-cmd-line (split-string (buffer-string) "\n" t))))

(defun ushell-working-dir (ushelldir)
  "Return the working directory for ushell given the USHELLDIR."
  (concat ushelldir "\\.working"))

(defun ushell-channel-dir (ushelldir)
  "Return the channel directory for ushell given the USHELLDIR."
  (concat ushelldir "\\channels"))

(defun ushell-python-exe (ushelldir)
  "Return the current python version for ushell given the USHELLDIR."
  (concat (ushell-working-dir ushelldir)
          "\\python\\current\\python.exe"))

(defun ushell-make-cookie()
  "Make a temporary file for ushell to write its information."
  (make-temp-file "ushell_cookie"))

(defun ushell-priv-start-session ( uproject ushelldir cookie )
  "Private function that takes UPROJECT path, USHELLDIR and an COOKIE to kick-off session change."
  (let ((command (concat
                  (ushell-python-exe ushelldir)
                  " -Esu "
                  (ushell-channel-dir ushelldir)
                  "\\flow\\core\\system\\boot.py "
                  (ushell-working-dir ushelldir)
                  " "
                  (ushell-channel-dir ushelldir)
                  " -- "
                  (concat "--bootarg=cmd," cookie)
                  " "
                  (concat "--project=" uproject)
                  )))
    (if (string-suffix-p ".uproject" uproject)
        (shell-command-to-string command)
      (user-error "Invalid project file"))
    ;; TODO use call-process directly to get return code.
    ;;
    (ushell-read-cookie-file cookie)
    (message (concat "Session started with " uproject))
    ))

(require 'ido)

(defvar ushell-all-command-names
  '(
 ".autosdks info"
 ".autosdks purge"
 ".autosdks sync"
 ".build client"
 ".build editor"
 ".build game"
 ".build misc clangdb"
 ".build program"
 ".build server"
 ".build target"
 ".build xml"
 ".build xml clear"
 ".build xml edit"
 ".build xml set"
 ".cook"
 ".cook client"
 ".cook game"
 ".cook server"
 ".ddc prime"
 ".help"
 ".info"
 ".kill"
 ".p4 cherrypick"
 ".p4 mergedown"
 ".p4 switch"
 ".p4 switch list"
 ".p4 sync"
 ".p4 sync edit"
 ".project"
 ".run client"
 ".run commandlet"
 ".run editor"
 ".run game"
 ".run program"
 ".run server"
 ".sln generate"
 ".sln open"
 ".stage"
 ".uat"
 ".ushell update"
 ".ushell update check"
    ))

(defvar ushell-build-commands
  '( "client" "editor" "game" "misc clangdb" "program" "server"  "target" "xml" ) )

(defvar ushell-default-build-command "editor")
(defvar ushell-default-run-command "editor")

(defun ushell-get-build-input ( prefix commandlist &optional defaultval )
  "Get input from user to specify build COMMANDLIST and string PREFIX."
  (ido-completing-read prefix commandlist nil nil defaultval))

(defun ushell-save-last-input (input-val)
  "Save the given INPUT-VAL to the last used build command."
  (setq ushell-default-build-command input-val))

(defun ushell-is-ushell ()
  "Return non-nil if we are currently in a ushell session."
  (getenv "flow_sid"))

(defun ushell-user-error ()
  "Reports error back to user."
  (user-error "You need to establish a session using ushell-start-session"))

(defvar ushell-run-count 0)
(defvar ushell-buffer nil)

(defun ushell-get-shim-path ()
  "Examine the path to see if we can find ushell shim directory."
  (let ((path
         (car (seq-filter
               '(lambda (arg) (string-suffix-p "shims" arg))
               (split-string (getenv "PATH") ";")))))
    (if (and (ushell-is-ushell) path)
        path
      (ushell-user-error))
    ))

(defun ushell-run-as-process ( command )
  "Start a ushell COMMAND in a new buffer."
  (let ((shim-path (ushell-get-shim-path))
        (split-args (split-string command "\s+")))
    (setq ushell-run-count (+ ushell-run-count 1))
    (setq ushell-buffer
          (concat "*USHELL*" command ":" (number-to-string ushell-run-count)))
    (generate-new-buffer ushell-buffer)
    (display-buffer ushell-buffer)
    (apply #'start-process ushell-buffer ushell-buffer (concat shim-path "/" (car split-args)) (cdr split-args))))

(defun ushell-run-command ()
  "Run any ushell command."
  (interactive)
  (let ((input-val (ushell-get-build-input "ushell: "
                                           ushell-all-command-names
                                           ushell-default-run-command
                                           )))
    (setq ushell-default-run-command input-val)
    (if (ushell-is-ushell)
        (ushell-run-as-process (read-string "ushell: " input-val))
      (ushell-user-error))))

(defun ushell-build ()
  "Prompt user to pick a choice from a list."
  (interactive)
  (let ((input-val (ushell-get-build-input "ushell: .build "
                                           ushell-build-commands
                                           ushell-default-build-command
                                           )))
    (ushell-save-last-input input-val)
    (setq compile-command (concat ".build " input-val " " (read-string (concat ".build " input-val " "))))
    (if (ushell-is-ushell)
        (compile compile-command)
      (ushell-user-error))))

(defun ushell-start-session ()
  "Start a new ushell session."
  (interactive)
  (let ((pdir (my-project-dir)))
    (ushell-priv-start-session
     (read-file-name "Please set .uproject" pdir)
     (ushell-get-ushell-dir)
     (ushell-make-cookie))))

(provide 'my-ushell)

;;; my-ushell ends here
