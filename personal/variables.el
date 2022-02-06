;; On OSX we want to swap the 'meta key with the command key.
;; maps closer to the Unix and windows way...
;;
(setq mac-command-modifier 'meta)

;; display the system time in the status var.
(display-time)

;;(if (eq system-type 'darwin)
    ;;(setq exec-path (append exec-path '("/opt/homebrew/bin"))))

(if (not (string-match "nt" system-configuration))
    (setenv "P4DIFF" "/usr/bin/diff")
  (setenv "P4DIFF" "c:/cygwin64/bin/diff.exe"))
