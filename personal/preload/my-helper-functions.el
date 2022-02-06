;;; Package -- Summary
;;; Commentary:
;;;
;;; A set of small helper functions that I have created to do code development.
;;;
;;; Code:

(require 'whitespace)

(defvar blank-line-regexp "^\\s *$"
  "*The regexp that describes a `blank' line.")

(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (let ((ack-command "ack --nofilter --nogroup --with-filename "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))

(defun my-buffer-whitespace (space-or-tab)
  (let ()
    (if (string= (car space-or-tab) "space")
        (setq whitespace-style (quote (face lines trailing tabs newline tab-mark)))
      (setq whitespace-style (quote (face lines trailing space newline space-mark))))
    (setq-default whitespace-line-column 180)
    (whitespace-mode 1)))

;; When loading a 'README' file assume it is a text-mode document.
;;
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))

(defun part-of-paragraph ()
  "Used to determine whether the current line is part of a paragraph.
Returns nil if the line is blank or is not preceded by `fill-prefix';
returns non-nil otherwise.
  Must be used from the beginning of the line."
  (and (not (looking-at blank-line-regexp))
       (or (not fill-prefix)
           (looking-at fill-prefix))))

(defun unfill-paragraph ()
  "Do the opposite of `fill-paragraph'.
Takes the current paragraph with newline-separated lines and uses
`delete-indentation' to make one long string out of the paragraph."
  (interactive)
  (save-excursion
    (while (not (part-of-paragraph))
      (forward-line 1))
    (forward-line 1)
    (while (and (part-of-paragraph)
                (not (eq (point) (point-max))))
      (delete-indentation)
      (forward-line 1))))

(defun create-scratch-buffer nil
  "Create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


(defun insert-random-uuid ()
  "Insert a UUID.  This perform a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5 , which is
not cryptographically safe.  I'm not sure what's the implication
of its use here."

;; by Christopher Wellons, 2011-11-18. Editted by Xah Lee.
;; Edited by Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))

    (insert (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))

(provide 'my-helper-functions)

;;; End my-helper-functions.el
