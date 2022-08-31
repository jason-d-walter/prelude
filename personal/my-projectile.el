;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'projectile)

;; Faster searching on Windows
(when (eq system-type 'windows-nt)
  (when (or (executable-find "fd") (executable-find "rg"))
    (setq projectile-indexing-method 'alien))

  ;; FIXME: too slow while getting submodule files on Windows
  (setq projectile-git-submodule-command nil))


;; my-projectile ends here
