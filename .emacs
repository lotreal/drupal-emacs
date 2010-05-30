;; This is the first thing to get loaded.
;;
;; Stolen from the Emacs Starter Kit
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;          -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.

(setq debug-on-error t)

;;(setq menu-bar-mode 1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(setq scroll-bar-mode 1)

;; -- main plugin directory
(add-to-list 'load-path "~/.emacs.d/includes")
(add-to-list 'load-path "~/.emacs.d")

;; ----------------------------------------------------------- variable

(defvar workspace-dir "c:/Progra~1/xampplite/htdocs/htdt6")

;; ----------------------------------------------------------- load stuff

(require 'bm)
(require 'linum+)
(require 'undo-tree)
(require 'windows)
(require 'uniquify) 
(require 'ido)
(require 'nav)
(require 'php-mode)
(require 'auto-complete-config)
(require 'tortoise-svn)
(require 'tortoise-git)
(require 'zencoding-mode)
(require 'yasnippet)
(require 'browse-kill-ring)
(require 'repository-root)
(require 'php-electric)
(require 'css-mode)
(require 'css-complete)

(eval-after-load "menu-bar" '(require 'menu-bar+))

(load "~/.emacs.d/includes/ifind-mode.el")

(load-file "~/.emacs.d/drupal-emacs-sys-type.el")
(load-file "~/.emacs.d/drupal-emacs-coding.el")
(load-file "~/.emacs.d/drupal-emacs-functions.el")
(load-file "~/.emacs.d/drupal-emacs-keys.el")
(load-file "~/.emacs.d/drupal-emacs-ui.el")
(load-file "~/.emacs.d/drupal-emacs-indenting.el")

;;(load-file "~/.emacs.d/drupal-emacs-sys-linux.el")
(load-file "~/.emacs.d/emacs-nt-window.el")

;; ----------------------------------------------------------- hooks

;; -- load the saved windows automatically on boot
(add-hook 'window-setup-hook 'resume-windows)

;; -- load my keybgindsings w a hook for php
(add-hook 'php-mode-hook 'math-keys-help)

;; -- load my keybindings initially
(math-keys-help)

;; -- zen coding
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; ----------------------------------------------------------- system spec

;; You can keep system- or user-specific customizations here
;; -- stolen from the emacs started kit
;;(setq system-specific-config (concat dotfiles-dir system-name ".el")
;;      user-specific-config (concat dotfiles-dir user-login-name ".el")
;;      emacsv-specific-config (concat dotfiles-dir (emacs-type) ".el")
;;      user-specific-dir (concat dotfiles-dir user-login-name))
;;(add-to-list 'load-path user-specific-dir)

;;(if (file-exists-p system-specific-config) (load system-specific-config))
;;(if (file-exists-p user-specific-config) (load user-specific-config))
;;(if (file-exists-p emacsv-specific-config) (load emacsv-specific-config))
;;(if (file-exists-p user-specific-dir)
;; (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;; ----------------------------------------------------------- misc settings

;; -- might fix rgrep
(grep-compute-defaults)


;; -- enable recent files menu
;;(require 'recentf)
;;(setq recentf-auto-cleanup 'never)
;;(recentf-mode 1)

;; -- tame the mouse scrolling a little
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; -- need explanaion
(transient-mark-mode t)

;; -- hide startup screen
(set 'inhibit-startup-message' 1)

;; -- Show line-number in the mode line
(line-number-mode 1)

;; -- set my global ta width to 2
(setq my-tab-width 2)

;; -- Show line-number in the mode line
(line-number-mode 1)

;; -- stops emacs from auto-copying selection
(setq mouse-drag-copy-region nil)

;; don't treat _ as word delimiter for double click selection
(modify-syntax-entry ?_ "w")

;; -- windows esque redo plugin with vis branching
(global-undo-tree-mode)

; to answer y or n instead of yes or no :-P ...I'm to lazy
(defalias 'yes-or-no-p 'y-or-n-p) 

;; -- make buffer names unique
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; -- surpress shell exit warnings
(add-hook 'shell-mode-hook 
          (lambda () 
            (set-process-query-on-exit-flag 
              (get-buffer-process (current-buffer)) nil))) 

;; -- this is so we can open files in same editor instance
(server-start)

;; -- load ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; -- setup our php mode
(setup-php)

;; -- setup custom css mode
(setup-css)

;; -- set up repository detection
(add-to-list 'repository-root-matchers repository-root-matcher/svn)
(add-to-list 'repository-root-matchers repository-root-matcher/git)

;; -- custom drupal install detection
(defconst repository-root-matcher/drupal (cons 'repository-root-rule/root-contains "includes/bootstrap.inc")
  "Drupal install root directory matching criterion."
  )
(add-to-list 'repository-root-matchers repository-root-matcher/drupal)

;; -- set up window saving !! RUNLAST
(win:startup-with-window)