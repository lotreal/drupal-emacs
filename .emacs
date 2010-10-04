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

;;(setq debug-on-error t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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
(require 'saveplace)
(require 'recentf)
(require 'anything-config)

(eval-after-load "menu-bar" '(require 'menu-bar+))

(load "~/.emacs.d/includes/ifind-mode.el")

(load-file "~/.emacs.d/drupal-emacs-sys-type.el")
(load-file "~/.emacs.d/drupal-emacs-functions.el")
(load-file "~/.emacs.d/drupal-emacs-keys.el")
(load-file "~/.emacs.d/drupal-emacs-coding.el")
(load-file "~/.emacs.d/drupal-emacs-ui.el")
(load-file "~/.emacs.d/drupal-emacs-indenting.el")

;;(load-file "~/.emacs.d/drupal-emacs-sys-linux.el")
(load-file "~/.emacs.d/emacs-nt-window.el")

;; ----------------------------------------------------------- hooks

;; -- load the saved windows automatically on boot
(add-hook 'window-setup-hook 'resume-windows)

;; -- zen coding
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")



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

;; -- store things
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq savehist-file "~/.emacs.d/cache/savehist")
(setq recentf-save-file "~/.emacs.d/cache/recentf")
(setq auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.saves-")

;; -- might fix rgrep
(grep-compute-defaults)

;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
  '(search ring regexp-search-ring)    ;; ... my search entries
  savehist-autosave-interval 60)        ;; save every minute (default: 5 min)
(savehist-mode t)                      ;; do customization before activation

;; -- save place in file
(setq-default save-place t)

;; -- enable recent files menu
(setq
  recentf-max-saved-items 100     ;; max save 100
  recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                  ;; turn it on

;; backups
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  version-control t
  kept-new-versions 2
  backup-directory-alist '(("." . "~/.emacs.d/cache/backups"))
  kept-old-versions 5
  delete-old-versions t)
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; bookmarks
(setq bm-highlight-style 'bm-highlight-only-fringe)
(setq bm-buffer-persistence t)
(global-set-key (kbd "<left-fringe> <mouse-3>") #'(lambda(event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (bm-toggle))))

;; -- highlight line mode
(global-hl-line-mode 1)
(set-face-background 'hl-line "#111")  ;; Emacs 22 Only

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

;; -- show file size
(size-indication-mode t)

;; -- Show line-number in the mode line
(line-number-mode 0)

;; -- stops emacs from auto-copying selection
(setq mouse-drag-copy-region nil)

;; don't treat _ as word delimiter for double click selection
(modify-syntax-entry ?_ "w")

;; -- windows esque redo plugin with vis branching
(global-undo-tree-mode)
;;(undo-list-transfer-to-tree)

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

;; -- fring mode settings
(modify-all-frames-parameters
     (list (cons 'left-fringe 15)
	   (cons 'right-fringe 3)))

;; -- this is so we can open files in same editor instance
(server-start)

;; -- load ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; -- setup our coding modes
(setup-php)
(setup-css)
(setup-js)

;; -- show matching braces
(show-paren-mode)
(set-face-background 'show-paren-match-face "#000000")
(set-face-attribute 'show-paren-match-face nil :weight 'bold)

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