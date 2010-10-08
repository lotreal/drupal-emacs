;; -- load the color theme plugin and load the theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-twilight)

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0               ;; ... are very ...
  scroll-down-aggressively 0             ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v 



;; -- set the custom font
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#181818" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "ProggyTinyTT")))))

;; -- smart cursor mode
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "yellow")
(setq djcb-normal-cursor-type    'bar)
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
    (buffer-read-only
      (set-cursor-color djcb-read-only-color)
      (setq cursor-type djcb-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color djcb-overwrite-color)
      (setq cursor-type djcb-overwrite-cursor-type))
    (t 
      (set-cursor-color djcb-normal-color)
      (setq cursor-type djcb-normal-cursor-type))))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

;; ;; ergomacs file menu + couple changes
(setq menu-bar-file-menu
      '(keymap
	(new-file menu-item "New" new-empty-buffer)
	(make-frame menu-item "New Frame" make-frame-command)
	(open-file menu-item "Open..." find-file)
	(open-file-rep menu-item "Open in Project" find-file-in-repository)

	(find-infile menu-item "Find in File" rgrep)
	(find-infile-rep menu-item "Find in File in Project" rgrep-in-files-in-repository-drupal)
	
	(kill-buffer menu-item "Close" close-current-buffer)
	(separator1 menu-item "--")
	(save-buffer menu-item "Save" save-buffer)
	(write-file menu-item "Save As..." write-file)
	(revert-buffer menu-item "Revert" revert-buffer)
	(separator2 menu-item "--")
	(lang-modes menu-item "Language Modes"
		    (keymap (c "C" . c-mode)
			    (c++ "C++" . c++-mode)
			    (java "Java" . java-mode)
			    (separator3 "--")
			    (css "CSS" . css-mode)
			    (html "HTML" . html-mode)
			    (nxml "XML (nxml-mode)" . nxml-mode)
			    (xml "XML (xml-mode)" . xml-mode)
			    (js "Javascript (espresso-mode)" . espresso-mode)
			    (js2 "Javascript (js2-mode)" . js2-mode)
			    (latex "LaTeX" . latex-mode)
			    (separator2 "--")
			    (elisp "Emacs Lisp" . emacs-lisp-mode)
			    (ocaml "OCaml" . tuareg-mode)
			    (haskell "Haskell" . haskell-mode)
			    (separator1 "--")
			    (perl "Perl" . cperl-mode)
			    (php "PHP" . php-mode)
			    (python "Python" . python-mode)
			    (ruby "Ruby" . ruby-mode)
			    (tcl "TCL" . tcl-mode)
			    (bash "Bash" . sh-mode)
			    (cmd "cmd.exe" . dos-mode)
			    (powershell "PowerShell" . powershell-mode)
			    "major modes"))
	(separator3 menu-item "--")
	(print-buffer menu-item "Print" print-buffer)
	(ps-print-buffer-faces menu-item "Print (font+color)" ps-print-buffer-faces)
	(separator4 menu-item "--")
	(split-window menu-item "Split Window"
		      split-window-switch-buffer)
	(split-window-leftright menu-item "Split Window left/right"
				hsplit-window-switch-buffer)
	(one-window menu-item "Toggle Split"
		    toggle-windows-split)
	(separator5 menu-item "--")
	(exit-emacs menu-item "Quit" save-buffers-kill-emacs)
	(kill-buffer menu-item "Quit and Save" see-you-again)
	(kill-buffer menu-item "Revive" resume-windows)
	"File"))

(define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))


;; Creating a new menu pane in the menu bar to the right of "Tools" menu
(define-key-after
  global-map
  [menu-bar mymenu]
  (cons "Source" (make-sparse-keymap "hoot hoot"))
  'tools )

;; Creating a menu item, under the menu by the id "[menu-bar mymenu]"
(define-key
  global-map
  [menu-bar mymenu al]
  '("SVN Commit" . tortoise-svn-commit))

;; creating another menu item
(define-key
  global-map
  [menu-bar mymenu bl]
  '("SVN Log" . tortoise-svn-commit))

(define-key
  global-map
  [menu-bar mymenu cl]
  '("SVN Commit Repository" . tortoise-svn-commit-repository))

;; creating another menu item
(define-key
  global-map
  [menu-bar mymenu dl]
  '("SVN Log Repository" . tortoise-svn-log-repository))

;; Creating a menu item, under the menu by the id "[menu-bar mymenu]"
(define-key
  global-map
  [menu-bar mymenu el]
  '("Git Commit" . tortoise-git-commit))

;; creating another menu item
(define-key
  global-map
  [menu-bar mymenu fl]
  '("Git Log" . tortoise-git-commit))

(define-key
  global-map
  [menu-bar mymenu gl]
  '("Git Commit Repository" . tortoise-git-commit-repository))

;; creating another menu item
(define-key
  global-map
  [menu-bar mymenu hl]
  '("Git Log Repository" . tortoise-git-log-repository))

;; creating another menu item
(define-key
  global-map
  [menu-bar mymenu sep]
  '("----------"))

(define-key
  global-map
  [menu-bar mymenu il]
  '("Lookup Drupal Function" . drupal-search-documentation))
(define-key
  global-map
  [menu-bar mymenu jl]
  '("Zen Coding" . zencoding-expand-line))
(define-key
  global-map
  [menu-bar mymenu kl]
  '("PHP Function List" . phpcode-anything))
(define-key
  global-map
  [menu-bar mymenu ll]
  '("Shell in Current Directory" . shell-here))
(define-key
  global-map
  [menu-bar mymenu ml]
  '("Comment / Uncomment" . my-comment-dwim))

;; code to remove the whole menu panel
;;(global-unset-key [menu-bar mymenu])

;; Creating a new menu pane in the menu bar to the right of "Tools" menu
(define-key-after
  global-map
  [menu-bar mymenuorg]
  (cons "Org-TODO" (make-sparse-keymap "hoot hoot"))
  'tools )

;; Creating a menu item, under the menu by the id "[menu-bar mymenu]"
(define-key
  global-map
  [menu-bar mymenuorg al]
  '("Show BaseCamp" . basecamp-showlist))

(define-key
  global-map
  [menu-bar mymenuorg am]
  '("Show Projects" . basecamp-showprojects))




;; ;; -- keep org mode tags displayed correctly
;; (setq ba/org-adjust-tags-column t)

;; (defun ba/org-adjust-tags-column-reset-tags ()
;;   "In org-mode buffers it will reset tag position according to
;; `org-tags-column'."
;;   (when (and
;;          (not (string= (buffer-name) "*Remember*"))
;;          (eql major-mode 'org-mode))
;;     (let ((b-m-p (buffer-modified-p)))
;;       (condition-case nil
;;           (save-excursion
;;             (goto-char (point-min))
;;             (command-execute 'outline-next-visible-heading)
;;             ;; disable (message) that org-set-tags generates
;;             (flet ((message (&rest ignored) nil))
;;               (org-set-tags 1 t))
;;             (set-buffer-modified-p b-m-p))
;;         (error nil)))))

;; (defun ba/org-adjust-tags-column-now ()
;;   "Right-adjust `org-tags-column' value, then reset tag position."
;;   (set (make-local-variable 'org-tags-column)
;;        (- (- (window-width) (length org-ellipsis))))
;;   (ba/org-adjust-tags-column-reset-tags))

;; (defun ba/org-adjust-tags-column-maybe ()
;;   "If `ba/org-adjust-tags-column' is set to non-nil, adjust tags."
;;   (when ba/org-adjust-tags-column
;;     (ba/org-adjust-tags-column-now)))

;; (defun ba/org-adjust-tags-column-before-save ()
;;   "Tags need to be left-adjusted when saving."
;;   (when ba/org-adjust-tags-column
;;      (setq org-tags-column 1)
;;      (ba/org-adjust-tags-column-reset-tags)))

;; (defun ba/org-adjust-tags-column-after-save ()
;;   "Revert left-adjusted tag position done by before-save hook."
;;   (ba/org-adjust-tags-column-maybe)
;;   (set-buffer-modified-p nil))

;; ; automatically align tags on right-hand side
;; (add-hook 'window-configuration-change-hook
;;           'ba/org-adjust-tags-column-maybe)
;; (add-hook 'before-save-hook 'ba/org-adjust-tags-column-before-save)
;; (add-hook 'after-save-hook 'ba/org-adjust-tags-column-after-save)
;; (add-hook 'org-agenda-mode-hook '(lambda ()
;;                                   (setq org-agenda-tags-column (- (window-width)))))

;; ; between invoking org-refile and displaying the prompt (which
;; ; triggers window-configuration-change-hook) tags might adjust, 
;; ; which invalidates the org-refile cache
;; (defadvice org-refile (around org-refile-disable-adjust-tags)
;;   "Disable dynamically adjusting tags"
;;   (let ((ba/org-adjust-tags-column nil))
;;     ad-do-it))
;; (ad-activate 'org-refile)




