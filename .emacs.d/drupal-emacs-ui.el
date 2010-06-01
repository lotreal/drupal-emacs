;; -- load the color theme plugin and load the theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-gruber-darker)

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

;; ergomacs file menu + couple changes
(setq menu-bar-file-menu
      '(keymap
	(new-file menu-item "New" new-empty-buffer)
	(make-frame menu-item "New Frame" make-frame-command)
	(open-file menu-item "Open..." find-file)
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