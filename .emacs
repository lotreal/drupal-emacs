;; -- main plugin directory
(add-to-list 'load-path "~/.emacs.d/includes")

(transient-mark-mode t)

(setq my-tab-width 2)

;; -- a better bookmars plugin
(require 'bm)

;; I want to be able to conmute between a split and a single window (sort of "C-x 1" for the one on focus)
(defun toggle-windows-split()
"Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
(interactive)
(if (not(window-minibuffer-p (selected-window)))
(progn
(if (< 1 (count-windows))
(progn
(window-configuration-to-register ?u)
(delete-other-windows))
(jump-to-register ?u))))
;;(my-iswitchb-close)
)

;; -- look up a drupal function
(defcustom drupal-search-url "http://api.drupal.org/api/function/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 'php)
(defun drupal-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat drupal-search-url (current-word t))))

;; -- goto symbol ido
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))


;; -- nice indenting functions
(defun indent-block()
  (shift-region my-tab-width)
  (setq deactivate-mark nil))

(defun unindent-block()
  (shift-region (- my-tab-width))
  (setq deactivate-mark nil))

(defun shift-region(numcols)
" my trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point)(mark))
    (if (not(bolp))    (progn (beginning-of-line)(exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line)(exchange-point-and-mark)(beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

(defun indent-or-complete ()
  "Indent region selected as a block; if no selection present either indent according to mode,
or expand the word preceding point. "
  (interactive)
  (if  mark-active
      (indent-block)
    (if (looking-at "\\>")
  (yas/expand nil)
      (insert "\t"))))

(defun my-unindent()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up to the beginning of line.
Now it correctly stops at the beginning of the line when the pointer is at the first char of an indented line. Before the command would (unconveniently)  kill all the white spaces, as well as the last word of the previous line."

  (interactive)
  (if mark-active
      (unindent-block)
    (progn
      (unless(bolp)
        (if (looking-back "^[ \t]*")
            (progn
              ;;"a" holds how many spaces are there to the beginning of the line
              (let ((a (length(buffer-substring-no-properties (point-at-bol) (point)))))
                (progn
                  ;; delete backwards progressively in my-tab-width steps, but without going further of the beginning of line.
                  (if (> a my-tab-width)
                      (delete-backward-char my-tab-width)
                    (backward-delete-char a)))))
          ;; delete tab and spaces first, if at least 2 exist, before removing words
          (progn
            (if(looking-back "[ \t]\\{2,\\}")
                (delete-horizontal-space)
              (backward-kill-word 1))))))))

(add-hook 'find-file-hooks (function (lambda ()
 (unless (eq major-mode 'org-mode)
(local-set-key (kbd "<tab>") 'indent-or-complete)))))

(if (not (eq  major-mode 'org-mode))
    (progn
      (define-key global-map "\t" 'indent-or-complete) ;; with this you have to force tab (C-q-tab) to insert a tab after a word
      (define-key global-map [S-tab] 'my-unindent)
      (define-key global-map [C-S-tab] 'my-unindent)))

;; mac and pc users would like selecting text this way
(defun dave-shift-mouse-select (event)
 "Set the mark and then move point to the position clicked on with
 the mouse. This should be bound to a mouse click event type."
 (interactive "e")
 (mouse-minibuffer-check event)
 (if mark-active (exchange-point-and-mark))
 (set-mark-command nil)
 ;; Use event-end in case called from mouse-drag-region.
 ;; If EVENT is a click, event-end and event-start give same value.
 (posn-set-point (event-end event)))

;; be aware that this overrides the function for picking a font. you can still call the command
;; directly from the minibufer doing: "M-x mouse-set-font"
(define-key global-map [S-down-mouse-1] 'dave-shift-mouse-select)

;; to use in into emacs for  unix I  needed this instead
; define-key global-map [S-mouse-1] 'dave-shift-mouse-select)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this final line is only necessary to escape the *scratch* fundamental-mode
;; and let this demonstration work
(text-mode)

;; -- stops emacs from auto-copying selection
(setq mouse-drag-copy-region nil)

;; -- adds line numbers
(require 'linum+)

;; don't treat _ as word delimiter for double click selection
(modify-syntax-entry ?_ "w")

;; -- windows esque redo plugin with vis branching
(require 'undo-tree)
(global-undo-tree-mode)

;; -- set up window saving
(require 'windows)
(win:startup-with-window)
(add-hook 'window-setup-hook 'resume-windows)

;; -- google's nav
(require 'nav)

;; -- improve isearch
(defun windows-isearch-hook ()
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "RET") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
  (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance))
(add-hook 'isearch-mode-hook 'windows-isearch-hook)

;; --  my custom keys
(defun math-keys-help ()
(message "Keys Engage!")

;; -- make buffer names unique
(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; to answer y or n instead of yes or no :-P ...I'm to lazy
(defalias 'yes-or-no-p 'y-or-n-p) 

;;move
(define-key global-map  [\M-up] 'scroll-down)
(define-key global-map  [\M-down] 'scroll-up)
;;coding
(global-set-key "\C-cbs" 'weblogger-start-entry)
(global-set-key (kbd "M-z") 'zencoding-expand-line)
(global-set-key (kbd "S-SPC") 'auto-complete)
(global-set-key (kbd "C-S-SPC") 'ido-goto-symbol)
(global-set-key (kbd "C-~") 'drupal-search-documentation)
(global-set-key (kbd "C-`") 'php-search-documentation)
;;(global-set-key (kbd "<S-tab>") 'auto-complete)
(global-set-key (kbd "M-o") 'nav)
;;basic
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-S-o") 'ifind-mode)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
;;(global-set-key (kbd "C-f") 'my-find)
(global-set-key (kbd "C-y") 'undo-tree-redo)
(global-set-key (kbd "C-S-f") 'query-replace)
;;windowing
(define-key global-map  [\C-left] 'previous-buffer)
(define-key global-map  [\C-right] 'next-buffer)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-tab>") 'previous-multiframe-window)
(global-set-key (kbd "C-SPC") 'switch-to-buffer)
(global-set-key (kbd "C-n") 'switch-to-buffer)
(global-set-key (kbd "C-0") 'kill-buffer)
;;functions
;;(global-set-key (kbd "<f1>") 'bookmark-set)
;;(global-set-key (kbd "<f2>") 'bookmark-jump)
;;(global-set-key (kbd "<f3>") 'bookmark-delete)
(global-set-key (kbd "<f1>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<f3>") 'bm-previous)
(global-set-key (kbd "<f4>") 'shell-here)
(global-set-key (kbd "<f5>") 'toggle-windows-split)
;;(global-set-key (kbd "<f5>") 'delete-other-windows)
(global-set-key (kbd "<f6>") 'split-window-horizontally)
(global-set-key (kbd "<f7>") 'split-window-vertically)
(global-set-key (kbd "<f8>") 'delete-window)
(global-set-key (kbd "<f9>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f10>") 'enlarge-window)
(global-set-key (kbd "<f11>") 'tortoise-svn-commit)
(global-set-key (kbd "<f12>") 'tortoise-svn-log)
;;misc
(define-key ctl-x-map "C" 'see-you-again)
(custom-set-variables
'(cua-mode t nil (cua-base)))
)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-visualize)

;; -- smart indenting
(defun srb-adaptive-indent (beg end)
"Indent the region between BEG and END with adaptive filling."
(goto-char beg)
(while
(let ((lbp (line-beginning-position))
(lep (line-end-position)))
(put-text-property lbp lep 'wrap-prefix (fill-context-prefix lbp lep))
(search-forward "\n" end t))))
(define-minor-mode srb-adaptive-wrap-mode
"Wrap the buffer text with adaptive filling."
:lighter ""
(save-excursion
(save-restriction
(widen)
(let ((buffer-undo-list t)
(inhibit-read-only t)
(mod (buffer-modified-p)))
(if srb-adaptive-wrap-mode
(progn
(setq word-wrap t)
(unless (member '(continuation) fringe-indicator-alist)
(push '(continuation) fringe-indicator-alist))
(jit-lock-register 'srb-adaptive-indent))
(jit-lock-unregister 'srb-adaptive-indent)
(remove-text-properties (point-min) (point-max) '(wrap-prefix pref))
(setq fringe-indicator-alist
(delete '(continuation) fringe-indicator-alist))
(setq word-wrap nil))
(restore-buffer-modified-p mod)))))

;; -- php / drupal section
(require 'php-mode)
(defun drupal-mode ()
  "Drupal php-mode."
  (interactive)
  (php-mode)
  (message "Drupal mode activated.")
  (set 'tab-width 2)
  (set 'c-basic-offset 2)
  (set 'indent-tabs-mode nil)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
  ; More Drupal-specific customizations here
  (srb-adaptive-wrap-mode 1)
  (linum-mode 1)
  (imenu-add-menubar-index)

  
)
(defconst my-php-style
  '((c-offsets-alist . (
    (arglist-close . c-lineup-close-paren) ; correct arglist closing parenthesis
   )))
  "My PHP Programming style"
  
)
(c-add-style "my-php-style" my-php-style)
(defun my-php-mode ()
  "My personal php-mode customizations."
  (c-set-style "my-php-style")
  ; More generic PHP customizations here
  (srb-adaptive-wrap-mode 1)
  (linum-mode 1)
  ;;(global-set-key (kbd "RET") 'newline-and-indent)
  ;;(math-keys-help)
)
(defun setup-php ()
  ; PHP
  (add-hook 'php-mode-hook 'my-php-mode)
  ; Drupal
  (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|php\\|inc\\)$" . drupal-mode))
  ;;(add-to-list 'auto-mode-alist '("/*\\.\\(php\\|inc\\)$" . drupal-mode))
  (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))
  ; More startup-setup for PHP customizations to work here
)
(setup-php)

;; -- load my keybindsings w a hook
(add-hook 'php-mode-hook 'math-keys-help)





;; ----------- LOOKS

;; -- load the color theme plugin and load the theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-gruber-darker)

;; -- hide startup screen
(set 'inhibit-startup-message' 1)

;; -- set the custom font
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#181818" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "ProggyTinyTT")))))

;; -- Show line-number in the mode line
(line-number-mode 1)




;; ----------- FUNCTIONALITY

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



;; -- this is so we can open files in open editor
(server-start)

;; -- load ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; -- sets up cygwin
(let* ((cygwin-root "c:/cygwin")
  (cygwin-bin (concat cygwin-root "/bin")))
(when (and (eq 'windows-nt system-type)
  (file-readable-p cygwin-root))
(setq exec-path (cons cygwin-bin exec-path))
(setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;; -- menu bar plus
(eval-after-load "menu-bar" '(require 'menu-bar+))





;; ----------- CODING

;; -- code completion // needs bin install
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/includes/ac-dict")
(ac-config-default)
(setq popup-use-optimized-column-computation nil)
(setq ac-auto-start nil)

;;tortoise svn
(require 'tortoise-svn)

;; -- zen coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; --  find in project
(defvar workspace-dir "c:/Progra~1/xampplite/htdocs/htdt6")
(load "~/.emacs.d/includes/ifind-mode.el")

;; -- add code snippit support
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/includes/snippets")





;; ----------- FUNCTIONS

;;shell in current directory
(defun shell-here ()
  "Open a shell in `default-directory'."
  (interactive)
  (let ((dir (expand-file-name default-directory))
        (buf (or (get-buffer "*shell*") (shell))))
    (goto-char (point-max))
    (if (not (string= (buffer-name) "*shell*"))
        (switch-to-buffer-other-window buf))
    (message list-buffers-directory)
    (if (not (string= (expand-file-name list-buffers-directory) dir))
        (progn (comint-send-string (get-buffer-process buf)
                                   (concat "cd \"" dir "\"\r"))
               (setq list-buffers-directory dir)))))





;; ----------- EXTRA

;; -- bloggin tool
(load-file "~/.emacs.d/includes/weblogger.el")






;; --  not used but enable tab bar tabs
;;(require 'tabbar)
;;(tabbar-mode)
;;(global-set-key [(control tab)] 'tabbar-forward)
;;(global-set-key [(control shift tab)] 'tabbar-backward)
;;(setq tabbar-cycling-scope "tabs")

;; -- hide the tool bar, this actually needs to be done in reg
;;(tool-bar-mode -1)

;; -- not used but this can max the window
;;(w32-send-sys-command 61488)


;; -- state save (just buffers)
;;(desktop-save-mode 1)

;; -- resize the window
;;(when window-system
;;  (run-at-time (format "%d sec" 1) nil '(lambda () (set-frame-position (selected-frame) 0 0)))
;;  (run-at-time (format "%d sec" 2) nil '(lambda () (set-frame-width (selected-frame) 165 t)))
;;  (run-at-time (format "%d sec" 3) nil '(lambda () (set-frame-height (selected-frame) 71 t)))
;;)



;; turn on transient mark mode
;;(that is, we highlight the selected text)
