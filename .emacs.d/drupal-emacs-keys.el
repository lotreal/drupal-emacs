
;;turn off my key mode for mini-buffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0)
  (local-set-key (kbd "<escape>") 'keyboard-escape-quit))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook) 

;;setup a minor mode to contain my key bindings
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
;;move
(define-key my-keys-minor-mode-map [\M-up] 'scroll-down-half)
(define-key my-keys-minor-mode-map [\M-down] 'scroll-up-half)
(define-key my-keys-minor-mode-map [next] 'scroll-up-half)
(define-key my-keys-minor-mode-map [prior] 'scroll-down-half)
;;coding
(define-key my-keys-minor-mode-map "\C-cbs" 'weblogger-start-entry)
(define-key my-keys-minor-mode-map (kbd "C-S-x") 'zencoding-expand-line)
(define-key my-keys-minor-mode-map (kbd "S-SPC") 'auto-complete)
(define-key my-keys-minor-mode-map (kbd "C-S-SPC") 'ido-goto-symbol)
(define-key my-keys-minor-mode-map (kbd "C-~") 'drupal-search-documentation)
(define-key my-keys-minor-mode-map (kbd "C-`") 'php-search-documentation)
(define-key my-keys-minor-mode-map (kbd "C-S-n") 'nav)
(define-key my-keys-minor-mode-map (kbd "C-{") 'my-comment-dwim)
;;basic
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key my-keys-minor-mode-map (kbd "RET") 'newline-and-indent)
(define-key my-keys-minor-mode-map (kbd "C-S-o") 'find-file-in-repository)
(define-key my-keys-minor-mode-map (kbd "C-o") 'find-file)
(define-key my-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-S-s") 'rename-file-and-buffer)
(define-key my-keys-minor-mode-map (kbd "C-S-g") 'google)
(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward)
(define-key my-keys-minor-mode-map (kbd "C-y") 'undo-tree-redo)
(define-key my-keys-minor-mode-map (kbd "C-S-f") 'query-replace)
(define-key my-keys-minor-mode-map (kbd "C-S-v") 'browse-kill-ring)
;;windowing
(define-key my-keys-minor-mode-map [\C-left] 'previous-buffer)
(define-key my-keys-minor-mode-map [\C-right] 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'next-multiframe-window)
(define-key my-keys-minor-mode-map (kbd "<C-S-tab>") 'previous-multiframe-window)
(define-key my-keys-minor-mode-map (kbd "C-SPC") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "C-n") 'shell-here)
(define-key my-keys-minor-mode-map (kbd "C-0") 'kill-buffer)
;;functions
(define-key my-keys-minor-mode-map (kbd "<f1>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "<S-f1>") 'buffer-anything)
(define-key my-keys-minor-mode-map (kbd "<f2>") 'bm-next)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'bm-previous)
(define-key my-keys-minor-mode-map (kbd "<f4>") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "<S-f4>") 'emacs-anything)
(define-key my-keys-minor-mode-map (kbd "<S-f4>") 'rgrep-in-files-in-repository-drupal)
(define-key my-keys-minor-mode-map (kbd "<C-S-f4>") 'phpcode-anything)
(define-key my-keys-minor-mode-map (kbd "<f5>") 'toggle-windows-split)
(define-key my-keys-minor-mode-map (kbd "<f6>") 'hsplit-window-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "<f7>") 'split-window-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "<f8>") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "<f9>") 'enlarge-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "<f10>") 'enlarge-window)
(define-key my-keys-minor-mode-map (kbd "<f11>") 'tortoise-svn-commit)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'tortoise-svn-log)
(define-key my-keys-minor-mode-map (kbd "<S-f11>") 'tortoise-git-commit)
(define-key my-keys-minor-mode-map (kbd "<S-f12>") 'tortoise-git-log)
(define-key my-keys-minor-mode-map (kbd "<C-S-f11>") 'tortoise-svn-commit-repository)
(define-key my-keys-minor-mode-map (kbd "<C-S-f12>") 'tortoise-svn-log-repository)
(define-key ctl-x-map (kbd "<f11>") 'tortoise-git-commit-repository)
(define-key ctl-x-map (kbd "<f12>") 'tortoise-git-log-repository)
;;misc
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo-tree-undo)
(define-key my-keys-minor-mode-map (kbd "C-S-z") 'undo-tree-visualize)
(define-key ctl-x-map "C" 'see-you-again)
(custom-set-variables
  '(cua-mode t nil (cua-base)))

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

;;turn it on globally
(my-keys-minor-mode 1)

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

;; -- hooks for nice indenting keys
(add-hook 'find-file-hooks (function (lambda ()
 (unless (eq major-mode 'org-mode)
(local-set-key (kbd "<tab>") 'indent-or-complete)))))

(if (not (eq  major-mode 'org-mode))
    (progn
      (global-set-key "\t" 'indent-or-complete)
      (global-set-key [S-tab] 'my-unindent)
      (global-set-key [C-S-tab] 'my-unindent)))
