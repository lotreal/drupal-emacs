;; --  my custom keys
(defun math-keys-help ()
  (message "Keys Engage!")

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
  (global-set-key (kbd "M-o") 'nav)
  ;;basic
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key (kbd "C-S-o") 'ifind-mode)
  (global-set-key (kbd "C-o") 'find-file)
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "C-f") 'isearch-forward)
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
  (global-set-key (kbd "<f1>") 'bm-toggle)
  (global-set-key (kbd "<f2>") 'bm-next)
  (global-set-key (kbd "<f3>") 'bm-previous)
  (global-set-key (kbd "<f4>") 'shell-here)
  (global-set-key (kbd "<f5>") 'toggle-windows-split)
  (global-set-key (kbd "<f6>") 'split-window-horizontally)
  (global-set-key (kbd "<f7>") 'split-window-vertically)
  (global-set-key (kbd "<f8>") 'delete-window)
  (global-set-key (kbd "<f9>") 'enlarge-window-horizontally)
  (global-set-key (kbd "<f10>") 'enlarge-window)
  (global-set-key (kbd "<f11>") 'tortoise-svn-commit)
  (global-set-key (kbd "<f12>") 'tortoise-svn-log)
  ;;misc
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-visualize)
  (define-key ctl-x-map "C" 'see-you-again)
  (custom-set-variables
  	'(cua-mode t nil (cua-base))))

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
      (define-key global-map "\t" 'indent-or-complete)
      (define-key global-map [S-tab] 'my-unindent)
      (define-key global-map [C-S-tab] 'my-unindent)))
