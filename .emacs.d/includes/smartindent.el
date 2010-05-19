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