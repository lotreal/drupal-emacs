;; -- Miscilanious Support Functions


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


;; -- virtual indenting
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


;; -- scroll with half pages instead of full pages
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()         
  (interactive)                    
  (scroll-down (window-half-height)))