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

;; -- nice commenting function
(defun my-comment-dwim (arg)
"If region is active, call `comment-or-uncomment-region'.
Else, if the line is empty, call `comment-insert-comment-function'
if it is defined, otherwise insert a comment and indent it.
Else, call `comment-or-uncomment-region' on the whole line"
(interactive "P")
(comment-normalize-vars)
(message "hello")
(if (and mark-active transient-mark-mode)
(comment-or-uncomment-region (region-beginning) (region-end) arg)
(if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
(comment-or-uncomment-region (line-beginning-position) (line-end-position))
(if comment-insert-comment-function
(funcall comment-insert-comment-function)
(let ((add (comment-add arg)))
;; Some modes insist on keeping column 0 comment in column 0
;; so we need to move away from it before inserting the comment.
(indent-according-to-mode)
(insert (comment-padright comment-start add))
(save-excursion
(unless (string= "" comment-end)
(insert (comment-padleft comment-end add)))
(indent-according-to-mode)))))))


;; -- better renaming
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

;; duplicate line
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; -- google selection
(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

;; -- better window spliting
(defun split-window-switch-buffer () (interactive)
  "Split current window and display the two last buffers used."
  (split-window)
  (switch-to-buffer (other-buffer (current-buffer)))
  )

(defun hsplit-window-switch-buffer () (interactive)
  "Split current window horizontally and display the two last buffers used."
  (split-window-horizontally)
  (switch-to-buffer (other-buffer (current-buffer)))
  )

;; -- fixes problem with cygwin and rgrep
;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
	ad-do-it))
(ad-activate 'grep-compute-defaults)

;; -- find file in current repository
(defun find-file-in-repository ()
  (interactive)
  "Calls Find-in-File within the current repository root."
  (if (repository-root buffer-file-name)
      (setq workspace-dir (repository-root buffer-file-name))
      (setq workspace-dir (concat "\"" (file-name-directory buffer-file-name) "\"")))
  (ifind-mode))

;; -- find file in current repository
(defun rgrep-in-files-in-repository-drupal (pattern)
  "Calls rgrep within the current repository root."
  (interactive "sGrep pattern: ")
  (if (repository-root buffer-file-name)
      (rgrep pattern "*.php *.module *.php *.inc *.js *.el *.html" (repository-root buffer-file-name))
      (rgrep pattern "*" (file-name-directory buffer-file-name))))

;; -- find file in current repository
(defun find-function-at-point-in-repository-drupal ()
  "Finds the definition of the function under cursor."
  (interactive)
  (if (repository-root buffer-file-name)
      (rgrep (concat "function " (current-word)) "*.php *.module *.php *.inc *.js" (repository-root buffer-file-name))
      (rgrep (concat "function " (current-word)) "*" (file-name-directory buffer-file-name))))

(defun replace-globally ()
  "Run replace-regexp across the whole file, rather than from
point to the end of the file."
  (interactive)
  (let ((before (point)))
    (goto-char (point-min))
    (call-interactively 'replace-regexp)
    (when (= (point) (point-min))
      (goto-char before))))

(defun buffer-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+
     anything-c-source-bm
     anything-c-source-recentf
     anything-c-source-files-in-current-dir+)
   " *buffer-anything*"))

(defun emacs-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-emacs-functions
     anything-c-source-emacs-commands
     anything-c-source-extended-command-history)
   " *emacs-anything*"))

(defun phpcode-anything ()
  (interactive)
  (anything-other-buffer-php
   '(anything-c-source-browse-code)
   " *code-anything*"))