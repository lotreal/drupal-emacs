;; -- code completion // needs bin install
(add-to-list 'ac-dictionary-directories "~/.emacs.d/includes/ac-dict")
(ac-config-default)
(setq popup-use-optimized-column-computation nil)
(setq ac-auto-start nil)

;; -- add code snippit support
(yas/initialize)
(yas/load-directory "~/.emacs.d/includes/snippets")

;; -- php / drupal section
(defun drupal-mode ()
  "Drupal php-mode."
  (interactive)
  (php-mode)
  (php-electric-mode)
  (message "Drupal mode activated.")
  (set 'tab-width 2)
  (set 'c-basic-offset 2)
  (set 'indent-tabs-mode nil)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
  ; More Drupal / PHP specific customizations here
  ;; -- this is unstable right now
  ;;(srb-adaptive-wrap-mode 1)
  (linum-mode 1)
  (imenu-add-menubar-index)
)
(defun setup-php ()
  ; PHP
  ; Drupal
  (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|php\\|inc\\)$" . drupal-mode))
  (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))
)

(defun my-css-mode ()
  "CSS Mode hook."
  (interactive)
  (css-mode)
  (set 'tab-width 2)
  (setq css-indent-offset 2)
  (set 'c-basic-offset 2)
  ;;(srb-adaptive-wrap-mode 1)
  (linum-mode 1)
  
)
(defun setup-css ()
  (add-to-list 'auto-mode-alist '("\\.css" . my-css-mode))
)

(defun my-js-mode ()
  "Js2 Mode Custom Hook."
  (message "JS2MODE")
  (interactive)
  (js2-mode)
  (setq js2-auto-indent-p t)
  (setq js2-mirror-mode t)
  (setq tab-width 2)
  (linum-mode 1)
  
)
(defun setup-js ()
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js" . my-js-mode))
)

;;; taken from starter-kit-js.el --- Some helpful Javascript helpers

(eval-after-load 'js2-mode
  '(progn

     ;; Cosmetics
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             "ƒ")
                             nil)))))

     (font-lock-add-keywords
      'js2-mode
      '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
         1 font-lock-warning-face t)))

     (defun js-lambda () (interactive) (insert "function () {\n}")
       (backward-char 5))

     ;;(add-hook 'js2-mode-hook 'coding-hook)

     (define-key js2-mode-map (kbd "C-c l") 'js-lambda)
     (define-key js2-mode-map "\C-\M-h" 'backward-kill-word)

     ;; Fix js2's crazy indentation
     (define-key js2-mode-map (kbd "TAB") (lambda () (interactive)
                                            (indent-for-tab-command)
                                            (back-to-indentation)))

     (setq js2-bounce-indent-flag nil
           js2-indent-on-enter-key t)

     (defun js-continued-var-decl-list-p ()
       "Return non-nil if point is inside a continued variable declaration list."
       (interactive)
       (let ((start (save-excursion (js-re-search-backward "\\<var\\>" nil t))))
         (and start
              (save-excursion (re-search-backward "\n" start t))
              (not (save-excursion
                     (js-re-search-backward
                      ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))
     
     (defun js-proper-indentation (parse-status)
       "Return the proper indentation for the current line."
       (save-excursion
         (back-to-indentation)
         (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
               (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
               (continued-expr-p (js-continued-expression-p)))
           (cond (ctrl-stmt-indent)
                 ((js-continued-var-decl-list-p)
                  (js-re-search-backward "\\<var\\>" nil t)
                  (+ (current-indentation) js2-basic-offset))
                 ((nth 1 parse-status)
                  (goto-char (nth 1 parse-status))
                  (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                      (progn
                        (skip-syntax-backward " ")
                        (when (= (char-before) ?\)) (backward-list))
                        (back-to-indentation)
                        (cond (same-indent-p
                               (current-column))
                              (continued-expr-p
                               (+ (current-column) (* 2 js2-basic-offset)))
                              (t
                               (+ (current-column) js2-basic-offset))))
                    (unless same-indent-p
                      (forward-char)
                      (skip-chars-forward " \t"))
                    (current-column)))
                 (continued-expr-p js2-basic-offset)
                 (t 0)))))))