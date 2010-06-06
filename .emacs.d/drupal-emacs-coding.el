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
  (srb-adaptive-wrap-mode 1)
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
  "Drupal php-mode."
  (interactive)
  (css-mode)
  (set 'tab-width 2)
  (setq css-indent-offset 2)
  (set 'c-basic-offset 2)
  (srb-adaptive-wrap-mode 1)
  (linum-mode 1)
  
)
(defun setup-css ()
  (add-to-list 'auto-mode-alist '("\\.css" . my-css-mode))
)

(defun my-js-mode ()
  "Drupal php-mode."
  (interactive)
  (js2-mode)
  (setq js2-auto-indent-p t)
  (setq js2-enter-indents-newline t)
  (setq js2-idle-timer-delay 10.0)
  (setq js2-indent-on-enter-key nil)
  (setq js2-mirror-mode t)
  (set 'tab-width 2)
  (setq js-indent-offset 1)
  (set 'c-basic-offset 1)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default indent-offset 1)
  ;;(srb-adaptive-wrap-mode 1)
  (linum-mode 1)
  
)
(defun setup-js ()
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js" . my-js-mode))
)


;; (define-key js-mode-map "[" 'js-electric-bracket)
;; (define-key js-mode-map "{" 'js-electric-brace)
;; (define-key js-mode-map "(" 'js-electric-paren)
;; (define-key js-mode-map "\"" 'js-electric-quote)
;; (define-key js-mode-map "'" 'js-electric-prime)
;; (define-key js-mode-map [backspace] 'js-electric-delete)

;; (defun js-electric-brace () 
;;   (interactive)
;;   (js-surround "{\n  " "\n}"))

;; (defun js-electric-bracket () 
;;   (interactive)
;;   (js-surround "[" "]"))

;; (defun js-electric-paren () 
;;   (interactive)
;;   (js-surround "(" ")"))

;; (defun js-electric-quote () 
;;   (interactive)
;;   (js-surround "\"" "\""))

;; (defun js-electric-prime () 
;;   (interactive)
;;   (js-surround "'" "'"))

;; (defun js-electric-delete ()
;;   (interactive)
;;   (if (or
;;        (js-between "[" "]")
;;        (js-between "{" "}")
;;        (js-between "(" ")")
;;        (js-between "\"" "\"")
;;        (js-between "'" "'"))
;; 	(progn
;; 	  (forward-char 1)
;; 	  (delete-char -2))
;;     (delete-char -1)))

;; (defun js-between (x y)
;;   (and (char-equal (char-before (point)) (string-to-char x))
;;        (looking-at y))
;; )

;; (defun js-surround (x y)
;;   (princ x (current-buffer))
;;   (save-excursion (princ y (current-buffer))))
  
;; (defun js-between (x y)
;;   (and (char-is-at -1 x)
;;        (looking-at y)))

;; (defun char-is-at (location char)
;;   (save-excursion
;;     (forward-char location)
;;     (looking-at char)))