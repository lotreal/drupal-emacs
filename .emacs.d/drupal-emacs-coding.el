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