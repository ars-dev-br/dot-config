(setq package-enable-at-startup nil)

(setq gc-cons-threshold (* 20 1024 1024)) ;; initial threshold
(add-hook 'emacs-startup-hook ;; threshold after init
          (lambda () (setq gc-cons-threshold (* 128 1024 1024))))

