;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors 'silent)

(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024)
                  gc-cons-percentage 0.1)))
