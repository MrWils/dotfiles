;; Copyright (C) 2018 Robin Wils

;; Author: Robin Wils
;; Maintainer: Robin Wils
;; Created:
;; Version: 0.0.1

;; This config is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This config is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this config. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Quick Start / installation:
;; 1. Replace your .emacs file with this one.
;; (It is recommended to backup your .emacs file before you do this)

;;; Code:

(package-initialize)

;; Melpa repository
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Always make sure that all packages are installed
;; (setq use-package-always-ensure t)


;; Default variables
(setq
 ;; Personal information
 user-full-name "Robin Wils"
 user-mail-address "mrwils@protonmail.com"
 ;; No startup message.
 inhibit-startup-message t
 ;; No startup-screen
 inhibit-startup-screen t
 ;; Hide the cursor in inactive windows
 cursor-in-non-selected-windows t
 ;; Empty scratch buffer
 initial-scratch-message nil
 ;; No splash screen on startup
 inhibit-splash-screen t
 ;; End sentences with one space
 sentence-end-double-space nil
 ;; No backups
 make-backup-files nil
 ;; Disable dialogs
 use-dialog-box nil
 ;; Integrate with X clipboard
 select-enable-clipboard t
 ;; Ensure clipboard strings are saved into kill ring
 save-interprogram-paste-before-kill t)


(when window-system
  ;; Disable GUI elements
  ;; No toolbar
  (tool-bar-mode 0)
  ;; No scrollbar
  (scroll-bar-mode 0)
  ;; No toolbar
  (tooltip-mode 0)
  ;; No cursor blink
  (blink-cursor-mode 0)
  
  ;; Code folding
  (allout-mode)
  ;; Syntax highlighting
  (global-font-lock-mode 1))

;; Better keys
;; No need for alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; TODO: Kill words with backspace
;; (global-set-key [] 'backward-kill-word)

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(if (get-buffer "*Messages*") (kill-buffer "*Messages*"))

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))


;; Indentation
(setq custom-tab-width 2)

;; For H mode
(setq big-custom-tab-width 4)

;; Don't use tabs
(setq-default indent-tabs-mode nil
              tab-width custom-tab-width)

;; web-mode, html tag in html file
(setq-local web-mode-markup-indent-offset custom-tab-width)
;; web-mode, css in html file
(setq-local web-mode-css-indent-offset custom-tab-width)
;; web-mode, js code in html file
(setq-local web-mode-code-indent-offset custom-tab-width)
;; css-mode
(setq-local css-indent-offset custom-tab-width)
;; D-mode
(setq-local d-mode-indent-offset big-custom-tab-width)


;; Theme
(use-package darktooth-theme)
(load-theme 'darktooth t)
;; Fonts
(set-frame-font "Hack-9" nil t)
(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive)))
  (mapc
   (lambda (face) (set-face-attribute face nil :font "Hack-11"))
   faces))

;; Unique buffernames
(use-package uniquify)

;; Magit
(use-package magit :bind ("C-c g" . magit-status))

;; Web mode
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode)))

;; D mode
(use-package d-mode
  :config
  (setq d-mode-markup-indent-offset 4)
  (add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode)))

;; CSS
(use-package css-mode :defer t)

;; Beautify
(use-package web-beautify
  :ensure t
  :bind (:map web-mode-map ("C-c b" . web-beautify-html)
              :map js2-mode-map ("C-c b" . web-beautify-js)))

;; Syntax checking - flycheck
(use-package flycheck
  :ensure t
  :config
  ;; enable flycheck mode in all buffers
  (global-flycheck-mode 1))

;; Auto-complete and code suggestions
;; Auto-complete
(use-package auto-complete
  ;; Avoid competing with org-mode templates.
  :hook (org-mode-hook . (lambda ()
                           (make-local-variable 'ac-stop-words)
                           (loop for template in org-structure-template-alist do
                                 (add-to-list 'ac-stop-words 
                                              (concat "<" (car template)))))))

; Use ido mode for file and buffer Completion when switching buffers
(use-package ido
  :config (ido-mode t))

;; Company
(use-package company)
;; (use-package git-complete)

;; Projectile
(use-package projectile
  :ensure t
  ;; use a different prefix
  :bind ("C-c p" . projectile-keymap-prefix)
  :config
  ;; enable projectile mode in all buffers
  (projectile-global-mode 1))

;; Weather in emacs
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Turnhout"
                                "Hasselt")))

;; IRC - ERC
(use-package erc
  :defer t
  :config (progn (setq erc-hide-list '("JOIN" "PART" "QUIT")
                       erc-nick "RMW"
                       erc-port 6665
                       erc-server "irc.freenode.net")))

;; MENUS AND COMPLETION (not code completion)

;; Use minimalist Ivy for most things.
;; (use-package ivy
;;   :diminish
;;   :config
;; (ivy-mode 1))

;; EXWM
(use-package exwm
  :demand t
  :hook ((exwm-init . display-battery-mode)
         (exwm-init . display-time-mode)
         (exwm-init . column-number-mode)
         (exwm-init . line-number-mode)
         (exwm-init . show-paren-mode)
         (exwm-init . whitespace-mode)))

(use-package exwm-config :after exwm :demand t)
(exwm-config-default)

;; Fix problems with function
;; (use-package exwm-randr
;;   :after exwm
;;   :demand t
;;   :preface
;;   (defun exwm-change-screen-hook (primary-screen-mode)
;;   (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
;;         default-output)
;;     (with-temp-buffer
;;       (call-process "xrandr" nil t nil)
;;       (goto-char (point-min))
;;       (re-search-forward xrandr-output-regexp nil 'noerror)
;;       (setq default-output (match-string 1))
;;       (forward-line)
;;       (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
;;           (call-process "xrandr" nil nil nil "--output" default-output "--auto")
;;         (call-process
;;          "xrandr" nil nil nil
;;          "--output" (match-string 1) "--auto")
;;          ;;"--output" default-output primary-screen-mode)
;;         (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))
;;    :hook (exwm-randr-screen-change-hook . (lambda () (exwm-change-screen-hook "--off")))
;;    :init (lambda () (exwm-change-screen-hook "--off"))
;;    :config (exwm-randr-enable))

;; TODO bind screen function to keys
;; (global-set-key (kbd "M-x ") (exwm-change-screen-hook "off"))
;; (global-set-key (kbd "C-P") (exwm-change-screen-hook "auto"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-go go-complete go-autocomplete wttrin web-mode web-beautify use-package typit smart-mode-line projectile magithub exwm emms dired-du darktooth-theme d-mode company-dcd auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 16777215)) (:foreground "#FDF4C1" :background "#282828")) (((class color) (min-colors 255)) (:foreground "#ffffaf" :background "#262626")))))
