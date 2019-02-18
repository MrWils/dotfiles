;; Copyright (C) 2018-2019 Robin Wils

;; Author: Robin Wils
;; Maintainer: Robin Wils
;; Created:
;; Version: 0.0.6

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

;; INITIALZE PACKAGES
(package-initialize)


;; REPOSITORIES
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;; INSTALL USE-PACKAGE IF NOT INSTALLED
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; REQUIRE
(eval-when-compile (require 'use-package))


;; VARIABLES
(setq
 ;; Personal information
 user-full-name "Robin Wils"
 user-mail-address "mrwils@tutanota.com"
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
 save-interprogram-paste-before-kill t
 ;; Indentation variables
 custom-tab-width 2
 big-custom-tab-width 4)


(setq-default
 ;; Mark column 80 in the header (Clean code matters)
 header-line-format (list " " (make-string 79 ?-) "|")
 ;; Forces the messages to 0
 message-log-max nil
 ;; Don't use tabs
 ;; use spaces instead
 indent-tabs-mode nil
 tab-width custom-tab-width)


;; GUI ELEMENTS
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
  ;; No fringes
  ;; (borders of buffers)
  (fringe-mode 0)
  
  ;; Code folding
  (allout-mode)
  ;; Syntax highlighting
  (global-font-lock-mode 1))


;; BETTER KEYS
;; No need for alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


;; SETUP SOME DEFAULTS
;; Kill the *Messages* buffer on startup
(if (get-buffer "*Messages*") (kill-buffer "*Messages*"))

;; Disable *Completions* buffer
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Replace typing "yes" or "no"
;; with typing "y" or "n"
(fset 'yes-or-no-p 'y-or-n-p)


;; INDENTATION
;; web-mode
;; html tag in html file
(setq-local web-mode-markup-indent-offset custom-tab-width)
;; css in html file
(setq-local web-mode-css-indent-offset custom-tab-width)
;; js code in html file
(setq-local web-mode-code-indent-offset custom-tab-width)

;; css-mode
(setq-local css-indent-offset custom-tab-width)

;; D-mode
(setq-local d-mode-indent-offset big-custom-tab-width)


;; THEME
(use-package darktooth-theme :ensure t :config (load-theme 'darktooth t))

;; Fonts
(set-frame-font "Hack-8" nil t)
(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive)))
  (mapc(lambda (face)
         (set-face-attribute face nil :font "Hack-10"))
       faces))


;; BUFFERS AND COMPLETIONS
;; Unique buffernames
(use-package uniquify)
;; I preffer ibuffer over list-buffers
(defalias 'list-buffers 'ibuffer-other-window)

;; ido mode - file and buffer completion
(use-package ido
  :config
  (setq indo-enable-flex-matching t
        ido-everywhere t)
  (ido-mode 1))

;; I use swiper to search.
;; Swiper uses counsel and ivy.
;; Ivy is a completion framework which uses the minibuffer.
;; Counsel is a collection of ivy enhanged version of common emacs
;; commands.
(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (define-key read-expression-map
    (kbd "C-r") 'counsel-expression-history))


;; Company - autocomplete for text and code
(use-package company
  :ensure t
  :diminish
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3)
  (global-company-mode 1))


;; SYNTAX CHECKING
;; Flycheck - syntax checker
(use-package flycheck :ensure t :config (global-flycheck-mode 1))


;; MORE PACKAGES
;; Magit
(use-package magit :ensure t :bind ("C-c g" . magit-status))

;; Web mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.mustache\\'" . web-mode))
  (add-to-list  'auto-mode-alist
                '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.xml?\\'" . web-mode)))

;; D-mode
(use-package d-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.d\\'" . d-mode)))

;; CSS
(use-package css-mode :defer t)

;; Beautify
(use-package web-beautify
  :ensure t
  :bind
  (:map web-mode-map ("C-c b" . web-beautify-html)
        :map js2-mode-map ("C-c b" . web-beautify-js)))

;; Projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-keymap-prefix)
  :config (projectile-mode 1))

;; Emacs bindings for browsers
;; https://github.com/stsquad/emacs_chrome
(use-package edit-server :ensure t)

;; Weather in emacs
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init (setq wttrin-default-cities '("Turnhout", "Hasselt")))

;; IRC - ERC
;; Use the erc-tls command to launch ERC
;; erc-tls uses SSL, erc doesn't,
;; TODO: add ZNC
(defalias 'erc 'erc-tls)
(use-package erc
  :defer t
  :config
  (setq
   ;; server to use if none is provided
   erc-server "irc.serverchan.club"
   ;; server which you can choose from in the menu
   erc-server-history-list
   '("irc.serverchan.club", "irc.lainchan.org")
   ;; port to use if none is provided
   erc-port 6697
   ;; nickname to use if none is provided
   erc-nick "rmw"
   ;; away nickname to use
   erc-away-nickname "rmw-away"
   ;; erc channels to autojoin
   erc-autojoin-channels-alist
   '(("serverchan.club" "#scoots")
     ("lainchan.org" "#lainchan"))))


;; EMMS
(use-package emms
  :ensure t
  :defer t
  :config
  (progn (require 'emms-setup)
         (emms-standard)
         (emms-default-players)))

;; EXWM
(use-package exwm
  :ensure t
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
;; if input is off, wrong screen turns black but I can move the cursor
;; if input auto, same thing happens

;; (use-package exwm-randr
;;   :after exwm
;;   :demand t
;;   :preface
;;   (defun exwm-change-screen-hook (primary-screen-mode)
;;     (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
;;           default-output)
;;       (with-temp-buffer
;;         (call-process "xrandr" nil t nil)
;;         (goto-char (point-min))
;;         (re-search-forward xrandr-output-regexp nil 'noerror)
;;         (setq default-output (match-string 1))
;;         (forward-line)
;;         (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
;;             (call-process "xrandr" nil nil nil "--output" default-output "--auto")
;;           (call-process
;;            "xrandr" nil nil nil
;;            "--output" (match-string 1) "--auto")
;;           "--output" default-output primary-screen-mode)
;;         (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1))))))
;;   :hook (exwm-randr-screen-change-hook . (lambda () (exwm-change-screen-hook "--off")))
;;   :init (lambda () (exwm-change-screen-hook "--auto"))
;;   :config (exwm-randr-enable))

;; Enable exwm
;; (exwm-enable)

;; TODO bind screen function to keys
;; (global-set-key (kbd "M-x ") (exwm-change-screen-hook "off"))
;; (global-set-key (kbd "C-P") (exwm-change-screen-hook "auto"))
