;; Copyright (C) 2018-2019 Robin Wils

;; Author: Robin Wils
;; Maintainer: Robin Wils
;; Created:
;; Version: 0.0.8

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


;; LESS GARBAGE COLLECTION DURING STARTUP
;; Emacs should load faster if with this configured. This is only recommended if
;; your machine has enough RAM, It should work on the most modern machines.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold 800000)))


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
 ;; Empty scratch buffer
 initial-scratch-message nil
 ;; End sentences with one space
 sentence-end-double-space nil
 ;; No backups
 make-backup-files nil
 ;; Disable dialogs
 use-dialog-box nil
 ;; Disable emacs beep
 visible-bell 1
 ;; Indentation variables
 custom-tab-width 2
 big-custom-tab-width 4)


(setq-default
 ;; Mark column 80 in the header (Clean code matters)
 header-line-format (list " " (make-string 79 ?-) "|")
 ;; Forces the messages to 0
 message-log-max nil
 ;; Set the tab-width
 tab-width custom-tab-width
 ;; Don't use tabs
 ;; use spaces instead
 indent-tabs-mode nil)


;; GUI ELEMENTS
;; No cursor blink
(blink-cursor-mode 0)


;; BETTER DEFAULTS
;; Adds some sane defaults
;; https://github.com/technomancy/better-defaults
(use-package better-defaults :ensure t)

(use-package better-shell
  :ensure t
  :bind
  (("C-'" . better-shell-shell)
   ("C-;" . better-shell-remote-open)))

;; better dired
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (dired-sort-toggle-or-edit)))

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

;; Make it possible to hide minor modes
(use-package diminish :ensure t)

;; I usually want to delete more whitespace than emacs wants to,
;; this fixes that.
(use-package hungry-delete :ensure t :config (global-hungry-delete-mode))


;; INDENTATION
;; Force indentation a bit, it does not force it on minimized/compressed files
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode))


;; THEME
(use-package darktooth-theme :ensure t :config (load-theme 'darktooth t))

;; Emacs font
(add-to-list 'default-frame-alist
             '(font . "Hack-8"))


;; BUFFERS AND COMPLETIONS
;; Unique buffernames
(use-package uniquify)

;; ido mode - file and buffer completion
(use-package ido
  :config
  (setq indo-enable-flex-matching t
        ido-everywhere t)
  (ido-mode 1))
;; Misc collection of ido changes,
;; including making it behave better with dired’s copying and
;; renaming commands (such as putting directory as first option).
(use-package ido-hacks :ensure t)

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


;; Yasnippet - template complation for many programming languages
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))
;; Add some snippets
(use-package yasnippet-snippets :ensure t)


;; SYNTAX CHECKING
;; Flycheck - syntax checker
(use-package flycheck :ensure t :config (global-flycheck-mode 1))


;; PROGRAMMING

;; Useful hook functions
(defun untabify-whole-buffer()
  "Untabifies a whole buffer."
  (untabify (point-min) (point-max)))

;; C#
;; (use-package dotnet :ensure t)

;; (use-package csharp-mode
;;   :ensure t
;;   :bind (("C-c r r" . 'omnisharp-run-code-action-refactoring)
;;          ("C-c C-c" . 'recompile))
;;   :hook (('csharp-mode-hook . omnisharp-mode)
;;          ('csharp-mode-hook . company-mode)
;;          ('csharp-mode-hook . dotnet-mode)
;;          ('csharp-mode-hook . flycheck-mode)
;;          ('csharp-mode-hook . 'neotree-show))
;;   :config
;;   (add-to-list 'auto-mode-alist
;;                '("\\.cs\\'" . csharp-mode))
;;   (setq tab-width big-custom-tab-width))

;; (use-package omnisharp :ensure t)
;; ;; Autocompletion
;; (eval-after-load
;;     'company
;;   '(add-to-list 'company-backends
;;                 #'company-omnisharp))


;; LISP
(use-package slime
  :ensure t
  :commands (slime slime-lisp-mode-hook)
  :config
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy)))
;; autocomplete for text and code
(use-package slime-company :ensure t)

;; Web mode
(use-package web-mode
  :ensure t
  :init
  (defun web-mode-hook ()
    "Hook for web-mode."
    ;; HTML indentation
    (setq-local web-mode-markup-indent-offset custom-tab-width)
    ;; CSS indentation
    (setq-local web-mode-css-indent-offset custom-tab-width)
    ;; JS indentat ion
    (setq-local web-mode-code-indent-offset custom-tab-width))
  :hook ((web-mode . web-mode-hook)
         (web-mode . 'untabify-whole-buffer))
  :config
  (add-to-list 'auto-mode-alist
               '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("\\.xml?\\'" . web-mode)))

;; D-mode
;; (use-package d-mode
;;   :ensure t
;;   :init
;;   (defun d-mode-hook ()
;;     "Hook for d-mode."
;;     (setq-local d-mode-indent-offset
;;                 big-custom-tab-width))
;;   :hook (d-mode . d-mode-hook)
;;   :config
;;   (add-to-list 'auto-mode-alist
;;                '("\\.d\\'" . d-mode)))


;; MORE PACKAGES
;; Magit - git for emacs
(use-package magit :ensure t :bind ("C-c g" . magit-status))

;; Projectile - make it easier to jump to files in a project
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-keymap-prefix)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

;; Emacs bindings for browsers
;; https://github.com/stsquad/emacs_chrome
;; Does not work on icecat, right now.
;; (use-package edit-server :ensure t)

;; Writegood mode - find common writing problems
(use-package writegood-mode
  :ensure t
  :bind ("C-c w" . writegood-mode))

;; Elfeed - RSS reader
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("http://vault.lunduke.com/LundukeShowMP3.xml")))

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
;; Display track titles as scrolling text
(use-package emms-mode-line-cycle
  :ensure t
  :config
  (emms-mode-line 1)
  (emms-playing-time 1)
  (emms-mode-line-cycle 1))

;; PDF-support
(use-package pdf-tools :ensure t)
(use-package org-pdfview :ensure t)

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

;; EXWM keys
;; Global EXWM keybindings
;; next buffer
(exwm-input-set-key (kbd "C-<tab>") 'next-buffer)
;; TODO: make this command togglable
(exwm-input-set-key (kbd "s-<tab>") 'exwm-input-toggle-keyboard)

;; System tray
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

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
