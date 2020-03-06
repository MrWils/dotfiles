;; LICENSE NOTICE
;; Copyright (C) 2018-2020 Robin Wils

;; Author: Robin Wils
;; Maintainer: Robin Wils
;; Created: 14 Jul, 2018
;; Version: 0.1.2

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
;; 1. Place emacs-init.org into your .emacs.d directory.
;; 2. Replace your .emacs file with my .emacs one.
;; (It is recommended to backup your .emacs file before you do this)

;;; Code:

;; LOAD EMACS CONFIG

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file
 (expand-file-name "emacs-init.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(lua-mode org-mode markdown-mode xah-fly-keys ox-hugo org yasnippet-snippets wttrin writegood-mode web-mode use-package slime-company projectile org-pdfview magit ido-hacks hungry-delete htmlize flycheck emms-mode-line-cycle elfeed diminish darktooth-theme counsel better-shell better-defaults auto-package-update aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
