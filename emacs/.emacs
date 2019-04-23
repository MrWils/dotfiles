;; LICENSE NOTICE
;; Copyright (C) 2018-2019 Robin Wils

;; Author: Robin Wils
;; Maintainer: Robin Wils
;; Created: 14 Jul, 2018
;; Version: 0.0.9

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
;; 1. Place config.org into your .emacs.d directory.
;; 2. Replace your .emacs file with my .emacs one.
;; (It is recommended to backup your .emacs file before you do this)

;;; Code:

;; LOAD EMACS CONFIG
(org-babel-load-file
 (expand-file-name "emacs-init.org"
                   user-emacs-directory))
