;;; egg-util.el --- Utilities with Egg

;; Copyright (C) 2000 ElectroTechinical Laboratory, Japan
;; Copyright (C) 2000 TOMURA Satoru <tomura@etl.go.jp>

;; Author: TOMURA Satoru <tomura@etl.go.jp>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

;; Keywords: mule, multilingual, input method

;; This file is part of EGG.

;; EGG is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; EGG is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(provide 'egg-util)

(defun add-directory-to-load-path (dir)
  (let ((dir (expand-file-name dir)))
    (add-to-list 'load-path dir)
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path))))

(defun locate-libraries (library &optional nosuffix path interactive-call)
  (let ((lpath (or path load-path))
	(result nil))
    (while lpath
      (let ((path (locate-library library nosuffix lpath interactive-call)))
	(if path
	    (progn
	      (setq lpath (cdr-safe 
			   (member (directory-file-name (file-name-directory path))
				   lpath))
		    result (cons path result)))
	  (progn
	    (setq lpath nil
		  result (reverse result))))))
    result))

(defun load-libraries (library &optional path)
  (let ((files (locate-libraries library nil (or path load-path) nil)))
    (while files
      (load-file (car files))
      (setq files (cdr files)))))

