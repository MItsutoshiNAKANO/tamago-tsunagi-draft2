;;; make-emacs-conf.el --- 

;; Copyright (C) 2000 ElectroTechnical Laboratory
;; Copyright (C) 2000 TOMURA Satoru <tomura@etl.go.jp>
;; Licensed to Free Software Foundation, Inc.

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

(defun make-emacs-configuration-file ()
  (with-temp-buffer
    (let* ((etc-dir data-directory)
	   (ver-dir (file-name-directory (substring etc-dir 0 -1)))
	   (emacs-dir (file-name-directory (substring ver-dir 0 -1))))
      (insert 
       ;;; INSDIR=/usr/local/share/emacs/site-lisp
       (format "INSDIR=%s/site-lisp\n"
	       (substring emacs-dir 0 -1))
       ;;; VINSDIR=/usr/local/share/emacs/20.5/site-lisp        
       (format "VINSDIR=%s/site-lisp\n"
	       (substring ver-dir 0 -1)))
      (write-file "dirs" nil))))

(make-emacs-configuration-file)
