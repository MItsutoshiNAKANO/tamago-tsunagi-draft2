;;; egg/cannarpc.el --- Canna Support (low level interface) in
;;;                     Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: NIIBE Yutaka <gniibe@chroot.org>

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



(eval-when-compile
  (require 'egg-com)
;;  (load-library "egg/canna")
  (defmacro canna-const (c)
    (cond ((eq c 'Initialize)            1)
	  ((eq c 'Finalize)              2)
	  ((eq c 'CreateContext)         3)
	  ((eq c 'CloseContext)          5)
	  ((eq c 'GetDictionaryList)     6)
	  ((eq c 'GetDirectoryList)      7)
	  ((eq c 'MountDictionary)       8)
	  ((eq c 'UnmountDictionary)       9)
	  ((eq c 'BeginConvert)         15)
	  ((eq c 'EndConvert)           16)
	  ((eq c 'GetCandidacyList)     17)
	  ((eq c 'GetYomi)              18)
	  ((eq c 'ResizePause)          26)

	  ((eq c 'CreateDictionary)      3)
	  (t (error "No such constant")))))

(defun cannarpc-get-error-message (errno)
  (or (aref cannarpc-error-message errno) (format "#%d" errno)))

(defmacro cannarpc-call-with-environment (e vlist send-expr &rest receive-exprs)
  (let ((v (append
	    `((proc (cannaenv-get-proc ,e))
	      (context (cannaenv-get-context ,e)))
	    vlist)))
    (list
     'let v
     (append
	`(save-excursion
	   (set-buffer (process-buffer proc))
	   (erase-buffer)
	   ,send-expr
	   (process-send-region proc (point-min) (point-max))
	   (goto-char (prog1 (point) (accept-process-output proc))))
	receive-exprs))))

(defconst canna-version-fmt "2.0:%s")

(defun cannarpc-open (proc username)
  "Open the session.  Return 0 on success, error code on failure."
  (let ((verusr (format canna-version-fmt username)))
    (comm-call-with-proc proc (result)
      (comm-format (u u v) (canna-const Initialize) (length verusr) verusr)
      (comm-unpack (u) result)
      result)))

(defun cannarpc-close (proc)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w) (canna-const Finalize) 0 0)
    (comm-unpack (b b w b) dummy dummy dummy result)
    result))

(defun cannarpc-create-context (proc)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w) (canna-const CreateContext) 0 0)
    (comm-unpack (b b w w) dummy dummy dummy result)
    result))

(defun cannarpc-close-context (proc context)
  (comm-call-with-proc proc (dummy result)
    (comm-format (b b w w) (canna-const CloseContext) 0 2 context)
    (comm-unpack (b b w b) dummy dummy dummy result)
    result))

;; XXX: Not implemented fully
(defun cannarpc-get-dictionary-list (env)
  (cannarpc-call-with-environment env (dymmy result)
    (comm-format (b b w w w) (canna-const GetDictionaryList) 0 4
		 context 1024)
    (comm-unpack (u w) dummy result)
    ;; follow list of dictionaries
    result))

;; XXX: Not implemented fully
(defun cannarpc-get-directory-list (env)
  (cannarpc-call-with-environment env (dymmy result)
    (comm-format (b b w w w) (canna-const GetDirectoryList) 0 4
		 context 1024)
    (comm-unpack (u w) dummy result)
    ;; follow list of directories
    result))

(defun cannarpc-open-dictionary (env dict-file-name mode)
  (cannarpc-call-with-environment env (dymmy result)
    (comm-format (b b w u w s) (canna-const MountDictionary) 0
		 (+ (length dict-file-name) 7)
		 mode context dict-file-name)
    (comm-unpack (u b) dummy result)
    result))

(defun cannarpc-close-dictionary (env dict-file-name mode)
  (cannarpc-call-with-environment env (dymmy result)
    (comm-format (b b w u w s) (canna-const UnmountDictionary) 0
		 (+ (length dict-file-name) 6)
		 mode context dict-file-name)
    (comm-unpack (u b) dummy result)
    result))

(defun cannarpc-begin-conversion (env yomi)
  "Begin conversion."
  (let ((yomi-ext (encode-coding-string yomi 'euc-japan))
	(i 0)
	converted bunsetsu-list bl)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w u w S) (canna-const BeginConvert) 0
		   (+ (length yomi-ext) 8) 0 context yomi)
      (comm-unpack (u w) dummy result)
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (S) converted)
	  (let ((bl1 (cons (canna-make-bunsetsu env converted i)
			   nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1))))
	  (setq i (1+ i)))
	bunsetsu-list))))

(defun cannarpc-end-conversion (env len zenkouho-pos-vector mode)
  "End conversion."
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w u v) (canna-const EndConvert) 0
		 (+ (* len 2) 8) context len mode zenkouho-pos-vector)
    (comm-unpack (u b) dummy result)
    (if (= result 255)
	-1				; failure
      result)))

(defun cannarpc-make-dictionary (env dict-name)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w u w s) (canna-const CreateDictionary) 1
		 (+ (length dict-name) 7) 0 context dict-name)
    (comm-unpack (u b) dummy result)
    result))

(defun cannarpc-get-bunsetsu-source (env bunsetsu-pos)
  (cannarpc-call-with-environment env (dummy result)
    (comm-format (b b w w w w) (canna-const GetYomi) 0 6 context
		 bunsetsu-pos 1024)
    (comm-unpack (u w) dummy result)
    (if (= result 65535)
	-1
      (comm-unpack (S) result)
      result)))

(defun cannarpc-get-bunsetsu-candidates (env bunsetsu-pos)
  (let ((i 0)
	converted bunsetsu-list bl)
    (cannarpc-call-with-environment env (dummy result)
      (comm-format (b b w w w w) (canna-const GetCandidacyList) 0 6 context
		   bunsetsu-pos 1024)
      (comm-unpack (u w) dymmy result)
      (if (= result 65535)
	  -1				; failure
	(while (< i result)
	  (comm-unpack (S) converted)
	  (let ((bl1 (cons (canna-make-bunsetsu env converted bunsetsu-pos)
			   nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1))))
	  (setq i (1+ i)))
	bunsetsu-list))))

;;; egg/cannarpc.el ends here.
