;;; egg/sj3rpc.el --- SJ3 Support (low level interface) in Egg
;;;                   Input Method Architecture

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



;; Only support SJ3 version 2.

(eval-when-compile
  (require 'egg-com)
;;  (load-library "egg/sj3")
  (defmacro sj3-const (c)
    (cond ((eq c 'OPEN)            1)
	  ((eq c 'CLOSE)           2)
	  ((eq c 'DICADD)         11)
	  ((eq c 'DICDEL)         12)
	  ((eq c 'OPENSTDY)       21)
	  ((eq c 'CLOSESTDY)      22)
	  ((eq c 'STDYSIZE)       23)
	  ((eq c 'LOCK)           31)
	  ((eq c 'UNLOCK)         32)
	  ((eq c 'BEGIN)          41)
	  ((eq c 'BEGIN_EUC)     111)
	  ((eq c 'TANCONV)        51)
	  ((eq c 'TANCONV_EUC)   112)
	  ((eq c 'KOUHO)          54)
	  ((eq c 'KOUHO_EUC)     115)
	  ((eq c 'KOUHOSU)        55)
	  ((eq c 'KOUHOSU_EUC)   116)
	  ((eq c 'STDY)           61)
	  ((eq c 'CLSTDY)         62)
	  ((eq c 'CLSTDY_EUC)    117)
	  ((eq c 'WREG)           71)
	  ((eq c 'WREG_EUC)      118)
	  ((eq c 'WDEL)           72)
	  ((eq c 'WDEL_EUC)      119)
	  ((eq c 'MKDIC)          81)
	  ((eq c 'MKSTDY)         82)
	  ((eq c 'MKDIR)          83)
	  ((eq c 'ACCESS)         84)
	  ((eq c 'WSCH)           91)
	  ((eq c 'WSCH_EUC)      120)
	  ((eq c 'WNSCH)          92)
	  ((eq c 'WNSCH_EUC)     121)
	  ((eq c 'VERSION)       103)
	  (t (error "No such constant")))))

;; XXX
(defconst sj3rpc-error-message (vector ))

(defun sj3rpc-get-error-message (errno)
  (or (and (>= errno 0)
	   (< errno (length sj3rpc-error-message))
	   (aref sj3rpc-error-message errno))
      (format "#%d" errno)))

(defmacro sj3rpc-call-with-environment (e vlist send-expr &rest receive-exprs)
  (let ((v (append
	    `((proc (sj3env-get-proc ,e)))
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

(defun sj3rpc-open (proc myhostname username)
  "Open the session.  Return 0 on success, error code on failure."
  (comm-call-with-proc proc (result)
    (comm-format (u u s s s) (sj3-const OPEN) 2 ; Server version
		 myhostname username
		 ;; program name
		 (format "%d.emacs-egg" (emacs-pid)))
    (comm-unpack (u) result)
    (if (= result -2)
	0
      result)))

(defun sj3rpc-close (proc)
  (comm-call-with-proc proc (result)
    (comm-format (u) (sj3-const CLOSE))
    (comm-unpack (u) result)
    result))

(defun sj3rpc-get-stdy-size (proc)
  "Return STDYSIZE of SJ3 server.  On failure, return error code."
  (comm-call-with-proc proc (result)
    (comm-format (u) (sj3-const STDYSIZE))
    (comm-unpack (u) result)
    (if (/= result 0)
	(- result)			; failure
      (comm-unpack (u) result)
      result)))

(defsubst sj3rpc-get-stdy (proc)
  (let ((n 0)
	(stdy (make-vector sj3-stdy-size 0)))
    (while (< n sj3-stdy-size)
      (comm-unpack (b) r)
      (aset stdy n r)
      (setq n (1+ n)))
    stdy))

(defun sj3rpc-begin (env yomi)
  "Begin conversion."
  (let ((yomi-ext (encode-coding-string yomi 'euc-japan))
	(p 0)
	len source converted stdy bunsetsu-list bl)
    (sj3rpc-call-with-environment env (result)
      (comm-format (u s) (sj3-const BEGIN_EUC) yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)			; failure
	(comm-unpack (u) result)	; skip
	(while (progn
		 (comm-unpack (b) len)
		 (> len 0))
	  (setq stdy (sj3rpc-get-stdy proc))
	  (comm-unpack (E) converted)
	  (setq source
		(decode-coding-string (substring yomi-ext p (+ p len))
				      'euc-japan)
		p (+ p len))
	  (let ((bl1 (cons (sj3-make-bunsetsu env
					      source converted nil stdy) nil)))
	    (if bl
		(setq bl (setcdr bl bl1))
	      (setq bunsetsu-list (setq bl bl1)))))
	bunsetsu-list))))

(defun sj3rpc-open-dictionary (proc dict-file-name password)
  (comm-call-with-proc proc (result)
    (comm-format (u s s) (sj3-const DICADD) dict-file-name password)
    (comm-unpack (u) result)
    (if (/= result 0)
	(- result)			; failure
      (comm-unpack (u) result)
      result)))

(defun sj3rpc-close-dictionary (proc dict-no)
  (comm-call-with-proc proc (result)
    (comm-format (u u) (sj3-const DICDEL) dict-no)
    (comm-unpack (u) result)
    result))

(defun sj3rpc-make-dictionary (proc dict-name)
  (comm-call-with-proc proc (result)
    (comm-format (u s u u u) (sj3-const MKDIC) dict-name
		 2048  ; Index length
		 2048  ; Length
		 256   ; Number
		 )
    (comm-unpack (u) result)
    result))

(defun sj3rpc-open-stdy (proc stdy-name)
  (comm-call-with-proc proc (result)
    (comm-format (u s s) (sj3-const OPENSTDY) stdy-name "")
    (comm-unpack (u) result)
    result))

(defun sj3rpc-close-stdy (proc)
  (comm-call-with-proc proc (result)
    (comm-format (u) (sj3-const CLOSESTDY))
    (comm-unpack (u) result)
    result))

(defun sj3rpc-make-stdy (proc stdy-name)
  (comm-call-with-proc proc (result)
    (comm-format (u s u u u) (sj3-const MKSTDY) stdy-name
		 2048  ; Number
		 1     ; Step
		 2048  ; Length
		 )
    (comm-unpack (u) result)
    result))

(defun sj3rpc-make-directory (proc name)
  (comm-call-with-proc proc (result)
    (comm-format (u s) (sj3-const MKDIR) name)
    (comm-unpack (u) result)
    result))

(defun sj3rpc-get-bunsetsu-candidates-sub (proc env yomi yomi-ext len n)
  (let ((i 0)
	stdy converted bunsetsu bl bunsetsu-list cylen rest)
    (comm-call-with-proc-1 proc (result)
      (comm-format (u u s) (sj3-const KOUHO_EUC) len yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)			; failure
	(while (< i n)
	  (comm-unpack (u) cylen)
	  (setq stdy (sj3rpc-get-stdy proc))
	  (comm-unpack (E) converted)
	  (setq rest (decode-coding-string
		      (substring yomi-ext cylen) 'euc-japan))
	  (setq bunsetsu (sj3-make-bunsetsu env yomi converted rest stdy))
	  (if bl
	      (setq bl (setcdr bl (cons bunsetsu nil)))
	    (setq bunsetsu-list (setq bl (cons bunsetsu nil))))
	  (setq i (1+ i)))
	(setq bunsetsu (sj3-make-bunsetsu env yomi yomi nil nil))
	(setq bl (setcdr bl (cons bunsetsu nil)))
	(setq bunsetsu
	      (sj3-make-bunsetsu env yomi (japanese-katakana yomi) nil nil))
	(setq bl (setcdr bl (cons bunsetsu nil)))
	bunsetsu-list))))

(defun sj3rpc-get-bunsetsu-candidates (env yomi)
  (let* ((yomi-ext (encode-coding-string yomi 'euc-japan))
	 (len (length yomi-ext)))
    (sj3rpc-call-with-environment env (result)
      (comm-format (u u s) (sj3-const KOUHOSU_EUC) len yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)			; failure
	(comm-unpack (u) result)
	(if (= result 0)
	    (list (sj3-make-bunsetsu env yomi yomi nil nil)) ; XXX
	  (sj3rpc-get-bunsetsu-candidates-sub proc env
					      yomi yomi-ext len result))))))

(defun sj3rpc-tanbunsetsu-conversion (env yomi)
  (let* ((yomi-ext (encode-coding-string yomi 'euc-japan))
	 (len (length yomi-ext)) cylen stdy converted rest)
    (sj3rpc-call-with-environment env (result)
      (comm-format (u u s) (sj3-const TANCONV_EUC) len yomi-ext)
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)
	(comm-unpack (u) cylen)
	(setq stdy (sj3rpc-get-stdy proc))
	(comm-unpack (E) converted)
	(setq rest (decode-coding-string
		    (substring yomi-ext cylen) 'euc-japan))
	(setq bunsetsu (sj3-make-bunsetsu env yomi converted rest stdy))))))

(defun sj3rpc-bunsetsu-stdy (env stdy)
  (sj3rpc-call-with-environment env (result)
     (comm-format (u v) (sj3-const STDY) stdy (length stdy))
     (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)
	0)))

(defun sj3rpc-kugiri-stdy (env yomi1 yomi2 stdy)
  (let* ((yomi1-ext (encode-coding-string yomi1 'euc-japan))
	 (yomi2-ext (encode-coding-string yomi2 'euc-japan)))
    (sj3rpc-call-with-environment env (result)
      (comm-format (u s s v) (sj3-const CLSTDY_EUC)
		   yomi1-ext yomi2-ext stdy (length stdy))
      (comm-unpack (u) result)
      (if (/= result 0)
	  (- result)
	0))))

;;; egg/sj3rpc.el ends here.
