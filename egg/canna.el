;;; egg/canna.el --- Canna Support (high level interface) in
;;;                  Egg Input Method Architecture

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


(require 'egg-edep)

(eval-when-compile
  (defmacro CANNA-const (c)
    (cond ((eq c 'FileNotExist) xxxxxxxxxxxxxx)
	  )))

(defconst canna-conversion-backend
  [ canna-init

    canna-start-conversion
      canna-get-bunsetsu-converted
      canna-get-bunsetsu-source
      canna-list-candidates
          canna-get-number-of-candidates
          canna-get-current-candidate-number
          canna-get-all-candidates
          canna-decide-candidate
      canna-change-bunsetsu-length
    canna-end-conversion
    nil

    canna-fini
 ])

(defconst canna-server-port 5680 "Port number of Canna server")
(defvar canna-hostname "localhost"
  "Hostname of Canna server")

(defun canna-open (hostname)
  "Establish the connection to CANNA server.  Return environment object."
  (let* ((buf (generate-new-buffer " *CANNA*"))
	 (proc (open-network-stream "CANNA" buf hostname canna-server-port))
	 result)
    (process-kill-without-query proc)
    (set-process-coding-system proc 'no-conversion 'no-conversion)
    (set-marker-insertion-type (process-mark proc) t)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (buffer-disable-undo)
      (set-buffer-multibyte nil))
    (setq result (cannarpc-open proc (user-login-name)))
    (if (< result 0)
	(let ((msg (cannarpc-get-error-message (- result))))
	  (delete-process proc)
	  (kill-buffer buf)
	  (error "Can't open CANNA session (%s): %s" hostname msg)))
    (vector proc result)))

;; XXX: Should support multiple outstanding context
;; <env> ::= [ <proc> <context> ]
(defvar canna-environment nil
  "Environment for CANNA kana-kanji conversion")

(defsubst cannaenv-get-proc (env)
  (aref env 0))
(defsubst cannaenv-get-context (env)
  (aref env 1))

;; <bunsetsu> ::=
;;  [ <env> <converted> <bunsetsu-pos>
;;    <source> <zenkouho-pos> <zenkouho> ]
(defsubst canna-make-bunsetsu (env converted bunsetsu-pos)
  (vector env converted bunsetsu-pos nil nil nil))

(defsubst cannabunsetsu-get-env (b)
  (aref b 0))
(defsubst cannabunsetsu-get-converted (b)
  (aref b 1))
(defsubst cannabunsetsu-get-bunsetsu-pos (b)
  (aref b 2))
(defsubst cannabunsetsu-get-source (b)
  (aref b 3))
(defsubst cannabunsetsu-set-source (b s)
  (aset b 3 s))
(defsubst cannabunsetsu-get-zenkouho-pos (b)
  (aref b 4))
(defsubst cannabunsetsu-set-zenkouho-pos (b p)
  (aset b 4 p))
(defsubst cannabunsetsu-get-zenkouho (b)
  (aref b 5))
(defsubst cannabunsetsu-set-zenkouho (b z)
  (aset b 5 z))

(defun canna-get-bunsetsu-source (b)
  (let ((s (cannabunsetsu-get-source b)))
    (or s
	(let* ((env (cannabunsetsu-get-env b))
	       (bp (cannabunsetsu-get-bunsetsu-pos b))
	       (s (cannarpc-get-bunsetsu-source env bp)))
	  (cannabunsetsu-set-source b s)))))

(defun canna-get-bunsetsu-converted (b)
  (cannabunsetsu-get-converted b))

(defconst canna-dictionary-specification
  '("iroha"
    "fuzokugo"
    "hojomwd"
    "hojoswd"
    "bushu"
    "user"
    )
  "Dictionary specification of CANNA.")

(defun canna-filename (p)
  ""
  (cond ((consp p) (concat (car p) "/" (user-login-name)))
	(t p)))

(defun canna-get-environment ()
  "Return the backend of CANNA environment."
  (if canna-environment
      canna-environment
    (let* ((env (canna-open canna-hostname))
	   (l canna-dictionary-specification)
	   dict-list)
      (while l
	(let ((dic (car l))
	      result)
	  (setq result
		(canna-open-dictionary env (canna-filename dic)))
	  (if (= result 255)
	      (error "Damedamedame")		; XXX
	    (setq l (cdr l)))))
      (setq canna-environment env))))

(defun canna-open-dictionary (env name)
  (let ((trying t)
	ret)
    (while trying
      (setq ret (cannarpc-open-dictionary env name 0)) ; XXX MODE=0
      (if (= ret 0)
	  (setq trying nil)
	(message "辞書ファイル(%s)がありません" name)
	(setq ret (- ret))		; Get error code.
	(if (and (y-or-n-p
		  (format "辞書ファイル(%s)がありません。作りますか? "
			  name))
		 (= (cannarpc-make-dictionary env name) 0))
	    (message "辞書ファイル(%s)を作りました" name)
	  (error "Fatal"))))
    ret))

(defun canna-init ()
  )

(defun canna-start-conversion (yomi lang)
  "Convert YOMI string to kanji, and enter conversion mode.
Return the list of bunsetsu."
  (if (eq lang 'Japanese)
      (let ((env (canna-get-environment)))
	(cannarpc-begin-conversion env yomi))
    (signal 'lang-not-supported)))    

(defun canna-end-conversion (bunsetsu-list abort)
  (let* ((env (cannabunsetsu-get-env (car bunsetsu-list)))
	 (l bunsetsu-list)
	 (len (length bunsetsu-list))
	 (zenkouho-pos-vector (make-vector (* 2 len) 0))
	 (i 0)
	 (mode 1) ;XXX MODE=1 attru?
	 bunsetsu zenkouho-pos)
    (if abort
	(setq mode 0))
    (while l
      (setq bunsetsu (car l))
      (setq l (cdr l))
      (setq zenkouho-pos (cannabunsetsu-get-zenkouho-pos bunsetsu))
      (if (null zenkouho-pos)
	  () ; XXX: NIL--> 0 atteru???
	(aset zenkouho-pos-vector i 0)	; XXX Don't support >=256
	(aset zenkouho-pos-vector (1+ i) zenkouho-pos))
      (setq i (+ i 2)))
    (cannarpc-end-conversion env len zenkouho-pos-vector 0)))

(defun canna-list-candidates (bunsetsu prev-bunsetsu)
  (let* ((env (cannabunsetsu-get-env bunsetsu))
	 (bunsetsu-pos (cannabunsetsu-get-bunsetsu-pos bunsetsu))
	 (z (cannarpc-get-bunsetsu-candidates env bunsetsu-pos)))
    (cannabunsetsu-set-zenkouho bunsetsu z)
    (cannabunsetsu-set-zenkouho-pos bunsetsu 0)
    0))

(defun canna-get-number-of-candidates (bunsetsu)
  (let ((l (cannabunsetsu-get-zenkouho bunsetsu)))
    (if l
	(length l)
      nil)))

(defun canna-decide-candidate (bunsetsu candidate-pos)
  (let* ((candidate-list (cannabunsetsu-get-zenkouho bunsetsu))
	 (candidate (nth candidate-pos candidate-list)))
    (cannabunsetsu-set-zenkouho candidate candidate-list)
    (cannabunsetsu-set-zenkouho-pos candidate candidate-pos)
    candidate))

(defun canna-get-current-candidate-number (bunsetsu)
  (cannabunsetsu-get-zenkouho-pos bunsetsu))

(defun canna-get-all-candidates (bunsetsu)
  (let* ((l (cannabunsetsu-get-zenkouho bunsetsu))
	 (result (cons nil nil))
	 (r result))
    (catch 'break
      (while t
	(let ((candidate (car l)))
	  (setcar r (cannabunsetsu-get-converted candidate))
	  (if (null (setq l (cdr l)))
	      (throw 'break nil)
	    (setq r (setcdr r (cons nil nil)))))))
    result))

;;;;;;;;;;;;;;;;;;;;;;; MADAMADA zenzendame, just copy from SJ3
(defun canna-change-bunsetsu-length (b0 b1 b2 len)
  (let ((yomi (concat
	       (cannabunsetsu-get-source b1)
	       (if b2 (cannabunsetsu-get-source b2))))
	(env (cannabunsetsu-get-env b1))
	yomi1 yomi2
	bunsetsu1 bunsetsu2)
    (setq yomi1 (substring yomi 0 len)
	  yomi2 (substring yomi len))
    (setq bunsetsu1
	  (cannarpc-tanbunsetsu-conversion env yomi1))
    ;; Only set once (memory original length of the bunsetsu).
    (cannabunsetsu-set-kugiri-changed bunsetsu1
				    (or (cannabunsetsu-get-kugiri-changed b1)
					(length (cannabunsetsu-get-source b1))))
    (if (< 0 (length yomi2))
	(setq bunsetsu2 (cannarpc-tanbunsetsu-conversion env yomi2))
      (setq bunsetsu2 nil))
    (if bunsetsu2
	(list bunsetsu1 bunsetsu2)
      (list bunsetsu1))))

;;;;;;;;;;;;;; MADAMADA
(defun canna-fini ()
)

;;; setup
(require 'egg)

;;;###autoload
(defun egg-activate-canna (&rest arg)
  "Activate CANNA backend of Tamagotchy."
  (setq egg-conversion-backend canna-conversion-backend)
  (if (not (fboundp 'cannarpc-open))
      (load-library "egg/canna"))
  (apply 'egg-mode arg))

;;; egg/canna.el ends here.
