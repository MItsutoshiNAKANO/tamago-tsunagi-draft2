;;; egg/sj3.el --- SJ3 Support (high level interface) in Egg
;;;                Input Method Architecture

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

(require 'egg)
(require 'egg-edep)

(defgroup sj3 nil
  "SJ3 interface for Tamago 4"
  :group 'egg)

(defcustom  sj3-hostname "localhost"
  "*Hostname of SJ3 server"
  :group 'sj3 :type 'string)

(defcustom  sj3-server-port 3086 
  "*Port number of SJ3 server"
  :group 'sj3 :type 'integer)


(eval-when-compile
  (defmacro SJ3-const (c)
    (cond ((eq c 'FileNotExist) 35)
	  )))

(setplist 'sj3-conversion-backend
	  '(egg-start-conversion          sj3-start-conversion
	    egg-get-bunsetsu-source       sj3-get-bunsetsu-source
	    egg-get-bunsetsu-converted    sj3-get-bunsetsu-converted
	    egg-get-source-language       sj3-get-source-language
	    egg-get-converted-language    sj3-get-converted-language
	    egg-list-candidates           sj3-list-candidates
	    egg-decide-candidate          sj3-decide-candidate
	    egg-change-bunsetsu-length    sj3-change-bunsetsu-length
	    egg-end-conversion            sj3-end-conversion))

(defconst sj3-backend-alist '((Japanese ((sj3-conversion-backend)))))

(egg-set-finalize-backend '(sj3-finalize-backend))

(defvar sj3-stdy-size 0 "STDYSIZE of SJ3 server")

(defvar sj3-open-message)

(defun sj3-open (hostname)
  "Establish the connection to SJ3 server.  Return process object."
  (let* ((buf (generate-new-buffer " *SJ3*"))
	 proc result)
    (condition-case err
	(setq proc (open-network-stream "SJ3" buf hostname sj3-server-port))
      ((error quit)
       (egg-error "failed to connect sj3 server")))
    (process-kill-without-query proc)
    (set-process-coding-system proc 'no-conversion 'no-conversion)
    (set-marker-insertion-type (process-mark proc) t)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (buffer-disable-undo)
      (set-buffer-multibyte nil))
    ;; Initialize dictionaries
    (setq sj3-sys-dict-list nil)
    (setq sj3-user-dict-list nil)
    (setq result (sj3rpc-open proc (system-name) (user-login-name)))
    (if (< result 0)
	(let ((msg (sj3rpc-get-error-message (- result))))
	  (delete-process proc)
	  (kill-buffer buf)
	  (egg-error "Can't open SJ3 session (%s): %s" hostname msg)))
    (setq result (sj3rpc-get-stdy-size proc))
    (if (< result 0)
	(let ((msg (sj3rpc-get-error-message (- result))))
	  (delete-process proc)
	  (kill-buffer buf)
	  (egg-error "Can't get SJ3 STDYSIZE: %s"msg)))
    (setq sj3-stdy-size result)
    proc))

;; (defun sj3-open (hostname-list)
;;   "Establish the connection to SJ3 server.  Return process object."
;;   (let* ((buf (generate-new-buffer " *SJ3*"))
;; 	 (msg-form "SJ3: connecting to sj3serv at %s...")
;; 	 hostname proc result msg)
;;     (save-excursion
;;       (set-buffer buf)
;;       (erase-buffer)
;;       (buffer-disable-undo)
;;       (setq enable-multibyte-characters nil))
;;     (cond
;;      ((null hostname-list)
;;       (setq hostname-list '("localhost")))
;;      ((null (listp hostname-list))
;;       (setq hostname-list (list hostname-list))))
;;     (while (and hostname-list (null proc))
;;       (setq hostname (car hostname-list)
;; 	    hostname-list (cdr hostname-list))
;;       (message msg-form hostname)
;;       (sit-for 0)
;;       (condition-case result
;; 	  (setq proc (open-network-stream "SJ3" buf hostname sj3-server-port))
;; 	(error nil))
;;       (if proc
;; 	  (progn
;; 	    (process-kill-without-query proc)
;; 	    (set-process-coding-system proc 'no-conversion 'no-conversion)
;; 	    (set-marker-insertion-type (process-mark proc) t)
;; 	    ;; Initialize dictionaries
;; 	    (setq sj3-sys-dict-list nil)
;; 	    (setq sj3-user-dict-list nil)
;; 	    (setq result (sj3rpc-open proc (system-name) (user-login-name)))
;; 	    (if (< result 0)
;; 		(progn
;; 		  (delete-process proc)
;; 		  (setq proc nil
;; 			msg (format "Can't open SJ3 session (%s): %s"
;; 				    hostname msg)))
;; 	      (setq result (sj3rpc-get-stdy-size proc))
;; 	      (if (< result 0)
;; 		  (progn
;; 		    (delete-process proc)
;; 		    (setq proc nil
;; 			  msg (format "Can't get SJ3 STDYSIZE: %s"
;; 				      (sj3rpc-get-error-message (- result)))))
;; 		(setq sj3-stdy-size result))))))
;;     (if proc
;; 	(progn
;; 	  (setq sj3-open-message (format (concat msg-form "done") hostname))
;; 	  proc)
;;       (kill-buffer buf)
;;       (error "%s" (or msg "no sj3serv available")))))

;; <env> ::= [ <proc> <dictionary-list> ]
(defvar sj3-environment nil
  "Environment for SJ3 kana-kanji conversion")

(defsubst sj3env-get-proc (env)
  (aref env 0))
(defsubst sj3env-get-dictionary-list (env)
  (aref env 1))

;; <sj3-bunsetsu> ::=
;;  [ <env> <source> <converted> <rest> <stdy>
;;    <zenkouho> <zenkouho-pos> <zenkouho-converted>
;;    <kugiri-changed> ]
(defsubst sj3-make-bunsetsu (env source converted rest stdy)
  (egg-bunsetsu-create
   'sj3-conversion-backend
   (vector env source converted rest stdy nil nil nil nil nil)))

(defsubst sj3bunsetsu-get-env (b)
  (aref (egg-bunsetsu-get-info b) 0))
(defsubst sj3bunsetsu-get-source (b)
  (aref (egg-bunsetsu-get-info b) 1))
(defsubst sj3bunsetsu-get-converted (b)
  (aref (egg-bunsetsu-get-info b) 2))
(defsubst sj3bunsetsu-get-rest (b)
  (aref (egg-bunsetsu-get-info b) 3))
(defsubst sj3bunsetsu-get-stdy (b)
  (aref (egg-bunsetsu-get-info b) 4))

(defsubst sj3bunsetsu-get-zenkouho (b)
  (aref (egg-bunsetsu-get-info b) 5))
(defsubst sj3bunsetsu-set-zenkouho (b z)
  (aset (egg-bunsetsu-get-info b) 5 z))

(defsubst sj3bunsetsu-get-zenkouho-pos (b)
  (aref (egg-bunsetsu-get-info b) 6))
(defsubst sj3bunsetsu-set-zenkouho-pos (b zp)
  (aset (egg-bunsetsu-get-info b) 6 zp))

(defsubst sj3bunsetsu-get-zenkouho-converted (b)
  (aref (egg-bunsetsu-get-info b) 7))
(defsubst sj3bunsetsu-set-zenkouho-converted (b zc)
  (aset (egg-bunsetsu-get-info b) 7 zc))

(defsubst sj3bunsetsu-get-kugiri-changed (b)
  (aref (egg-bunsetsu-get-info b) 8))
(defsubst sj3bunsetsu-set-kugiri-changed (b s)
  (aset (egg-bunsetsu-get-info b) 8 s))

(defun sj3-get-bunsetsu-source (b)
  (sj3bunsetsu-get-source b))

(defun sj3-get-bunsetsu-converted (b)
  (concat (sj3bunsetsu-get-converted b) (sj3bunsetsu-get-rest b)))

(defun sj3-get-source-language (b) 'Japanese)
(defun sj3-get-converted-language (b) 'Japanese)
(defun sj3-get-bunsetsu-stdy (b) (sj3bunsetsu-get-stdy b))

(defvar sj3-dictionary-specification
  '(("study.dat")
    ["sj3main.dic" ""]
    [("private.dic") ""])
  "Dictionary specification of SJ3.")

(defvar sj3-usr-dic-dir (concat "user/" (user-login-name))
  "*Directory of user dictionary for SJ3.")

(defun sj3-filename (p)
  ""
  (cond ((consp p) (concat sj3-usr-dic-dir "/" (car p)))
	(t p)))

(defun sj3-get-environment ()
  "Return the backend of SJ3 environment."
  (if sj3-environment
      sj3-environment
    (let* ((proc (sj3-open sj3-hostname))
	   (freq-info-name (sj3-filename (car sj3-dictionary-specification)))
	   (l (cdr sj3-dictionary-specification))
	   dict-list)
      (sj3-open-freq-info proc freq-info-name)
      (while l
	(let ((dic (car l))
	      dic-id)
	  (setq dic-id
		(sj3-open-dictionary proc (sj3-filename (aref dic 0))
				     (aref dic 1)))
	  (if (< dic-id 0)
	      (egg-error "Dame2")	; XXX
	    (setq dict-list (cons dic-id dict-list)
		  l (cdr l)))))
      (setq sj3-environment (vector proc dict-list)))))

(defun sj3-open-freq-info (proc name)
  (let ((trying t)
	ret)
    (while trying
      (setq ret (sj3rpc-open-stdy proc name))
      (if (= ret 0)
	  (setq trying nil)
	(message "学習ファイル(%s)がありません" name)
	(if (/= ret (SJ3-const FileNotExist))
	    (egg-error "Fatal1")	; XXX
	  (if (and (y-or-n-p
		    (format "学習ファイル(%s)がありません。作りますか? "
			    name))
		   (sj3rpc-make-directory proc
					  (file-name-directory name))
		   ;; ignore error
		   (= (sj3rpc-make-stdy proc name) 0))
	      (message "学習ファイル(%s)を作りました" name)
	    (egg-error "Fatal2")))))))	; XXX

(defun sj3-open-dictionary (proc name passwd)
  (let ((trying t)
	ret)
    (while trying
      (setq ret (sj3rpc-open-dictionary proc name passwd))
      (if (>= ret 0)
	  (setq trying nil)
	(message "辞書ファイル(%s)がありません" name)
	(setq ret (- ret))		; Get error code.
	(if (/= ret (SJ3-const FileNotExist))
	    (egg-error "Fatal3 %d" ret)	; XXX
	  (if (and (y-or-n-p
		    (format "辞書ファイル(%s)がありません。作りますか? "
			    name))
		   (= (sj3rpc-make-dictionary proc name) 0))
	      (message "辞書ファイル(%s)を作りました" name)
	    (egg-error "Fatal4")))))	; XXX
    ret))

(defun sj3-start-conversion (backend yomi &optional context)
  "Convert YOMI string to kanji, and enter conversion mode.
Return the list of bunsetsu."
  (let ((env (sj3-get-environment)))
    (sj3rpc-begin env yomi)))

(defun sj3-end-conversion (bunsetsu-list abort)
  (if abort
      ()
    (let ((env (sj3bunsetsu-get-env (car bunsetsu-list)))
	  (l bunsetsu-list)
	  bunsetsu stdy kugiri-changed)
      (while l
	(setq bunsetsu (car l))
	(setq l (cdr l))
	(setq stdy (sj3bunsetsu-get-stdy bunsetsu))
	(if stdy
	    (sj3rpc-bunsetsu-stdy env stdy))
	(if (setq kugiri-changed (sj3bunsetsu-get-kugiri-changed bunsetsu))
	    (let ((yomi1 (sj3bunsetsu-get-source bunsetsu))
		  (yomi2 (sj3bunsetsu-get-source (car l))))
	      (if (/= kugiri-changed (length yomi1))
		  (sj3rpc-kugiri-stdy env yomi1 yomi2
				      (sj3bunsetsu-get-stdy (car l))))))))))

(defun sj3-list-candidates (bunsetsu prev-bunsetsu next-bunsetsu major)
  (setq bunsetsu (car bunsetsu))
  (if (sj3bunsetsu-get-zenkouho bunsetsu)
      (cons (sj3bunsetsu-get-zenkouho-pos bunsetsu)
	    (sj3bunsetsu-get-zenkouho-converted bunsetsu))
    (let* ((env (sj3bunsetsu-get-env bunsetsu))
	   (yomi (sj3bunsetsu-get-source bunsetsu))
	   (z (sj3rpc-get-bunsetsu-candidates env yomi)))
      (sj3bunsetsu-set-zenkouho bunsetsu z)
      (cons (sj3bunsetsu-set-zenkouho-pos bunsetsu 0)
	    (sj3bunsetsu-set-zenkouho-converted
	     bunsetsu
	     (mapcar 'sj3bunsetsu-get-converted z))))))

(defun sj3-decide-candidate (bunsetsu candidate-pos prev-b next-b)
  (setq bunsetsu (car bunsetsu))
  (let* ((candidate-list (sj3bunsetsu-get-zenkouho bunsetsu))
	 (candidate (nth candidate-pos candidate-list)))
    (sj3bunsetsu-set-zenkouho candidate candidate-list)
    (sj3bunsetsu-set-zenkouho-pos candidate candidate-pos)
    (sj3bunsetsu-set-zenkouho-converted
     candidate (sj3bunsetsu-get-zenkouho-converted bunsetsu))
    (list (list candidate))))

(defun sj3-change-bunsetsu-length (bunsetsu prev-b next-b len major)
  (let ((yomi (apply 'concat (mapcar 'sj3bunsetsu-get-source bunsetsu)))
	(env (sj3bunsetsu-get-env (car bunsetsu)))
	(old (car bunsetsu))
	new yomi1 yomi2)
    (setq yomi1 (substring yomi 0 len)
	  yomi2 (substring yomi len))
    (setq new (sj3rpc-tanbunsetsu-conversion env yomi1))
    ;; Only set once (memory original length of the bunsetsu).
    (sj3bunsetsu-set-kugiri-changed new 
				    (or (sj3bunsetsu-get-kugiri-changed old)
					(length (sj3bunsetsu-get-source old))))
    (if (> (length yomi2) 0)
	(list (list new (sj3rpc-tanbunsetsu-conversion env yomi2)))
      (list (list new)))))

(defun sj3-finalize-backend ()
  (if sj3-environment
      (let ((proc (sj3env-get-proc sj3-environment))
	    (dict-list (sj3env-get-dictionary-list sj3-environment))
	    dict)
	(while dict-list
	  (setq dict (car dict-list))
	  (setq dict-list (cdr dict-list))
	  (sj3rpc-close-dictionary proc dict)) ; XXX: check error
	(sj3rpc-close-stdy proc)
	(sj3rpc-close proc)
	(setq sj3-environment nil))))

;;; setup

(load "egg/sj3rpc")
(run-hooks 'sj3-load-hook)

;;;###autoload
(defun egg-activate-sj3 (&rest arg)
  "Activate SJ3 backend of Tamagotchy."
  (apply 'egg-mode (append arg sj3-backend-alist)))

;;; egg/sj3.el ends here.
