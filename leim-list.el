;;; leim-list-egg.el --- Egg setup for leim API

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: NIIBE Yutaka <gniibe@chroot.org>
;;         KATAYAMA Yoshio <kate@pfu.co.jp>

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

;;; leim-list-egg.el --- Egg setup for leim API
;;; CAUTION: Don't delete the above line.

(when site-run-file
  (autoload 'egg-activate-wnn "egg/wnn" "Activate Wnn backend of Tamagotchy." t)
  (autoload 'egg-activate-sj3 "egg/sj3" "Activate SJ3 backend of Tamagotchy." t)

  (register-input-method
   "japanese-egg-wnn" "Japanese" 'egg-activate-wnn
   "$B$"(B.."  "Romaji -> Hiragana -> Kanji&Kana"
   'its-select-hiragana)

  (register-input-method
   "japanese-egg-sj3" "Japanese" 'egg-activate-sj3
   "$B$"(B.."  "Romaji -> Hiragana -> Kanji&Kana"
   'its-select-hiragana)

  (register-input-method
   "chinese-gb-egg-wnn-py" "Chinese-GB" 'egg-activate-wnn
   "$AF4(BG"  "Pinyin -> Simplified Hanzi"
   'its-select-pinyin-cn)

  (register-input-method
   "chinese-gb-egg-wnn-zy" "Chinese-GB" 'egg-activate-wnn
   "$AW"(BG"  "Zhunyin -> Simplified Hanzi"
   'its-select-zhuyin-cn)

  (register-input-method
   "chinese-gb-egg-wnn-qm" "Chinese-GB" 'egg-activate-wnn
   "$AG.(B"  "QianMa Simplified Hanzi inputmethod"
   'its-select-qianma)

  (register-input-method
   "chinese-gb-egg-wnn-wb" "Chinese-GB" 'egg-activate-wnn
   "$ANe(B"  "WuBi Simplified Hanzi inputmethod"
   'its-select-wubi)

  (register-input-method
   "chinese-cns-egg-wnn-py" "Chinese-CNS" 'egg-activate-wnn
   "$(GQ;(BC"  "Pinyin -> Traditional Hanzi"
   'its-select-pinyin-tw)

  (register-input-method
   "chinese-cns-egg-wnn-zy" "Chinese-CNS" 'egg-activate-wnn
   "$(GNC(BC"  "Zhunyin -> Traditional Hanzi"
   'its-select-zhuyin-tw)

  (register-input-method
   "korean-egg-wnn" "Korean" 'egg-activate-wnn
   "$(CGQ(B"  "Hangul -> Hanja"
   'its-select-hangul)

  (autoload 'egg-mode "egg" "Toggle EGG  mode." t)

  (set-language-info "Japanese"    'input-method "japanese-egg-wnn")
  (set-language-info "Chinese-GB"  'input-method "chinese-gb-egg-wnn-py")
  (set-language-info "Chinese-CNS" 'input-method "chinese-cns-egg-wnn-py")
  (set-language-info "Korean"      'input-method "korean-egg-wnn")

  (require 'egg)
  (require 'its/hira)

;;;;

  (require 'egg-util)

  (defun load-leim-list-except-this ()
    (load-libraries "leim-list" (cdr-safe
				 (member (substring 
					  (file-name-directory
					   load-file-name)
					  0 -1)
					 load-path))))

  (message "Finished loading %s \n   and load others..." load-file-name)
  (load-leim-list-except-this)

  )