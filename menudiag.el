;;; menudiag.el --- Minibuffer Menu System

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
;; Inspired by the menu subsystem of EGG V3.0
;;
;; Completely different implementation, using keymap and recursive edit.

;;; Code:

;;
;; Data structure of MENU
;;
;; <menu> ::= ( menu <prompt> <item-list> )
;; <prompt> ::= STRING
;; <item-list> ::= ( <item> ... )
;; <item> ::= <string> | ( <string> . <value> )
;;
;; <value> ::=  <menu> | INTEGER | STRING  (Must *NOT* cons cell)
;;
;;
;
;;
;; <selection-list> ::= ( <line>... )
;; <line>  ::= ( <item>... )
;;

(defgroup menudiag nil
  "Input Translation System of Tamagotchy"
  :group 'egg)

(defcustom menudiag-select-without-return nil
  "*Number keys not only goes the item, but also select the item, if non-NIL."
  :group 'menudiag :type 'boolean)

(defvar menudiag-mode-map
  (let ((map (make-sparse-keymap))
	ch)
    (setq ch ?0)
    (while (<= ch ?9)
      (define-key map (char-to-string ch) 'menudiag-goto-item)
      (setq ch (1+ ch)))
    (setq ch ?a)
    (while (<= ch ?z)
      (define-key map (char-to-string ch) 'menudiag-goto-item)
      (setq ch (1+ ch)))
    (setq ch ?A)
    (while (<= ch ?Z)
      (define-key map (char-to-string ch) 'menudiag-goto-item)
      (setq ch (1+ ch)))
    (define-key map "\C-a" 'menudiag-beginning-of-line)
    (define-key map "\C-e" 'menudiag-end-of-line)
    (define-key map "\M-<" 'menudiag-beginning-of-items)
    (define-key map "\M->" 'menudiag-end-of-items)
    (define-key map "\C-f" 'menudiag-forward-item)
    (define-key map "\C-b" 'menudiag-backward-item)
    (define-key map "\C-n" 'menudiag-next-line)
    (define-key map "\C-p" 'menudiag-previous-line)
    (define-key map "\C-]" 'menudiag-exit)
    (define-key map "\C-g" 'menudiag-exit-one-level)
    (define-key map "\C-l" 'menudiag-redraw)
    (define-key map "\C-m" 'menudiag-select-this-item)
    (define-key map "?"    'menudiag-list-other-window)
    (define-key map [return] 'menudiag-select-this-item)
    (define-key map [left] 'menudiag-backward-item)
    (define-key map [right] 'menudiag-forward-item)
    (define-key map [up] 'menudiag-previous-line)
    (define-key map [down] 'menudiag-next-line)
    (define-key map [menudiag-continuation] 'menudiag-follow-continuation)
    (define-key map [t] 'undefined)
    map)
  "Keymap for MENU.")

(defun menudiag-menu-p (item)
  (and (consp item) (eq 'menu (car item))))

(defun menudiag-item-string (item)
  (if (stringp item)
      item
    (format "%s" (car item))))

(defun menudiag-item-value (item)
  (if (stringp item)
      item
    (cdr item)))

(defsubst menudiag-item-width (item)
  (+ 4 (string-width (menudiag-item-string item))))

(defvar menudiag-window-conf nil)

(defun menudiag-make-selection-list (item-list line-width)
  (let ((l nil)
	(line nil)
	(width 0)
	(i 0))
    (while item-list
      (let* ((item (car item-list))
	     (item-width (menudiag-item-width item)))
	(if (and line (or (>= (+ width item-width) line-width)
                          (>= i 36)))
	    (setq l (cons (reverse line) l)
		  line nil
		  width 0
		  i 0))
	(setq line (cons item line)
	      width (+ width (menudiag-item-width item))
	      i (1+ i)
	      item-list (cdr item-list))))
    (if line
	(reverse (cons (reverse line) l))
      (reverse l))))

;; Entry function
(defun menudiag-select (menu &optional menudiag-continuation return-contin)
  (let ((enable-recursive-minibuffers t)
	value done)
    (setq menudiag-window-conf nil)
    (if menudiag-continuation
	(setq unread-command-events (cons 'menudiag-continuation
					  unread-command-events)))
    (if (not return-contin)
	(setq value t))
    (menudiag-select-internal menu)
    (if (eq done t)
	value
      (signal 'quit ""))))

;; Entry function
(defun menudiag-get-value (continuation)
  (menudiag-item-value (nth (1- (length continuation)) continuation)))

(defun menudiag-follow-continuation ()
  (interactive)
  (let ((item (car menudiag-continuation)))
    (setq menudiag-continuation (cdr menudiag-continuation))
    (if menudiag-continuation
	(setq unread-command-events (cons 'menudiag-continuation
					  unread-command-events)))
    (if (eq item 'menudiag-list-all)
	(menudiag-list-other-window)
      (let ((in-loop t))
	(while in-loop
	  (if (eq item (nth pos-in-line line))
	      (setq in-loop nil)
	    (menudiag-forward-item)
	    (if (and (= linepos 0) (= pos-in-line 0))
		(error "no such item: %s" (menudiag-item-string item))))))
      (let ((v (menudiag-item-value item)))
	(if (menudiag-menu-p v)
	    (unwind-protect
		(progn
		  (menudiag-select-internal v)
		  (menudiag-redraw))
	      (if (consp value)
		  (setq value (cons item value)))
	      (if done (menudiag-exit-minibuffer))))))))

(defun menudiag-select-internal (menu)
  (let* ((minibuf-prompt (nth 1 menu))
	 (current-items (nth 2 menu))
	 (selection-list
	  (menudiag-make-selection-list current-items
					(- (window-width (minibuffer-window))
					   (string-width minibuf-prompt))))
	 (line (car selection-list))
	 (minibuf-contents
	  (menudiag-make-menu-formatted-string line)))
    (let ((linepos 0)
	  (pos-in-line 0))
      (read-from-minibuffer minibuf-prompt
			    (cons minibuf-contents 3)
			    menudiag-mode-map))))

(defun menudiag-make-menu-formatted-string (item-list)
  (let ((i -1))
    (mapconcat
     (function (lambda (item)
		 (setq i (1+ i))
		 (format "  %c.%s" (menudiag-item-num-to-char i) 
                           (menudiag-item-string item))))
     item-list "")))


;; ITEM No --> Character
(defun menudiag-item-num-to-char (num)
  (let ((char))
    (cond ((<= num 9)
           (setq char (+ ?0 num)))
          (t
           (setq char (+ ?a (- num 10))))
          )
    char))

;; Character --> ITEM No
(defun menudiag-char-to-item-num (char)
  (let ((num))
    (cond ((and (<= ?0 ch) (<= ch ?9))
           (setq num (- ch ?0)))
          ((and (<= ?a ch) (<= ch ?z))
           (setq num (+ 10 (- ch ?a))))
          ((and (<= ?A ch) (<= ch ?Z))
           (setq num (+ 10 (- ch ?A))))
          (t (setq num 1000)))
    num))

(defun menudiag-goto-item ()
  (interactive)
  (let ((ch last-command-char)
	(n 0))
    (setq n (menudiag-char-to-item-num ch))
    (if (>= n (length line))
	(error "No such item")
      (menudiag-goto-item-internal n)
      (if menudiag-select-without-return
	  (menudiag-select-this-item)))))

(defun menudiag-goto-item-internal (n)
  (let ((old-pos-in-line pos-in-line)
	(p 3)
	(i 0))
    (setq pos-in-line n)
    (while (< i pos-in-line)
      (setq p (+ p (length (menudiag-item-string (nth i line))) 4))
      (setq i (1+ i)))
    (goto-char p)))

(defun menudiag-beginning-of-items ()
  (interactive)
  (menudiag-goto-line 0)
  (menudiag-beginning-of-line))

(defun menudiag-end-of-items ()
  (interactive)
  (menudiag-goto-line (1- (length selection-list)))
  (menudiag-end-of-line))

(defun menudiag-beginning-of-line ()
  (interactive)
  (menudiag-goto-item-internal 0))

(defun menudiag-end-of-line ()
  (interactive)
  (menudiag-goto-item-internal (1- (length line))))

;; Should retain compatibility.  Must.
;;
;;(defun menudiag-forward-item ()
;;  (interactive)
;;  (if (< pos-in-line (1- (length line)))
;;      (menudiag-goto-item-internal (1+ pos-in-line))
;;    (if (>= linepos (1- (length selection-list)))
;;	(signal 'end-of-buffer "")
;;      (menudiag-goto-line (1+ linepos))
;;      (menudiag-beginning-of-line))))
;;
;;(defun menudiag-backward-item ()
;;  (interactive)
;;  (if (< 0 pos-in-line)
;;      (menudiag-goto-item-internal (1- pos-in-line))
;;    (if (< linepos 1)
;;	(signal 'beginning-of-buffer "")
;;      (menudiag-goto-line (1- linepos))
;;      (menudiag-end-of-line))))
;;
;;(defun menudiag-goto-line (n)
;;  (if (or (>= n (length selection-list)) (< n 0))
;;      (ding)
;;    (setq line (nth n selection-list)
;;	  linepos n)
;;    (delete-region (point-min) (point-max))
;;    (insert (menudiag-make-menu-formatted-string line))))
;;

(defun menudiag-forward-item ()
  (interactive)
  (if (< pos-in-line (1- (length line)))
      (menudiag-goto-item-internal (1+ pos-in-line))
    (if (>= linepos (1- (length selection-list)))
	(menudiag-goto-line 0)
      (menudiag-goto-line (1+ linepos)))
    (menudiag-beginning-of-line)))

(defun menudiag-backward-item ()
  (interactive)
  (if (< 0 pos-in-line)
      (menudiag-goto-item-internal (1- pos-in-line))
    (if (< linepos 1)
        (menudiag-goto-line (1- (length selection-list)))
      (menudiag-goto-line (1- linepos)))
    (menudiag-end-of-line)))

(defun menudiag-goto-line (n)
  (cond
   ((>= n (length selection-list))
    (setq n 0))
   ((< n 0)
    (setq n (1- (length selection-list)))))
  (setq line (nth n selection-list)
        linepos n)
  (delete-region (point-min) (point-max))
  (insert (menudiag-make-menu-formatted-string line)))

(defun menudiag-next-line ()
  (interactive)
  (menudiag-goto-line (1+ linepos))
  (if (< pos-in-line (length line))
      (menudiag-goto-item-internal pos-in-line)
    (menudiag-end-of-line)))

(defun menudiag-previous-line ()
  (interactive)
  (menudiag-goto-line (1- linepos))
  (if (< pos-in-line (length line))
      (menudiag-goto-item-internal pos-in-line)
    (menudiag-end-of-line)))

(defun menudiag-redraw ()
  (interactive)
  (menudiag-goto-line linepos)
  (menudiag-goto-item-internal pos-in-line))

(defun menudiag-exit-one-level ()
  (interactive)
  (menudiag-exit-minibuffer))

(defun menudiag-exit ()
  (interactive)
  (setq done 'quit)
  (menudiag-exit-minibuffer))

(defun menudiag-select-this-item ()
  (interactive)
  (let* ((item (nth pos-in-line line))
	 (v (menudiag-item-value item)))
    (if (menudiag-menu-p v)
	(unwind-protect
	    (progn
	      (menudiag-restore-window)
	      (menudiag-select-internal v)
	      (menudiag-redraw))
	  (if (consp value)
	      (setq value (cons item value)))
	  (if done (menudiag-exit-minibuffer)))
      (if (eq value t)
	  (setq value (menudiag-item-value item))
	(setq value (cons item nil)))
      (setq done t)
      (menudiag-exit-minibuffer))))

(defconst menudiag-selection-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right]   'next-completion)
    (define-key map [left]    'previous-completion)
    (define-key map "\r"      'menudiag-choose-item)
    (define-key map [mouse-2] 'menudiag-mouse-choose-item)
    map))

(defvar menudiag-selection-buffer nil)
(make-variable-buffer-local 'menudiag-selection-buffer)
(put 'menudiag-selection-buffer 'permanent-local t)

(defvar menudiag-selection-main-buffer nil)
(make-variable-buffer-local 'menudiag-selection-main-buffer)
(put 'menudiag-selection-main-buffer 'permanent-local t)

(defun menudiag-selection-mode ()
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'inhibit-read-only)
  (setq buffer-read-only t
	inhibit-read-only nil)
  (use-local-map menudiag-selection-map)
  (setq mode-name "Menudiag Selection")
  (setq major-mode 'menudiag-selection-mode))

(defun menudiag-max-item-width (item-list)
  (let ((max 0))
    (while item-list
      (setq max (max max (menudiag-item-width (car item-list)))
	    item-list (cdr item-list)))
    max))

(defun menudiag-buffer-show-function ()
  (let* ((items current-items)
	 (digits (length (concat (length items))))
	 (columns (max 1 (/ (window-width (minibuffer-window))
			    (+ digits (menudiag-max-item-width items)))))
	 (width (/ (window-width (minibuffer-window)) columns))
	 (col 0) (n 0) str)
    (insert " ")
    (while items
      (setq p (point)
	    str (format (concat "%" digits "d. %s")
			n (menudiag-item-string (car items))))
      (insert str)
      (set-text-properties p (point) '(mouse-face highlight))
      (setq col (1+ col)
	    n (1+ n)
	    items (cdr items))
      (if items
	  (if (/= col columns)
	      (insert (make-string (- width (string-width str)) ?\ ))
	    (insert "\n ")
	    (setq col 0))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (menudiag-selection-mode)))

(defun menudiag-buffer-name (prompt)
  (let ((len (1- (length prompt))))
    (if (= (aref prompt len) ?:) (substring prompt 0 len) prompt)))

(defun menudiag-list-other-window ()
  (interactive)
  (let ((temp-buffer-show-hook 'menudiag-buffer-show-function)
	(main-buf (current-buffer)))
    (setq menudiag-window-conf (current-window-configuration))
    (with-output-to-temp-buffer (menudiag-buffer-name minibuf-prompt)
      (setq menudiag-selection-buffer standard-output))
    (set-buffer menudiag-selection-buffer)
    (setq menudiag-selection-main-buffer main-buf)))

(defun menudiag-choose-item ()
  (interactive)
  (let ((org-buf menudiag-selection-main-buffer)
	(sel-buf (current-buffer))
	(item-list selection-list)
	(l 0)
	tmp-buf n)
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (set-buffer sel-buf)
      (setq completion-reference-buffer tmp-buf)
      (choose-completion)
      (set-buffer tmp-buf)
      (setq n (string-to-int (buffer-string))))
    (pop-to-buffer org-buf)
    (while (and item-list (>= (- n (length (car item-list))) 0))
      (setq l (1+ l)
	    n (- n (length (car item-list)))
	    item-list (cdr item-list)))
    (menudiag-goto-line l)
    (menudiag-goto-item-internal n)
    (menudiag-select-this-item)))

(defun menudiag-mouse-choose-item (event)
  (interactive "e")
  (set-buffer (window-buffer (car (nth 1 event))))
  (let ((org-buf menudiag-selection-main-buffer)
	(sel-buf (current-buffer))
	(item-list selection-list)
	(l 0)
	tmp-buf n)
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (set-buffer sel-buf)
      (setq completion-reference-buffer tmp-buf)
      (mouse-choose-completion event)
      (set-buffer tmp-buf)
      (setq n (string-to-int (buffer-string))))
    (pop-to-buffer org-buf)
    (while (and item-list (>= (- n (length (car item-list))) 0))
      (setq l (1+ l)
	    n (- n (length (car item-list)))
	    item-list (cdr item-list)))
    (menudiag-goto-line l)
    (menudiag-goto-item-internal n)
    (menudiag-select-this-item)))

(defun menudiag-restore-window ()
  (if menudiag-window-conf
      (progn
	(set-window-configuration menudiag-window-conf)
	(setq menudiag-window-conf nil)
	(kill-buffer menudiag-selection-buffer))))

(defun menudiag-exit-minibuffer ()
  (and menudiag-window-conf (menudiag-restore-window))
  (exit-minibuffer))

(provide 'menudiag)
;;; menudiag.el ends here.
