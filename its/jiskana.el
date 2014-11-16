;;; its/jiskana.el --- Hiragana Input in Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: Kubo "Munk3" Hiroshi <hiroshi@netird.ad.jp>

;; This file is not a part of EGG.

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
;;
;; Symbol input is desined by jiro@math.keio.ac.jp (TANAKA Jiro)
;; This file is based on the rules of its/hira.el of Tamago 4
;;

;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defgroup jikana nil
  "JIS Keyboard kana Input Method"
  :group 'its)

(defvar its-jiskana-enable-zenkaku-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Zenkaku alphabet")

(defcustom its-jiskana-period "$B!#(B" 
  "* >$B$rF~NO$7$?$H$-$N6gE@$NJ8;z(B: \"$B!#(B\"  \". \" \"$B!%(B\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-comma  "$B!"(B"
 "* <$B$rF~NO$7$?$H$-$NFIE@$NJ8;z(B: \"$B!"(B\" \", \" \"$B!$(B\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-open-bracket  "$B!V(B"
 "* { $B$rF~NO$7$?$H$-$N$+$.3g8L3+$1$NJ8;z(B: \"$B!V(B\" \"$B!P(B\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-close-bracket "$B!W(B"
 "* } $B$rF~NO$7$?$H$-$N$+$.3g8LJD$8$NJ8;z(B: \"$B!W(B\" \"$B!Q(B\""
  :group 'jiskana :type 'string)

(defcustom its-jiskana-horizontal  "$B!<(B"
  "* - $B$rF~NO$7$?$H$-$ND92;5-9f$NJ8;z(B: \"$B!<(B\" \"$B!](B\""
  :group 'jiskana :type 'string)

(define-its-state-machine its-jiskana-map
  "kana" "$B$o(B" Japanese
  "Map for JIS arrangement keyboard to hiragana translation. (Japanese)"

  (defconst its-zenkaku-escape "A")  ;; Escape character to Zenkaku inputs
  (defconst its-hankaku-escape "H")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "D" downcase)
  (its-defrule-select-mode-temporally "Q" zenkaku-downcase)


  ;;; $B@62;(B

  ;; $B$"9T(B
  (its-defrule   "3"    "$B$"(B")
  (its-defrule   "e"    "$B$$(B")
  (its-defrule   "4"    "$B$&(B")
  (its-defrule   "5"    "$B$((B")
  (its-defrule   "6"    "$B$*(B")

  ;; $B$+9T(B
  (its-defrule*   "t"    "$B$+(B"  "$B$+(B")
  (its-defrule*   "g"    "$B$-(B"  "$B$-(B")
  (its-defrule*   "h"    "$B$/(B"  "$B$/(B")
  (its-defrule*   ":"    "$B$1(B"  "$B$1(B")
  (its-defrule*   "b"    "$B$3(B"  "$B$3(B")

  ;; $B$59T(B
  (its-defrule*   "x"    "$B$5(B"  "$B$5(B")
  (its-defrule*   "d"    "$B$7(B"  "$B$7(B")
  (its-defrule*   "r"    "$B$9(B"  "$B$9(B")
  (its-defrule*   "p"    "$B$;(B"  "$B$;(B")
  (its-defrule*   "c"    "$B$=(B"  "$B$=(B")

  ;; $B$?9T(B
  (its-defrule*   "q"    "$B$?(B"  "$B$?(B")
  (its-defrule*   "a"    "$B$A(B"  "$B$A(B")
  (its-defrule*   "z"    "$B$D(B"  "$B$D(B")
  (its-defrule*   "w"    "$B$F(B"  "$B$F(B")
  (its-defrule*   "s"    "$B$H(B"  "$B$H(B")

  ;; $B$J9T(B
  (its-defrule   "u"   "$B$J(B")
  (its-defrule   "i"   "$B$K(B")
  (its-defrule   "1"   "$B$L(B")
  (its-defrule   ","   "$B$M(B")
  (its-defrule   "k"   "$B$N(B")

  ;; $B$O9T(B
  (its-defrule*   "f"   "$B$O(B"  "$B$O(B")
  (its-defrule*   "v"   "$B$R(B"  "$B$R(B")
  (its-defrule*   "2"   "$B$U(B"  "$B$U(B")
  (its-defrule*   "^"   "$B$X(B"  "$B$X(B")
  (its-defrule*   "-"   "$B$[(B"  "$B$[(B")

  ;; $B$^9T(B
  (its-defrule   "j"   "$B$^(B")
  (its-defrule   "n"   "$B$_(B")
  (its-defrule   "]"   "$B$`(B")
  (its-defrule   "/"   "$B$a(B")
  (its-defrule   "m"   "$B$b(B")

  ;; $B$d9T(B
  (its-defrule   "7"   "$B$d(B")
  (its-defrule   "8"   "$B$f(B")
  (its-defrule   "9"   "$B$h(B")

  ;; $B$i9T(B
  (its-defrule   "o"   "$B$i(B")
  (its-defrule   "l"   "$B$j(B")
  (its-defrule   "."   "$B$k(B")
  (its-defrule   ";"   "$B$l(B")
  (its-defrule   "\\"   "$B$m(B")

  ;; $B$o9T(B
  (its-defrule   "0"   "$B$o(B")
  (its-defrule   "F"   "$B$p(B")
  (its-defrule   "J"   "$B$q(B")
  (its-defrule   "~"   "$B$r(B")
  (its-defrule   "y"   "$B$s(B")

  ;;; $BBy2;(B
  ;; $B$,9T(B
  (its-defrule   "t@"    "$B$,(B")
  (its-defrule   "g@"    "$B$.(B")
  (its-defrule   "h@"    "$B$0(B")
  (its-defrule   ":@"    "$B$2(B")
  (its-defrule   "b@"    "$B$4(B")

  ;; $B$69T(B
  (its-defrule   "x@"    "$B$6(B")
  (its-defrule   "d@"    "$B$8(B")
  (its-defrule   "r@"    "$B$:(B")
  (its-defrule   "p@"    "$B$<(B")
  (its-defrule   "c@"    "$B$>(B")

  ;; $B$@9T(B
  (its-defrule   "q@"    "$B$@(B")
  (its-defrule   "a@"    "$B$B(B")
  (its-defrule   "z@"    "$B$E(B")
  (its-defrule   "w@"    "$B$G(B")
  (its-defrule   "s@"    "$B$I(B")

  ;; $B$P9T(B
  (its-defrule   "f@"   "$B$P(B")
  (its-defrule   "v@"   "$B$S(B")
  (its-defrule   "2@"   "$B$V(B")
  (its-defrule   "^@"   "$B$Y(B")
  (its-defrule   "-@"   "$B$\(B")

  ;; $B$Q9T(B
  (its-defrule   "f["   "$B$Q(B")
  (its-defrule   "v["   "$B$T(B")
  (its-defrule   "2["   "$B$W(B")
  (its-defrule   "^["   "$B$Z(B")
  (its-defrule   "-["   "$B$](B")

  ;;; 
  ;; $B$d9T(B
  (its-defrule   "'"   "$B$c(B")
  (its-defrule   "("   "$B$e(B")
  (its-defrule   ")"   "$B$g(B")

  ;; $B$"9T(B
  (its-defrule "#"   "$B$!(B")
  (its-defrule "E"   "$B$#(B")
  (its-defrule "$"   "$B$%(B")
  (its-defrule "%"   "$B$'(B")
  (its-defrule "&"   "$B$)(B")

  (its-defrule "Z"  "$B$C(B")
  (its-defrule "W0" "$B$n(B")

  (its-defrule "4@" "$B%t(B")

;;;
;;; Zenkaku inputs
;;;

  (its-defrule (concat its-zenkaku-escape "0") "$B#0(B")
  (its-defrule (concat its-zenkaku-escape "1") "$B#1(B")
  (its-defrule (concat its-zenkaku-escape "2") "$B#2(B")
  (its-defrule (concat its-zenkaku-escape "3") "$B#3(B")
  (its-defrule (concat its-zenkaku-escape "4") "$B#4(B")
  (its-defrule (concat its-zenkaku-escape "5") "$B#5(B")
  (its-defrule (concat its-zenkaku-escape "6") "$B#6(B")
  (its-defrule (concat its-zenkaku-escape "7") "$B#7(B")
  (its-defrule (concat its-zenkaku-escape "8") "$B#8(B")
  (its-defrule (concat its-zenkaku-escape "9") "$B#9(B")

  (its-defrule (concat its-zenkaku-escape "A") "$B#A(B")
  (its-defrule (concat its-zenkaku-escape "B") "$B#B(B")
  (its-defrule (concat its-zenkaku-escape "C") "$B#C(B")
  (its-defrule (concat its-zenkaku-escape "D") "$B#D(B")
  (its-defrule (concat its-zenkaku-escape "E") "$B#E(B")
  (its-defrule (concat its-zenkaku-escape "F") "$B#F(B")
  (its-defrule (concat its-zenkaku-escape "G") "$B#G(B")
  (its-defrule (concat its-zenkaku-escape "H") "$B#H(B")
  (its-defrule (concat its-zenkaku-escape "I") "$B#I(B")
  (its-defrule (concat its-zenkaku-escape "J") "$B#J(B")
  (its-defrule (concat its-zenkaku-escape "K") "$B#K(B")
  (its-defrule (concat its-zenkaku-escape "L") "$B#L(B")
  (its-defrule (concat its-zenkaku-escape "M") "$B#M(B")
  (its-defrule (concat its-zenkaku-escape "N") "$B#N(B")
  (its-defrule (concat its-zenkaku-escape "O") "$B#O(B")
  (its-defrule (concat its-zenkaku-escape "P") "$B#P(B")
  (its-defrule (concat its-zenkaku-escape "Q") "$B#Q(B")
  (its-defrule (concat its-zenkaku-escape "R") "$B#R(B")
  (its-defrule (concat its-zenkaku-escape "S") "$B#S(B")
  (its-defrule (concat its-zenkaku-escape "T") "$B#T(B")
  (its-defrule (concat its-zenkaku-escape "U") "$B#U(B")
  (its-defrule (concat its-zenkaku-escape "V") "$B#V(B")
  (its-defrule (concat its-zenkaku-escape "W") "$B#W(B")
  (its-defrule (concat its-zenkaku-escape "X") "$B#X(B")
  (its-defrule (concat its-zenkaku-escape "Y") "$B#Y(B")
  (its-defrule (concat its-zenkaku-escape "Z") "$B#Z(B")

  (its-defrule (concat its-zenkaku-escape "a") "$B#a(B")
  (its-defrule (concat its-zenkaku-escape "b") "$B#b(B")
  (its-defrule (concat its-zenkaku-escape "c") "$B#c(B")
  (its-defrule (concat its-zenkaku-escape "d") "$B#d(B")
  (its-defrule (concat its-zenkaku-escape "e") "$B#e(B")
  (its-defrule (concat its-zenkaku-escape "f") "$B#f(B")
  (its-defrule (concat its-zenkaku-escape "g") "$B#g(B")
  (its-defrule (concat its-zenkaku-escape "h") "$B#h(B")
  (its-defrule (concat its-zenkaku-escape "i") "$B#i(B")
  (its-defrule (concat its-zenkaku-escape "j") "$B#j(B")
  (its-defrule (concat its-zenkaku-escape "k") "$B#k(B")
  (its-defrule (concat its-zenkaku-escape "l") "$B#l(B")
  (its-defrule (concat its-zenkaku-escape "m") "$B#m(B")
  (its-defrule (concat its-zenkaku-escape "n") "$B#n(B")
  (its-defrule (concat its-zenkaku-escape "o") "$B#o(B")
  (its-defrule (concat its-zenkaku-escape "p") "$B#p(B")
  (its-defrule (concat its-zenkaku-escape "q") "$B#q(B")
  (its-defrule (concat its-zenkaku-escape "r") "$B#r(B")
  (its-defrule (concat its-zenkaku-escape "s") "$B#s(B")
  (its-defrule (concat its-zenkaku-escape "t") "$B#t(B")
  (its-defrule (concat its-zenkaku-escape "u") "$B#u(B")
  (its-defrule (concat its-zenkaku-escape "v") "$B#v(B")
  (its-defrule (concat its-zenkaku-escape "w") "$B#w(B")
  (its-defrule (concat its-zenkaku-escape "x") "$B#x(B")
  (its-defrule (concat its-zenkaku-escape "y") "$B#y(B")
  (its-defrule (concat its-zenkaku-escape "z") "$B#z(B")

  (its-defrule (concat its-zenkaku-escape " ")  "$B!!(B")
  (its-defrule (concat its-zenkaku-escape "!")  "$B!*(B")
  (its-defrule (concat its-zenkaku-escape "@")  "$B!w(B")
  (its-defrule (concat its-zenkaku-escape "#")  "$B!t(B")
  (its-defrule (concat its-zenkaku-escape "$")  "$B!p(B")
  (its-defrule (concat its-zenkaku-escape "%")  "$B!s(B")
  (its-defrule (concat its-zenkaku-escape "^")  "$B!0(B")
  (its-defrule (concat its-zenkaku-escape "&")  "$B!u(B")
  (its-defrule (concat its-zenkaku-escape "*")  "$B!v(B")
  (its-defrule (concat its-zenkaku-escape "(")  "$B!J(B")
  (its-defrule (concat its-zenkaku-escape ")")  "$B!K(B")
  (its-defrule (concat its-zenkaku-escape "-")  "$B!](B")
  (its-defrule (concat its-zenkaku-escape "=")  "$B!a(B")
  (its-defrule (concat its-zenkaku-escape "`")  "$B!.(B")
  (its-defrule (concat its-zenkaku-escape "\\") "$B!o(B")
  (its-defrule (concat its-zenkaku-escape "|")  "$B!C(B")
  (its-defrule (concat its-zenkaku-escape "_")  "$B!2(B")
  (its-defrule (concat its-zenkaku-escape "+")  "$B!\(B")
  (its-defrule (concat its-zenkaku-escape "~")  "$B!1(B")
  (its-defrule (concat its-zenkaku-escape "[")  "$B!N(B")
  (its-defrule (concat its-zenkaku-escape "]")  "$B!O(B")
  (its-defrule (concat its-zenkaku-escape "{")  "$B!P(B")
  (its-defrule (concat its-zenkaku-escape "}")  "$B!Q(B")
  (its-defrule (concat its-zenkaku-escape ":")  "$B!'(B")
  (its-defrule (concat its-zenkaku-escape ";")  "$B!((B")
  (its-defrule (concat its-zenkaku-escape "\"") "$B!I(B")
  (its-defrule (concat its-zenkaku-escape "'")  "$B!G(B")
  (its-defrule (concat its-zenkaku-escape "<")  "$B!c(B")
  (its-defrule (concat its-zenkaku-escape ">")  "$B!d(B")
  (its-defrule (concat its-zenkaku-escape "?")  "$B!)(B")
  (its-defrule (concat its-zenkaku-escape "/")  "$B!?(B")
  (its-defrule (concat its-zenkaku-escape ",")  "$B!$(B")
  (its-defrule (concat its-zenkaku-escape ".")  "$B!%(B")

;;;
;;; Hankaku inputs
;;;

  (dolist (digit '( "1"  "2"  "3"  "4" "5"  "6"  "7"  "8"  "9"  "0" ))
    (its-defrule (concat its-hankaku-escape digit)  digit))

  (dolist (symbol '( " "  "!"  "@"  "#"  "$"  "%"  "^"  "&"  "*"  "("  ")"
		     "-"  "="  "`"  "\\" "|"  "_"  "+"  "~" "["  "]"  "{"  "}"
		     ":"  ";"  "\"" "'"  "<"  ">"  "?"  "/"  ","  "." ))
    (its-defrule (concat its-hankaku-escape symbol) symbol))

  (dolist (downcase '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
		      "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
    (its-defrule (concat its-hankaku-escape downcase) downcase))

  (dolist (upcase    '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
		       "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
    (its-defrule (concat its-hankaku-escape upcase) upcase))

;; SYMBOL Input
  (its-defrule   "X1"   "$B!{(B")	(its-defrule   "X!"   "$B!|(B")
  (its-defrule   "X2"   "$B"&(B")	(its-defrule   "X@"   "$B"'(B")
  (its-defrule   "X3"   "$B"$(B")	(its-defrule   "X#"   "$B"%(B")
  (its-defrule   "X4"   "$B""(B")	(its-defrule   "X$"   "$B"#(B")
  (its-defrule   "X5"   "$B!~(B")	(its-defrule   "X%"   "$B"!(B")
  (its-defrule   "X6"   "$B!y(B")	(its-defrule   "X^"   "$B!z(B")
  (its-defrule   "X7"   "$B!}(B")	(its-defrule   "X&"   "$B!r(B")
  (its-defrule   "X8"   "$B!q(B")	(its-defrule   "X*"   "$B!_(B")
  (its-defrule   "X9"   "$B!i(B")	(its-defrule   "X("   "$B!Z(B")
  (its-defrule   "X0"   "$B!j(B")	(its-defrule   "X)"   "$B![(B")
  (its-defrule   "X-"   "$B!A(B")	(its-defrule   "X_"   "$B!h(B")
  (its-defrule   "X="   "$B!b(B")	(its-defrule   "X+"   "$B!^(B")
  (its-defrule   "X\\"  "$B!@(B")	(its-defrule   "X|"   "$B!B(B")
  (its-defrule   "X`"   "$B!-(B")	(its-defrule   "X~"   "$B!/(B")

  (its-defrule   "Xq"   "$B!T(B")	(its-defrule   "XQ"   "$B!R(B")
  (its-defrule   "Xw"   "$B!U(B")	(its-defrule   "XW"   "$B!S(B")
					; e
  (its-defrule   "Xr"   "$B!9(B")	(its-defrule   "XR"   "$B!8(B")
  (its-defrule   "Xt"   "$B!:(B")	(its-defrule   "XT"   "$B!x(B")
					; y u i o
  (its-defrule   "Xp"   "$B")(B")	(its-defrule   "XP"   "$B",(B")
  (its-defrule   "X["   "$B!X(B")	(its-defrule   "X{"   "$B!L(B")
  (its-defrule   "X]"   "$B!Y(B")	(its-defrule   "X}"   "$B!M(B")

					; a
  (its-defrule   "Xs"   "$B!3(B")	(its-defrule   "XS"   "$B!4(B")
  (its-defrule   "Xd"   "$B!5(B")	(its-defrule   "XD"   "$B!6(B")
  (its-defrule   "Xf"   "$B!7(B")	(its-defrule   "XF"   "$B"*(B")
  (its-defrule   "Xg"   "$B!>(B")	(its-defrule   "XG"   "$B!=(B")
  (its-defrule   "Xh"   "$B"+(B")
  (its-defrule   "Xj"   "$B"-(B")
  (its-defrule   "Xk"   "$B",(B")
  (its-defrule   "Xl"   "$B"*(B")
  (its-defrule   "X;"   "$B!+(B")	(its-defrule   "X:"   "$B!,(B")
  (its-defrule   "X\'"  "$B!F(B")	(its-defrule   "X\""  "$B!H(B")

					; z
  (its-defrule   "Xx"   ":-")	(its-defrule   "XX"   ":-)")
  (its-defrule   "Xc"   "$B!;(B")	(its-defrule   "XC"   "$B!n(B")
  (its-defrule   "Xv"   "$B"((B")	(its-defrule   "XV"   "$B!`(B")
  (its-defrule   "Xb"   "$B!k(B")	(its-defrule   "XB"   "$B"+(B")
  (its-defrule   "Xn"   "$B!l(B")	(its-defrule   "XN"   "$B"-(B")
  (its-defrule   "Xm"   "$B!m(B")	(its-defrule   "XM"   "$B".(B")
  (its-defrule   "X,"   "$B!E(B")	(its-defrule   "X<"   "$B!e(B")
  (its-defrule   "X."   "$B!D(B")	(its-defrule   "X>"   "$B!f(B")
  (its-defrule   "X/"   "$B!&(B")	(its-defrule   "X?"   "$B!g(B")
  )

(define-its-state-machine-append its-jiskana-map

  (its-defrule "|" its-jiskana-horizontal)
  (its-defrule "{" its-jiskana-open-bracket)
  (its-defrule "}" its-jiskana-close-bracket)
  (its-defrule ">" its-jiskana-period)
  (its-defrule "<" its-jiskana-comma)

  )

(provide 'its/jiskana)
;;; its/jiskana.el ends here.
