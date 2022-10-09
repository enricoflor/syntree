;;; syntree.el --- Draw plain text constituency trees  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/syntree
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1") (org "9.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Syntree.el returns plain text constituency trees, given an input
;; that specifies the constituency.  It is primarily aimed at
;; linguists (syntacticians, semanticists etc.) who might want to have
;; trees in their plain text nodes because of their portability, but
;; it can of course be used to draw any sort of tree of the suitable
;; form.
;;
;; For full documentation, see the readme file at
;;          <https://github.com/enricoflor/syntree>
;; or the info page installed with the package.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'org)
(require 'ob-core)
(eval-when-compile (require 'subr-x))

;;; Global variables

(defgroup syntree nil
  "Generate plain text constituency trees."
  :prefix "syntree-"
  :link '(url-link :tag "Website for syntree"
                   "https://github.com/enricoflor/syntree")
  :group 'convenience)

;;; Styles related functions and variables

(defconst syntree-properties
  '(:growing
    :hspace
    :one-line
    :height
    :hbranch
    :branchcenter
    :intersection
    :l-intersection
    :r-intersection
    :stem
    :l-stem
    :r-stem
    :roofstem
    :rooftop
    :rooftopangle
    :roofbottom
    :roofbottomangle
    :roofwidth
    :roofminwidth
    :word-wrap)
  "Properties that define the the style of a tree.

In addition to any number of these properties, a style must
assign a value to the special keywords ':der' and ':name'.")

(defvar syntree--style-basic
  '(:name basic
          :der nil
          :growing down
          :hspace 0
          :one-line nil
          :height 0
          :hbranch "_"
          :branchcenter "|"
          :intersection "."
          :l-intersection "."
          :r-intersection "."
          :stem "|"
          :l-stem "|"
          :r-stem "|"
          :roofstem "|"
          :rooftop "_"
          :rooftopangle "."
          :roofbottom "_"
          :roofbottomangle "|"
          :roofwidth 0
          :roofminwidth 3
          :word-wrap 0)
  "Basic vertical style for `syntree' trees.

This style can be used as basis for others since it specifies a
value for all properties in `syntree-properties'.")

(defvar syntree--style-horizontal
  '(:name horizontal
          :der nil
          :growing right
          :hspace 0
          :one-line nil
          :height 2
          :hbranch "|"
          :branchcenter "+"
          :intersection "+"
          :l-intersection "+"
          :r-intersection "+"
          :stem "-"
          :l-stem "-"
          :r-stem "-"
          :roofstem "|"
          :rooftop "|"
          :rooftopangle "|"
          :roofbottom ""
          :roofbottomangle ""
          :roofwidth 0
          :roofminwidth 1
          :word-wrap 5)
  "Basic horizontal style for `syntree' trees.

This style can be used as basis for others since it specifies a
value for all properties in `syntree-properties'.")

(defvar syntree--style-basic-upwards
  '(:name basic-upwards
          :der basic
          :growing up
          :hbranch "-"
          :branchcenter "+"
          :intersection "+"
          :l-intersection "+"
          :r-intersection "+"
          :rooftop "-"
          :roofbottom " "
          :roofstem "+"
          :roofbottomangle " "
          :rooftopangle "+"))

(defvar syntree--style-basic-one-line
  '(:name basic-one-line
          :der basic
          :one-line t))

(defvar syntree-styles-list '()
  "List of plists that define a style for syntree trees.")

(setq syntree-styles-list (mapcar #'eval
                                  '(syntree--style-basic
                                    syntree--style-horizontal
                                    syntree--style-basic-upwards
                                    syntree--style-basic-one-line)))

(defvar syntree-default-style 'basic
  "Initial style for syntree trees.

Its value must be the value for the property ':name' in one plist
in `syntree-styles-list'.")

(defvar-local syntree--style nil
  "Style for the current syntree input buffer.")

(defvar syntree--current-style nil
  "Style that informs `syntree--p-get'.")

(defvar syntree--styles-alist '()
  "Alist constructed from `syntree-styles-list'.

Each element is a cons (NAME . PLIST).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All stored styles are in 'syntree-styles-list', a list of plists.       ;;
;;                                                                         ;;
;; There is a special style, 'syntree--style' (a buffer local variable):   ;;
;; 'syntree-new' sets the value of 'syntree--style' to the plist           ;;
;; associated to 'syntree-default-style', which is a symbol that is the    ;;
;; car of a cell in 'syntree-styles-list'.  Functions that change the      ;;
;; style or manipulate current values do so operating on the plist         ;;
;; 'syntree--style'.                                                       ;;
;;                                                                         ;;
;; Every time 'syntree--refresh' is called, the value of                   ;;
;; 'syntree--current-style' is set to the one of 'syntree--style'.  If     ;;
;; multiple syntree buffers are open, they can each have a different       ;;
;; value for 'syntree--style'.                                             ;;
;;                                                                         ;;
;; 'syntree-styles-alist' is generated every time 'syntree-new' and        ;;
;; 'syntree-change-value' are called: it is ;; a list of cons cells (NAME  ;;
;; . PLIST) where NAME is the value of :name in PLIST.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun syntree--p-get (p &optional seq st)
  "Get value for P from plist ST.

If SEQ is non-nil, return a list containing the value of P, if
that value is a character.

If ST is nil, it defaults to `syntree--style'.  If there
is no value for P in ST, return the value for P in the plist
specified as value for ':der' in plist ST."
  (unless (memq p syntree-properties)
    (user-error "Property is not defined"))
  (let* ((style (or (map-elt syntree--styles-alist st)
                    syntree--current-style))
         (der (map-elt style :der)))
    ;; If the value of :der is missing or refers to a non defined
    ;; style, we need to quit.
    (cond ((and (not (memq :der (map-keys style))))
           (user-error "Malformed style plist: missing :der value"))
          ((and der (not (map-elt syntree--styles-alist der)))
           (user-error (format "%s not defined" der)))
          (t t))
    (let ((property (when (memq p (map-keys style)) (map-elt style p))))
      (cond ((and (memq p (map-keys style)) seq (characterp property))
             (list property))
            ((memq p (map-keys style)) property)
            (t (syntree--p-get p seq der))))))

;;; Basic string functions

(defun syntree--reverse (direction x)
  "Reverse X (a string or a list) depending on DIRECTION.

DIRECTION can be either ':down', ':up', ':right' or ':left'."
  (let ((dir (or direction (syntree--p-get :growing))))
    ;; If the tree grows upward, then of course we want to reverse the
    ;; list.  But the same is true if the growth is to the left:
    ;; because the strings will be transformed, and the first string
    ;; will end up being the last slice, etc.
    (if (or (eq dir 'up) (eq dir 'left)) (nreverse x) (identity x))))

(defun syntree--verticalize-text (direction l)
  "With L a list of strings, return an adjusted list.

The output of this function is a list of strings that will be
identical to those in L if the tree is turned from vertical to
horizontal.

Return L if DIRECTION is not 'left' or 'right'."
  (if (or (eq direction 'left) (eq direction 'right))
      (thread-last l
                   (syntree--homogenize-length 'list nil direction)
                   (mapcar #'syntree--string-to-list)
                   (mapcar #'(lambda (x) (syntree--reverse direction x)))
                   (nreverse)
                   (syntree--zip-concat-lists-of-strings ""))
    (identity l)))

(defun syntree--horizontalize-text (direction l)
  "With L a list of strings, return an adjusted list.

The output of this function is a list of strings that will be
identical to those in L if the tree is turned from vertical to
horizontal.

Return L if DIRECTION is not 'left' or 'right'."
  (if (or (eq direction 'left) (eq direction 'right))
      (let ((disassembled (thread-last l
                                       (mapcar #'syntree--string-to-list)
                                       (syntree--reverse nil))))
        (nreverse (syntree--zip-concat-lists-of-strings "" disassembled)))
    (identity l)))

(defun syntree--fill (p w right-or-left s)
  "If S is shorter than W, pad right and left with P.

If RIGHT-OR-LEFT is 'right' or 'left', just pad at the right or
left of S, respectively.

Always return a string that is at least as long as W."
  (let* ((diff (max 0 (- w (length s))))
         (post (/ diff 2))
         (pre (- diff post)))
    (cond ((eq right-or-left 'right)
           (concat s (apply #'concat (make-list diff p))))
          ((eq right-or-left 'left)
           (concat (apply #'concat (make-list diff p)) s))
          (t (concat (apply #'concat (make-list pre p))
                     s
                     (apply #'concat (make-list post p)))))))

(defun syntree--list-fill (s n l)
  "If L has less than N elements, add S to get there.

L is a list of string, S is a string, N is an integer.  Return L
if its length is at least N.  Otherwise, add and append as many S
to L until the length is N."
  (let* ((diff (max 0 (- n (length l))))
         (post (/ diff 2))
         (pre (- diff post)))
    (append (make-list post s) l (make-list pre s))))

(defun syntree--wrap-string (s w)
  "Word-wrap string S at width W.  If W < 1, don't wrap."
  (let ((wrap (cond ((not (numberp w))
                     most-positive-fixnum)
                    ((< w 1)
                     most-positive-fixnum)
                    (t w))))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (let ((fill-column wrap)
            (adaptive-fill-mode nil))
        (fill-region (point-min) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;; Converting leaf strings into terminal nodes

(defun syntree--split-and-wrap (s &optional width)
  "Split S at \"\\n\", wrap at desired length, and split again.

The desired length is the value for ':word-wrap' in
`syntree--current-style' if WIDTH is nil, otherwise is WIDTH.

Return a list of strings."
  ;; (when ())
  (let ((w (or width (syntree--p-get :word-wrap))))
    (if (string-blank-p s)
        (list "")
      (thread-last
        s
        (string-trim)
        (funcall #'(lambda (x) (split-string x "[\f\t\n\r\v]+" t)))
        (mapcar #'(lambda (x) (syntree--wrap-string x w)))
        (mapcar #'(lambda (x) (split-string x "[\f\t\n\r\v]+" t)))
        (flatten-tree)))))

(defun syntree--gen-roof (width)
  "Generate a roof of width WIDTH."
  (let* ((min (if (> 1 (syntree--p-get :roofminwidth))
                  1
                (syntree--p-get :roofminwidth)))
         (raw-w (+ width (* 2 (syntree--p-get :roofwidth))))
         (w (if (< raw-w 1) 1 raw-w))
         (stem (if (syntree--p-get :roofstem)
                   (syntree--p-get :roofstem)
                 ;; If there is no special value for :roofstem, just
                 ;; use the value of :stem
                 (syntree--p-get :stem)))
         (raw-top (syntree--fill (syntree--p-get :rooftop) w nil stem))
         (raw-bottom (apply #'concat
                            (make-list w (syntree--p-get :roofbottom))))
         (top (if (> w 2)
                  (replace-regexp-in-string
                   "^.\\|.$"
                   (syntree--p-get :rooftopangle t)
                   raw-top)
                raw-top))
         (bottom (if (> w 2)
                     (replace-regexp-in-string
                      "^.\\|.$"
                      (syntree--p-get :roofbottomangle t)
                      raw-bottom)
                raw-bottom)))
    (unless (< w min) (list top bottom))))

(defun syntree--homogenize-length (return-obj width right-or-left &rest str)
  "Pad STR with whitespace to make it as wide as the longest STR.

STR are either strings or lists of strings.  If WIDTH is non-nil,
make all strings in the output at least as wide as WIDTH.

If RIGHT-OR-LEFT is 'right' or 'left', just pad at the right or
left of S, respectively.  If it is nil, pad on both sides.

If RETURN-OBJ is 'node', return a node object instead of a list
of strings (that is, a list whose car is an integer, the width of
the lists in the cdr.).

Always remove empty strings from the input."
  (let* ((strings (thread-last
		    str
		    (apply #'list)
		    (flatten-tree)
		    (cl-delete-if-not #'(lambda (x) (or x
							(stringp x)
							(> (length x) 0))))))
         ;; We make the grand union of all STR and we only keep the
         ;; strings
         (w (or width (apply #'max (mapcar #'length strings))))
         ;; The node will be the list whose car is the max length in
         ;; integer and the cdr is the list of strings, each filled as
         ;; to be max-length long.
         (n (cons w
                  (mapcar #'(lambda (s) (syntree--fill " " w
                                                       right-or-left s))
                          strings))))
    ;; We return the node unless RETURN-NODE is nil, in which case we
    ;; only return the list of strings.
    (if (eq return-obj 'node) n (cdr n))))

(defun syntree--split-string (input-obj return-obj direction s
                                        &optional branch-string)
  "Take a string S and return the label or the terminal.

If INPUT-OBJ is 'leaf', also split S between a label and the
text.  If it is 'label', don't do anything about colons.

BRANCH-STRING is the string containing the branch connecting.

Always return a list of strings: if RETURN-OBJ is 'label', return
the label, if it is 'terminal', return the terminal.  The value
of RETURN-OBJ is ignored if INPUT-OBJ is 'label' (labels are not
divided between labels and text).

Whatever comes before the first \":\" is going to be the label.

If there is no label and RETURN-OBJ is 'label', return a list
containing the empty string.

If DIRECTION is ':right' or ':left', return a list with the
strings transformed accordingly."
  ;; If the label is empty, just return nil
  (when (and (eq input-obj 'label) (string-blank-p (string-trim s))) nil)
  (let* ((leaf-split (split-string (string-trim s) ":"))
         (text (if (= (length leaf-split) 1) leaf-split (cdr leaf-split)))
         ;; I actually can't reconstruct now why this weird inversion
         ;; is needed.  Probably because the tree is internally
         ;; constructed as if it grew to the right.  Now if PAD is
         ;; non-nil, it means that the growth is horizontal
         (dir (when (or (eq direction 'right) (eq direction 'left))
                direction))
         (actual-str (cond ((eq input-obj 'label) s)
                           ;; for the next two conditions, input-obj
                           ;; is assumed to be leaf!
                           ((eq return-obj 'label)
                            (if (= (length leaf-split) 1)
                                ""
                              (string-remove-prefix "_" (car leaf-split))))
                           ((eq return-obj 'terminal)
                            (string-join text ":"))))
         (string-list (thread-last actual-str
                                   (syntree--split-and-wrap)
                                   (syntree--reverse direction)))
         (p-string-list (thread-last string-list
                                     (syntree--homogenize-length 'list nil dir)
                                     (syntree--verticalize-text direction))))
    (if (not branch-string)
        p-string-list
      (let* ((br-segments (syntree--split-branch branch-string))
             (label-width (apply #'max (mapcar #'length string-list))))
        (if (> label-width (length (car br-segments)))
            ;; If the label is larger than the branch (not the branch
            ;; string, the branch!), then just center each string in
            ;; label-list, which means just give me the same thing as
            ;; you'd have without the branch
            p-string-list
          ;; Otherwise, center each string in label-list relative to the
          ;; branch, and then pad left and right with the left and right
          ;; offsets.
          (thread-last
            string-list
            (syntree--verticalize-text direction)
            (mapcar #'(lambda (x) (syntree--fill " "
                                                 (length (car br-segments))
                                                 nil
                                                 x)))
            (mapcar #'(lambda (x) (concat (cadr br-segments)
                                          x
                                          (caddr br-segments))))))))))

(defun syntree--gen-leaf (s)
  "Convert the string S into a node.

Return a list whose car is the numerical value of the width of
the node, and whose cdr is a list of strings."
  (let* ((direction (syntree--p-get :growing))
         (label (or (syntree--split-string 'leaf 'label direction s)
                    '("")))
         (text (or (syntree--split-string 'leaf 'terminal direction s)
                   '("")))
         (max-w-label (apply #'max (mapcar #'length label)))
         (max-w-text (apply #'max (mapcar #'length text)))
         (roof-w (+ max-w-text (* 2 (syntree--p-get :roofwidth))))
         ;; roof will be nil if the width ends up being less than 3
         (roof (syntree--gen-roof max-w-text))
         (roof-p (and (string-match ":" s)
                      (string-prefix-p "_" s)
                      roof))
         (roof-size (if roof-p roof-w 0))
         (roof-string (when roof-p roof))
         (max-size (apply #'max (list roof-size max-w-text max-w-label)))
         ;; If :height is 0 or less, we still want 1 stem
         (height (if (> 1 (syntree--p-get :height))
                     1
                   (syntree--p-get :height)))
         (stem-list (thread-last (syntree--p-get :stem)
                                 (make-list height)
                                 (syntree--homogenize-length nil
                                                             max-size
                                                             nil))))
    (thread-last (list stem-list label roof-string text)
                 (apply #'syntree--homogenize-length 'node nil nil)
                 (cl-delete-if #'(lambda (s) (and (stringp s)
                                                  (string-blank-p s)))))))

(defun syntree--string-to-list (s)
  "Convert string S into a list of strings."
  (let ((list-of-chars (string-to-list s)))
    (mapcar #'char-to-string list-of-chars)))

(defun syntree--replace-terminal-strings (l)
  "Replace every terminal string in L with the node it represents.

Every string in L that is not the first member of a list is a
terminal, and before building the tree with `syntree--merge' it
must be replaced with a node: a list whose car is a numerical
value (the width of the node) and whose cdr is a list of strings.
Each node is the output of `syntree--gen-leaf'."
  (cons (car l)
        (thread-last
          l
          (cdr)
          (mapcar #'(lambda (x) (if (stringp x)
                                    (syntree--gen-leaf x)
                                  (syntree--replace-terminal-strings x)))))))

;;; Building subtrees

(defun syntree--adjust-depth (node max-depth)
  "Make NODE reach MAX-DEPTH, according to value of ':one-line'."
  (let* ((spaces (make-string (car node) ?\s))
         (stem (cadr node))
         (original-list-of-strings (cdr node))
         (diff (- max-depth (length original-list-of-strings))))
    (cons (car node)
          (if (syntree--p-get :one-line) ; add stem strings to the top
              (append (make-list diff stem) original-list-of-strings)
            (append original-list-of-strings ; add empty strings to the bottom
                   (make-list diff spaces))))))

(defun syntree--concatenate-nodes (l)
  "L being a list of nodes, return the result of concatenating them.

The depth of the individual nodes is adjusted and the the
resulting node is obtained by zipping the lists of strings of
each node, concatenating them with the desidered amount of
padding."
  (let* ((max-depth (apply #'max (mapcar #'(lambda (x) (1- (length x))) l)))
         (terminals-list
          (mapcar #'(lambda (x) (syntree--adjust-depth x max-depth)) l))
         (str (mapcar #'cdr terminals-list))
         (padding-base (if (> 0 (syntree--p-get :hspace))
                           0
                         (syntree--p-get :hspace)))  ; can't have
                                                     ; negative
                                                     ; padding
         ;; and at the very least there will be two spaces between nodes
         (pad-string (thread-first padding-base
                                   (+ 2)
                                   (make-string ?\s)))
         (total-width (thread-last pad-string
                                   (length)
                                   (* (1- (length l)))
                                   (+ (apply #'+ (mapcar #'car l))))))
    (cons total-width
          (syntree--zip-concat-lists-of-strings pad-string str))))

(defun syntree--zip-concat-lists-of-strings (pad l)
  "Given a list of strings L, zip them with separator PAD.

The strings in L are expected to be the same length: this
function returns a list of strings.  The first string in the
output list is the result of concatenating the first character of
each string in L, the second the result of concatenating the
second, etc.

PAD is a string that function as a separator like in
`mapconcat'."
  (cond ((= (length l) 1)
         (car l))
        ((= (length l) 2)
         (cl-mapcar (lambda (x y) (concat x pad y))
                    (car l)
                    (cadr l)))
        (t (cl-mapcar (lambda (x y) (concat x pad y))
                      (car l)
                      (syntree--zip-concat-lists-of-strings pad (cdr l))))))

(defun syntree--draw-branch (s &optional return-only-center)
  "Given string of stems S, return a branch string connecting them.

If RETURN-ONLY-CENTER is non-nil, only return a string containing
whitespace and the value of ':stem' according to
`syntree--style'."
  (if (and (= (length (replace-regexp-in-string "\s" "" s)) 1)
           (not return-only-center))
      ""
    (let* ((stem-re (regexp-quote (syntree--p-get :stem)))
           (not-stem-re (concat "[^" stem-re "]"))
           (one-stem-p (thread-last s
                                    (replace-regexp-in-string not-stem-re "")
                                    (length)
                                    (eq 1)))
           (begin-re (concat "^.*" stem-re))
           (end-re (concat stem-re ".*$"))
           (left-offset (replace-regexp-in-string end-re "" s))
           (right-offset (replace-regexp-in-string begin-re "" s))
           (branch-width (- (length s)
                            (+ (1+ (length left-offset))
                               (1+ (length right-offset)))))
           (only-center (concat " "
                                (syntree--fill " " branch-width nil
                                               (syntree--p-get :stem))
                                " ")))
      (cond ((and one-stem-p return-only-center) only-center)
            (return-only-center
             (concat left-offset only-center right-offset))
            (t
             (let* ((spaces-list (thread-first s
                                               (string-trim left-offset
                                                            right-offset)
                                               (split-string stem-re t)))
                    (branch (thread-last
                              spaces-list
                              (mapcar #'(lambda (x)
                                          (replace-regexp-in-string
                                           "[[:space:]]"
                                           (syntree--p-get :hbranch)
                                           x)))
	                      (funcall
                               #'(lambda (x)
                                   (string-join
                                    x (syntree--p-get :intersection))))
	                      (funcall
                               #'(lambda (x)
                                   (concat
                                    (syntree--p-get :l-intersection t)
				    x
				    (syntree--p-get :r-intersection t))))))
                    (center (if (characterp (syntree--p-get :branchcenter))
                                (syntree--p-get :branchcenter)
                              (string-to-char (syntree--p-get :branchcenter))))
                    (index-center (save-match-data
                                    (string-match "[^[:space:]]"
                                                  only-center
                                                  nil)))
                    (branch-list (string-to-list branch)))
               (setf (nth index-center branch-list) center)
               (concat left-offset
                       (apply #'string branch-list)
                       right-offset)))))))

(defun syntree--split-branch (s)
  "Return list of branch, left and right offset.
S is the string containing the branch."
  (list (replace-regexp-in-string "\s" "" s)
	(thread-last s
		     (replace-regexp-in-string "\s*$" "")
		     (replace-regexp-in-string "[^\s]" ""))
	(thread-last s
		     (replace-regexp-in-string "^\s*" "")
		     (replace-regexp-in-string "[^\s]" ""))))

(defun syntree--gen-subtree (lab d)
  "Return a branching node given a label and a list of vertical nodes.

LAB is the label string, D the list of daughter nodes."
  (let* ((dir (syntree--p-get :growing))
         (complex-node (syntree--concatenate-nodes d))
         (raw-subtree (cddr complex-node))
         (raw-stems (cadr complex-node))
         (stems-tip-str (with-temp-buffer
                          (insert raw-stems)
                          (goto-char (point-min))
                          (let ((re (concat "^\\(\s*\\)"
		                            "[^[:space:]]"
		                            "\\(.*\\)"
		                            "[^[:space:]]"
		                            "\\(\s*\\)$")))
                            (if (re-search-forward re nil t)
                                (concat (match-string-no-properties 1)
	                                (syntree--p-get :l-stem t)
	                                (match-string-no-properties 2)
	                                (syntree--p-get :r-stem t)
	                                (match-string-no-properties 3))
                              ;; search failed, meaning there is only
                              ;; one stem:
                              raw-stems))))
         (branch (syntree--draw-branch raw-stems))
         (only-center (syntree--draw-branch raw-stems 'only-center))
         (top-long-stem (if (> (syntree--p-get :height) 1)
                            (make-list (syntree--p-get :height)
                                       only-center)
                          ""))
         (label (syntree--split-string 'label nil dir lab branch))
         (max-width (thread-last (list stems-tip-str label branch only-center)
                                 (flatten-tree)
                                 (mapcar #'length)
                                 (apply #'max))))
    (syntree--homogenize-length
     'node max-width nil
     (cl-delete-if #'string-blank-p
                   (flatten-tree (list only-center
                                       label
                                       top-long-stem
                                       branch
                                       stems-tip-str
                                       raw-subtree))))))

;;; Building the tree

(defun syntree--merge (l)
  "Recursively generate subtrees of list L.  Return the top node."
  (if (ignore-errors
        (and (stringp (car l))
             (thread-last l
                          (cdr)
                          (mapcar #'car)
                          (cl-every #'numberp))
             (cl-every #'numberp (mapcar #'car (cdr l)))))
      ;; l is a ready constituent: its car is a string and its cdr is
      ;; a list of nodes (lists whose car is an integer).
      (syntree--gen-subtree (car l) (cdr l))
    ;; l is not a ready constituent: there is one or more subtrees to
    ;; generate before we can merge
    (syntree--merge
     (cons (car l)
           (mapcar #'(lambda (x) (if (numberp (car x))
                                     (identity x)
                                   (syntree--merge x)))
                   (cdr l))))))

(defun syntree--sublists (l)
  "Return list of all sublists of L."
  (cons l
        (when (listp l)
          (mapcan (lambda (x) (when (listp x)
                                (syntree--sublists x)))
                  (identity l)))))

(defun syntree--main (s &optional style changes)
  "Return the plain text tree given input string S.

If the major mode is not `syntree-mode', this function sets the
value for `syntree--current-style' as the defined one for
`syntree-default-style', unless optional argument STYLE is
non-nil.  In that case, STYLE is the symbol corresponding to a
style in `syntree--styles-alist', the style according to which
the tree will be built.

CHANGES is an alist that maps properties (members of
`syntree-properties'): the values so specified override the ones
in `syntree-default-style' or in STYLE."
  (unless (eq major-mode 'syntree-mode)
    (setq syntree--styles-alist
	  (mapcar #'(lambda (x) (cons (map-elt x :name) x))
		  syntree-styles-list))
    (unless syntree-default-style
      (setq syntree-default-style 'basic))
    (setq syntree--current-style
          (syntree--copy-style (map-elt syntree--styles-alist
                                        (or style
					    syntree-default-style)))))
  (when changes
    (dolist (c changes)
      (map-put! syntree--current-style (car c) (cdr c))))
  (let* ((dir (syntree--p-get :growing))
         (raw-output (thread-last s
                                  (read)
                                  (syntree--replace-terminal-strings)
                                  (syntree--merge)
                                  (cddr)
                                  (syntree--horizontalize-text dir)
                                  (cl-delete-if #'string-blank-p))))
    (thread-first (syntree--reverse nil raw-output)
                  (string-join "\n")
                  (concat "\n"))))

;;; Interactive part

(defun syntree--copy-style (style-list)
  "Return deep copy of STYLE-LIST."
  (let ((copy))
    (dolist (x style-list)
      (push x copy))
    (nreverse copy)))

(defun syntree--check-input ()
  "Check that the string is valid for syntree to interpret.

Return an informative error message if it is not."
  (save-excursion
    (goto-char (point-min))
    (let (output)
      (catch 'error
        (unless (search-forward "(" nil t)
          (setq output "")
          (throw 'error nil))
        (let ((candidate
               (buffer-substring-no-properties (progn (search-backward "(")
                                                      (point))
                                               (progn (forward-sexp 1 nil)
                                                      (point)))))
          (unless (ignore-errors (read candidate))
            (setq output "")
            (throw 'error nil))
          (unless (thread-last candidate
                               (read)
                               (flatten-tree)
                               (cl-every #'stringp))
            (setq output "Each element must be a string")
            (throw 'error nil))
          (unless (thread-last candidate
                               (read)
                               (syntree--sublists)
                               (cl-every #'(lambda (x) (> (length x) 1))))
            (setq output "Each constituent must be labeled")
            (throw 'error nil))))
      output)))

(defvar syntree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<left>") 'syntree-reduce-padding)
    (define-key map (kbd "M-<right>") 'syntree-increase-padding)
    (define-key map (kbd "M-<up>") 'syntree-reduce-height)
    (define-key map (kbd "M-<down>") 'syntree-increase-height)
    (define-key map (kbd "C-c C-s") 'syntree-change-style)
    (define-key map (kbd "C-c C-v") 'syntree-change-value)
    (define-key map (kbd "C-c C-c") 'syntree-done)
    map)
  "Keymap for `syntree-mode'.")

(defvar-local syntree--original-marker nil)
(defvar-local syntree--input-hash nil)
(defvar-local syntree--output-window nil)
(defvar-local syntree--output-buffer nil)
(defvar-local syntree--input-buffer nil)
(defvar-local syntree--source nil)
(defvar-local syntree--buffer-type nil)

(defvar syntree--idle-timer nil)

(defun syntree-done ()
  "Add source and output to kill ring, kill syntree buffers."
  (interactive)
  (save-excursion
    (unless (eq syntree--buffer-type 'input)
      (select-window (get-buffer-window syntree--input-buffer) t))
    (let ((input-buf (current-buffer))
          (target syntree--original-marker)
          (output-buf syntree--output-buffer))
      (if (syntree--check-input)
          (when (yes-or-no-p "No valid input found.  Exit syntree-mode? ")
            (kill-buffer output-buf)
            (delete-frame)
            (kill-buffer input-buf)
            (goto-char target))
        (syntree--refresh)
        (kill-new (progn (goto-char (point-min))
                         (search-forward "(" nil t)
                         (buffer-substring-no-properties
                          (progn (search-backward "(")
                                 (point))
                          (progn (forward-sexp 1 nil)
                                 (point)))))
        (with-current-buffer output-buf
          (kill-new (buffer-substring-no-properties (point-min)
                                                    (point-max))))
        (kill-buffer output-buf)
        (delete-frame)
        (kill-buffer input-buf)
        (goto-char target)))))

;;;###autoload
(defun syntree-new ()
  "Create new frame with input and output buffers for syntree.

This function also sets the value of `syntree--style' to the one
of `syntree-default-style' (`syntree--style-basic', if the latter
is not already specified).

Unless it's already active, this function also activates the idle
timer that redraws the tree."
  (interactive)
  (when (eq major-mode 'syntree-mode)
    (user-error "Already in syntree mode"))
  (let ((old-position (point-marker))
        (input-buff (generate-new-buffer "syntree-input" nil))
        (message-log-max nil))
    (display-buffer-other-frame input-buff)
    (with-selected-window (get-buffer-window input-buff t)
      (syntree-mode)
      (setq syntree--buffer-type 'input)
      (setq syntree--input-hash (buffer-hash))
      (set-window-dedicated-p (selected-window) t)
      (let ((output-buffer (generate-new-buffer "syntree-output" nil)))
        (display-buffer-in-side-window output-buffer '((side . bottom)
                                                       (slot . 1)
                                                       (dedicated . p)))
        (window-resize (selected-window)
                       (- 5 (window-height (selected-window))))
        (setq syntree--output-window (next-window)
              syntree--output-buffer (window-buffer syntree--output-window)
              syntree--original-marker old-position)
        (select-window (get-buffer-window output-buffer))
        ;; we want 'truncate-lines' to be t in the output buffer, so
        ;; that large trees are displayed faithfully
        (syntree-mode)
        (font-lock-mode -1)
        (setq syntree--buffer-type 'output
              truncate-lines t
              syntree--input-buffer input-buff
              syntree--original-marker old-position)
        (read-only-mode 1)
        (select-window (get-buffer-window input-buff)))))
  (unless syntree--idle-timer
    (setq syntree--idle-timer
          (run-with-idle-timer 0.1 t #'syntree--on-timer))))

(defun syntree--refresh ()
  "Redraw the tree in `syntree--output-window'."
  (setq syntree--input-hash (buffer-hash))
  (setq syntree--current-style syntree--style)
  (save-excursion
    (let ((input-w (selected-window))
          (message-log-max nil))
      (goto-char (point-min))
      (if (syntree--check-input)
          (let ((err-mess (syntree--check-input)))
            (message err-mess))
        (let* ((source (progn (goto-char (point-min))
                              (search-forward "(" nil t)
                              (buffer-substring-no-properties
                               (progn (search-backward "(")
                                      (point))
                               (progn (forward-sexp 1 nil)
                                      (point)))))
               (output (syntree--main source)))
          (select-window syntree--output-window t)
          (read-only-mode -1)
          (setq syntree--source source)
          (delete-region (point-min) (point-max))
          (insert output)
          (read-only-mode 1)))
      (select-window input-w t))))

(defun syntree--on-timer ()
  "Call `syntree--refresh' if in the right circumstances.

Ignore errors.  Only call the function is the current buffer is a
syntree input buffer, the window specified there as
`syntree--output-window' is currently live, and the input buffer
hasn't changed since last time `syntree--refresh' was called in
this buffer."
  (when (and (eq major-mode 'syntree-mode)
             (eq syntree--buffer-type 'input)
             (window-live-p syntree--output-window)
             (not (equal (buffer-hash) syntree--input-hash)))
    (ignore-errors (syntree--refresh))))

(defun syntree--cancel-timer ()
  "Cancel `syntree--idle-timer' from the list of idle timers.

Only do this if there is currently no buffer in `syntree-mode'
except for the current one.

This function is added buffer-locally on `kill-buffer-hook' when
`syntree-mode' is initialized."
  (let ((other-buffers (delete (current-buffer) (buffer-list))))
    (unless (cl-some #'(lambda (b)
                         (let ((m (with-current-buffer b major-mode)))
                           (eq m 'syntree-mode)))
                     other-buffers)
      (cancel-timer syntree--idle-timer)
      (setq syntree--idle-timer nil))))

(defun syntree-increase-padding ()
  "Increase horizontal spacing.

Call `syntree--refresh' to redraw the tree."
  (interactive)
  (save-excursion
    (unless (eq syntree--buffer-type 'input)
      (select-window (get-buffer-window syntree--input-buffer) t))
    (let ((curr (syntree--p-get :hspace))
          (message-log-max nil))
      (map-put! syntree--style :hspace (1+ curr))
      (message (format ":hspace set to %s" (1+ curr))))
    (syntree--refresh)))

(defun syntree-reduce-padding ()
  "Reduce horizontal spacing.

Call `syntree--refresh' to redraw the tree."
  (interactive)
  (save-excursion
    (unless (eq syntree--buffer-type 'input)
      (select-window (get-buffer-window syntree--input-buffer) t))
    (let ((message-log-max nil)
          (curr (syntree--p-get :hspace)))
      (if (< curr 1)
          (message "Cannot reduce more")
        (map-put! syntree--style :hspace (1- curr))
        (message (format ":hspace set to %s" (1- curr)))
        (syntree--refresh)))))

(defun syntree-increase-height ()
  "Increase vertical lenght of stems.

Call `syntree--refresh' to redraw the tree."
  (interactive)
  (save-excursion
    (unless (eq syntree--buffer-type 'input)
      (select-window (get-buffer-window syntree--input-buffer) t))
    (let* ((curr (syntree--p-get :height))
           (new (if (> 1 curr) 2 (1+ curr)))
           (message-log-max nil))
      (map-put! syntree--style :height new)
      (message (format ":height set to %s" new)))
    (syntree--refresh)))

(defun syntree-reduce-height ()
  "Reduce vertical lenght of stems.

Call `syntree--refresh' to redraw the tree."
  (interactive)
  (save-excursion
    (unless (eq syntree--buffer-type 'input)
      (select-window (get-buffer-window syntree--input-buffer) t))
    (let ((message-log-max nil)
          (curr (syntree--p-get :height)))
      (if (< curr 2)
          (message "Cannot reduce more")
        (map-put! syntree--style :height (1- curr))
        (message (format ":height set to %s" (1- curr)))
        (syntree--refresh)))))

(defun syntree-change-value ()
  "Change a value in `syntree--style', with completion.

Call `syntree--refresh' to redraw the tree."
  (interactive)
  (save-excursion
    (unless (eq syntree--buffer-type 'input)
      (select-window (get-buffer-window syntree--input-buffer) t))
    (let* ((p (completing-read "Property to change: "
                               syntree-properties))
           (property (intern p))
           (curr (syntree--p-get property))
           (pr (format "(Current is %s): " curr))
           (value (cond ((eq property :one-line)
                         (not curr))
                        ((or (eq property :hspace)
                             (eq property :height)
                             (eq property :roofwidth)
                             (eq property :roofminwidth)
                             (eq property :word-wrap))
                         (read-number pr))
                        ((eq property :growing)
                         (intern (completing-read ": "
                                                  '("up" "down"
                                                    "left" "right"))))
                        (t (read-string pr)))))
      (map-put! syntree--style property value)
      (syntree--refresh))))

(defun syntree-change-style ()
  "Change value of `suntree--current-style' with completion.

Call `syntree--refresh' to redraw the tree."
  (interactive)
  (unless (eq syntree--buffer-type 'input)
    (select-window (get-buffer-window syntree--input-buffer) t))
  (setq syntree--styles-alist
	(mapcar #'(lambda (x) (cons (map-elt x :name) x))
		syntree-styles-list))
  (let ((style (intern (completing-read
                        (format "Change %s to: "
                                (map-elt syntree--style :name))
                        (map-keys syntree--styles-alist)))))
    (setq syntree--style
          (syntree--copy-style (map-elt syntree--styles-alist style)))
    (syntree--refresh)))

(defun syntree-info ()
  "Read the documentation for `syntree' in the info system."
  (interactive)
  (info "syntree"))

(define-derived-mode syntree-mode
  ;; We derive it from 'emacs-lisp-mode' so that we can leverage on
  ;; the lisp stuff to have a nice editing of the input string, which
  ;; is really a nested list of strings.  Font lock will be turned off
  ;; in the output buffer, which should be enough to make this choice
  ;; no be in the way there.
  emacs-lisp-mode "Syntree"
  "Major mode for Syntree."
  :interactive nil
  :after-hook
  ;; We don't want unnecessary idle timers to stick along.
  (progn
    (add-hook 'kill-buffer-hook #'syntree--cancel-timer nil t)
    (setq syntree--styles-alist
	(mapcar #'(lambda (x) (cons (map-elt x :name) x))
		syntree-styles-list))
    (setq syntree--style
          (map-elt syntree--styles-alist syntree-default-style))))

;;; Org-mode integration

(add-to-list 'org-structure-template-alist
             '("syntree" . "src syntree"))

(defun org-babel-execute:syntree (body params)
  "Execute a block containing syntree input."
  (let ((style (cdr (assq :style params)))
	(changes (cl-remove-if-not
                  (lambda (x) (member (car x) syntree-properties))
		  params)))
    (syntree--main body style changes)))

(provide 'syntree)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; syntree.el ends here
