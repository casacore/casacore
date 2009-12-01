;;; glish-mode.el --- Glish code editing commands for Emacs & XEmacs
;;;
;;; $Id$

;; Copyright (C) 1996-1998  Jeffrey A. Uphoff, NRAO/AUI.
;; modified to work under emacs (20.6) by Malte Marquarding

;; Based on Perl mode as distributed with XEmacs version 19.14.
;; Here's what those folks have to say:

;; Copyright (C) 1990, 1994  Free Software Foundation, Inc.
;; Author: William F. Mann
;; Maintainer: FSF
;; Adapted-By: ESR
;; Keywords: languages
;; Adapted from C code editing commands 'c-mode.el', Copyright 1987 by the
;; Free Software Foundation, under terms of its General Public License.
;; This file is part of XEmacs.
;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; Synched up with: FSF 19.30.
;; This code is based on the 18.53 version c-mode.el, with extensive
;; rewriting.  Most of the features of c-mode survived intact.
;; I added a new feature which adds functionality to TAB; it is controlled
;; by the variable glish-tab-to-comment.  With it enabled, TAB does the
;; first thing it can from the following list:  change the indentation;
;; move past leading white space; delete an empty comment; reindent a
;; comment; move to end of line; create an empty comment; tell you that
;; the line ends in a quoted string, or has a # which should be a \#.
;; If your machine is slow, you may want to remove some of the bindings
;; to electric-glish-terminator.  I changed the indenting defaults to be
;; what Larry Wall uses in perl/lib, but left in all the options.

;; To enter glish-mode automatically, add (autoload 'glish-mode "glish-mode")
;; to your .emacs file and change the first line of your glish script to:
;; #!/usr/local/glish/bin/glish --	 # -*-Glish-*-
;; With argments to glish:
;; #!/usr/local/glish/bin/glish -P-	 # -*-Glish-*-
;; To handle files named 'filename.g' (or 'filename.gp');, add something like
;; (setq auto-mode-alist (append '(("\\.g\\(p\\)?$" . glish-mode))
;;  			         auto-mode-alist))
;; to your .emacs file.

;; JAU: Everything prefixed with ";;x" needs to be rewritten for Glish.

;;x I also tuned a few things: comments starting in column zero are left
;;x there by indent-glish-exp; glish-beginning-of-function goes back to
;;x the first open brace/paren in column zero, the open brace in
;;x 'func... {', or the equal sign in 'format ... ='; indent-glish-exp
;;x (meta-^q) indents from the current line through the close of the next
;;x brace/paren, so you don't need to start exactly at a brace or paren.

;;x Known problems (these are all caused by limitations in the Emacs Lisp
;;x parsing routine (parse-partial-sexp), which was not designed for such
;;x a rich language; writing a more suitable parser would be a big job):
;;x 1)  Regular expression delimiters do not act as quotes, so special
;;x	  characters such as `'"#:;[](){} may need to be backslashed
;;x	  in regular expressions and in both parts of s/// and tr///.
;;x 2)  The globbing syntax <pattern> is not recognized, so special
;;x	  characters in the pattern string must be backslashed.
;;x 3)  The q, qq, and << quoting operators are not recognized; see below.
;;x 4)  \ (backslash) always quotes the next character, so '\' is
;;x	  treated as the start of a string.  Use "\\" as a work-around.
;;x 5)  To make variables such a $' and $#array work, glish-mode treats
;;x	  $ just like backslash, so '$' is the same as problem 5.
;;x 6)  Unfortunately, treating $ like \ makes ${var} be treated as an
;;x	  unmatched }.  See below.
;;x 7)  When ' (quote) is used as a package name separator, glish-mode
;;x	  doesn't understand, and thinks it is seeing a quoted string.

;;x Here are some ugly tricks to bypass some of these problems:  the glish
;;x expression /`/ (that's a back-tick) usually evaluates harmlessly,
;;x but will trick glish-mode into starting a quoted string, which
;;x can be ended with another /`/.  Assuming you have no embedded
;;x back-ticks, this can used to help solve problem 3:
;;x
;;x	/`/; $ugly = q?"'$?; /`/;
;;x
;;x To solve problem 6, add a /{/; before each use of ${var}:
;;x	/{/; while (<${glob_me}>) ...
;;x
;;x Problem 7 is even worse, but this 'fix' does work :-(
;;x	$DB'stop#'
;;x	    [$DB'line#'
;;x	     ] =~ s/;9$//;

; (defgroup glish nil
;   "Major mode for editing glish code."
;   :prefix "glish-"
;   :group 'languages)

(defvar glish-mode-abbrev-table nil
  "Abbrev table in use in glish-mode buffers.")

(define-abbrev-table 'glish-mode-abbrev-table ())

(defvar glish-mode-map ()
  "Keymap used in Glish mode.")

(if glish-mode-map
    ()
  (setq glish-mode-map (make-sparse-keymap))
  (define-key glish-mode-map "\C-c\C-c" 'comment-region)
  (define-key glish-mode-map "{" 'electric-glish-terminator)
  (define-key glish-mode-map "}" 'electric-glish-terminator)
  (define-key glish-mode-map ";" 'electric-glish-terminator)
  (define-key glish-mode-map "\e\C-a" 'glish-beginning-of-function)
  (define-key glish-mode-map "\e\C-e" 'glish-end-of-function)
  (define-key glish-mode-map "\e\C-m" 'mark-glish-function)
  (define-key glish-mode-map "\e\C-q" 'indent-glish-exp)
  (define-key glish-mode-map "\177" 'backward-delete-char-untabify)
  (define-key glish-mode-map "\t" 'glish-indent-command))

(defvar glish-mode-syntax-table nil
  "Syntax table in use in glish-mode buffers.")

;; JAU: Needs work.
(if glish-mode-syntax-table
    ()
  (setq glish-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n ">" glish-mode-syntax-table)
  (modify-syntax-entry ?# "<" glish-mode-syntax-table)
  (modify-syntax-entry ?$ "\\" glish-mode-syntax-table)
  (modify-syntax-entry ?% "." glish-mode-syntax-table)
  (modify-syntax-entry ?& "." glish-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" glish-mode-syntax-table)
  (modify-syntax-entry ?* "." glish-mode-syntax-table)
  (modify-syntax-entry ?+ "." glish-mode-syntax-table)
  (modify-syntax-entry ?- "." glish-mode-syntax-table)
  (modify-syntax-entry ?/ "." glish-mode-syntax-table)
  (modify-syntax-entry ?< "." glish-mode-syntax-table)
  (modify-syntax-entry ?= "." glish-mode-syntax-table)
  (modify-syntax-entry ?> "." glish-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" glish-mode-syntax-table)
  (modify-syntax-entry ?` "\"" glish-mode-syntax-table)
  (modify-syntax-entry ?| "." glish-mode-syntax-table))


(make-face 'font-lock-glish-pragma-face)
(set-face-underline-p 'font-lock-glish-pragma-face t)
(set-face-foreground 'font-lock-glish-pragma-face "darkseagreen")
(make-face 'font-lock-glish-incl-face)
(set-face-foreground 'font-lock-glish-incl-face "yellow")

;; JAU: Needs to handle 'const' function/closure definitions.
;; [JAU, 1997.10.15: is this still true?]
   ;; Pragma entry could use some tuning for whitespace and allowed statements:
   ;; pragma include once
   ;; pragma shared user
   ;; pragma shared group
   ;; pragma shared world
;;   

(defconst glish-font-lock-keywords-1
  (list
    ;;
    ;; Fontify function and subsequence names in declarations.
   '("[ \t]*\\(func\\(tion\\)?\\|subseq\\(uence\\)?\\)[ \t]+\\([^ \t(){]+\\)[ \t]*\\((.*)\\)?[ \t]*[{]" 1 font-lock-function-name-face)
   '("\\([^ \t\n]+\\)[ \t]*:=[ \t]*\\(func\\(tion\\)?\\|subseq\\(uence\\)?\\)" 1 font-lock-function-name-face)
   '("\\(include[ \t]+\\)['\"]+" 1 'font-lock-glish-incl-face)
   '("\\<\\(pragma.*[^;\n]\\)\\>" 1 'font-lock-glish-pragma-face))
  "Subdued level highlighting for Glish mode.")

(defconst glish-font-lock-keywords-2
  (append glish-font-lock-keywords-1
   (list
    ;;
    ;; Fontify keywords, except those fontified otherwise.
    ;;
    (concat "\\<\\("
	    "await\\([ \t]+only\\)?\\|break\\|"
	    "c\\(lient\\|onst\\)\\|d\\(ie\\|o\\)\\|\\(de\\)?activate\\|"
	    "e\\(lse\\|val\\|x\\(cept\\|it\\)\\)\\|"
	    "f\\(ail\\|or\\|unc\\(tion\\)?\\)\\|if\\|link\\|"
	    "re\\(f\\|turn\\)\\|to\\|"
	    "subseq\\(uence\\)?\\|"
	    "un\\(less\\|link\\|til\\)\\|val\\|"
	    "wh\\(enever\\|ile\\)\\|"
	    "\\)\\>")
    ;;
    ;; Fontify local and my keywords as types.
    '("\\<\\(local\\|global\\|wider\\)\\>" . font-lock-type-face)
    ;;
    ;; Fontify function, variable and file name references.

    ;;
    ;; Fontify keywords with/and labels as we do in `c++-font-lock-keywords'.
    '("\\<\\(continue\\|next\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))))
  "Gaudy level highlighting for Glish mode.")


;; We use a couple of locally-defined faces to prevent "contaminating"
;; other modes, such as C++.

(defvar glish-font-lock-keywords glish-font-lock-keywords-1
  "Default expressions to highlight in Glish mode.")


(defvar glish-indent-level 4
  "*Indentation of Glish statements with respect to containing block.")

;; JAU: This goes out one extra set for opening {'s after a continued
;; function declaration.  Fix!
(defvar glish-continued-statement-offset 4
  "*Extra indent for lines not starting new statements.")

(defvar glish-continued-brace-offset -4
  "*Extra indent for substatements that start with open-braces.
This is in addition to `glish-continued-statement-offset'.")

(defvar glish-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")

(defvar glish-brace-imaginary-offset 0
  "*Imagined indentation of an open brace that actually follows a statement.")

(defvar glish-tab-always-indent t
  "*Non-nil means TAB in Glish mode always indents the current line.
Otherwise it inserts a tab character if you type it past the first
nonwhite character on the line.")

;; I changed the default to nil for consistency with general Emacs
;; conventions -- rms.
(defvar glish-tab-to-comment nil
  "*Non-nil means TAB moves to eol or makes a comment in some cases.
For lines which don't need indenting, TAB either indents an
existing comment, moves to end-of-line, or if at end-of-line already,
create a new comment.")

(defvar glish-nochange ";?#\\|\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:"
  "*Lines starting with this regular expression are not auto-indented.")

(defvar glish-mode-hook nil
  "Invoked on entry to glish-mode.")

;;;###autoload
(defun glish-mode ()
  "Major mode for editing Glish code.
Expression and list commands understand all Glish brackets.
Tab indents for Glish code.
Comments are delimited with # ... \\n.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{glish-mode-map}
Variables controlling indentation style:
 glish-tab-always-indent
    Non-nil means TAB in Glish mode should always indent the current line,
    regardless of where in the line point is when the TAB command is used.
 glish-tab-to-comment
    Non-nil means that for lines which don't need indenting, TAB will
    either delete an empty comment, indent an existing comment, move
    to end-of-line, or if at end-of-line already, create a new comment.
 glish-nochange
    Lines starting with this regular expression are not auto-indented.
 glish-indent-level
    Indentation of Glish statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 glish-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 glish-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `glish-continued-statement-offset'.
 glish-brace-offset
    Extra indentation for line if it starts with an open brace.
 glish-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Various indentation styles:       K&R  BSD  BLK  GNU  LW
  glish-indent-level                5    8    0    2    4
  glish-continued-statement-offset  5    8    4    2    4
  glish-continued-brace-offset      0    0    0    0   -4
  glish-brace-offset               -5   -8    0    0    0
  glish-brace-imaginary-offset      0    0    4    0    0

Turning on Glish mode runs the normal hook `glish-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map glish-mode-map)
  (setq major-mode 'glish-mode
	mode-name "Glish"
	local-abbrev-table glish-mode-abbrev-table)
  (set-syntax-table glish-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'glish-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(^\\|\\s-\\);?}?#+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'glish-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Tell font-lock.el how to handle Glish.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((glish-font-lock-keywords
			      glish-font-lock-keywords-1
			      glish-font-lock-keywords-2)
			      nil nil ((?\_ . "w"))))
  (run-hooks 'glish-mode-hook))

;; This is used by indent-for-comment to decide how much to indent a
;; comment in Glish code based on its context.
(defun glish-comment-indent ()
  (if (and (bolp) (not (eolp)))
      0					; Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (if (bolp)			; Else indent at comment column
	       0			; except leave at least one space if
	     (1+ (current-column)))	; not at beginning of line.
	   comment-column))))

(defun electric-glish-terminator (arg)
  "Insert character and adjust indentation.
If at end-of-line, and not in a comment or a quote, correct the's indentation."
  (interactive "P")
  (let ((insertpos (point)))
    (and (not arg)			; decide whether to indent
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   ;; Eliminate comments quickly.
	   (and (not (re-search-forward comment-start-skip insertpos t))
		(let ((pps (parse-partial-sexp
			    (glish-beginning-of-function) insertpos)))
		  (not (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))
	 (progn				; must insert, indent, delete
	   (insert-char last-command-char 1)
	   (glish-indent-line)
	   (delete-char -1))))
  (self-insert-command (prefix-numeric-value arg)))


(defun glish-indent-command (&optional arg)
  "Indent current line as Glish code, or optionally, insert a tab character.

With an argument, indent the current line, regardless of other options.

If `glish-tab-always-indent' is nil and point is not in the indentation
area at the beginning of the line, simply insert a tab.

Otherwise, indent the current line.  If point was within the indentation
area it is moved to the end of the indentation area.  If the line was
already indented properly and point was not within the indentation area,
and if `glish-tab-to-comment' is non-nil (the default), then do the first
possible action from the following list:

  1) delete an empty comment
  2) move forward to start of comment, indenting if necessary
  3) move forward to end of line
  4) create an empty comment
  5) move backward to start of comment, indenting if necessary."
  (interactive "P")
  (if arg				; If arg, just indent this line
      (glish-indent-line "\f")
    (if (and (not glish-tab-always-indent) (> (current-column)
					      (current-indentation)))
	(insert-tab)
      (let (bof lsexp delta (oldpnt (point)))
	(beginning-of-line)
	(setq lsexp (point)
	      bof (glish-beginning-of-function))
	(goto-char oldpnt)
	(setq delta (glish-indent-line "\f\\|;?#" bof))
	(and glish-tab-to-comment
	     (= oldpnt (point))		; done if point moved
	     (if (listp delta)		; if line starts in a quoted string
		 (setq lsexp (or (nth 2 delta) bof))
	       (= delta 0))		; done if indenting occurred
	     (let (eol state)
	       (end-of-line)
	       (setq eol (point))
;	       (or (= (char-after bof) ?=)
	       (setq state (parse-partial-sexp lsexp eol))
	       (if (nth 3 state)
		   (and (= oldpnt eol) ; already at eol in a string
			(message "In a string which starts with a %c."
				 (nth 3 state)))
		 (if (not (nth 4 state))
		     (and (= oldpnt eol) ; no comment, create one?
			  (indent-for-comment))
		   (beginning-of-line)
		   (if (re-search-forward comment-start-skip eol 'move)
		       (if (eolp)
			   (progn	; kill existing comment
			     (goto-char (match-beginning 0))
			     (skip-chars-backward " \t")
			     (kill-region (point) eol))
			 (if (or (< oldpnt (point)) (= oldpnt eol))
			     (indent-for-comment) ; indent existing comment
			   (end-of-line)))
		     (if (/= oldpnt eol)
			 (end-of-line)
		       (message "Use backslash to quote # characters.")
		       (ding t)))))))))))

(defun glish-indent-line (&optional nochange parse-start)
  "Indent current line as Glish code.
Return the amount the indentation
changed by, or (parse-state) if line starts in a quoted string."
  (let ((case-fold-search nil)
	(pos (- (point-max) (point)))
	(bof (or parse-start (save-excursion (glish-beginning-of-function))))
	beg indent shift-amt)
    (beginning-of-line)
    (setq beg (point))
    (setq shift-amt
	  (cond ((= (char-after bof) ?=) 0)
		((listp (setq indent (calculate-glish-indent bof))) indent)
		((looking-at (or nochange glish-nochange)) 0)
		(t
		 (skip-chars-forward " \t\f")
;; JAU: Should try to handle braceless conditional (if/else) blocks
;; here, even though they're against the AIPS++ Glish coding standards.
		 (cond ((= (following-char) ?})
			(setq indent (- indent glish-indent-level)))
		       ((= (following-char) ?{)
			(setq indent (+ indent glish-brace-offset))))
		 (- indent (current-column)))))
    (skip-chars-forward " \t\f")
    (if (and (numberp shift-amt) (/= 0 shift-amt))
	(progn (delete-region beg (point))
	       (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt))

(defun calculate-glish-indent (&optional parse-start)
  "Return appropriate indentation for current line as Glish code.
In usual case returns an integer: the column to indent to.
Returns (parse-state) if line starts inside a string."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state containing-sexp)
      (if parse-start			;used to avoid searching
	  (goto-char parse-start)
	(glish-beginning-of-function))
      (while (< (point) indent-point)	;repeat until right sexp
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
; state = (depth_in_parens innermost_containing_list last_complete_sexp
;          string_terminator_or_nil inside_commentp following_quotep
;          minimum_paren-depth_this_scan)
; Parsing stops if depth in parentheses becomes equal to third arg.
	(setq containing-sexp (nth 1 state)))
      (cond ((nth 3 state) state)	; In a quoted string?
	    ((null containing-sexp)	; Line is at top level.
	     (skip-chars-forward " \t\f")
	     (if (= (following-char) ?{)
		 0   ; move to beginning of line if it starts a function body
	       ;; indent a little if this is a continuation line
	       (glish-backward-to-noncomment)
	       (if (or (bobp)
		       (memq (preceding-char) '(?\; ?\})))
		   0 glish-continued-statement-offset)))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (glish-backward-to-noncomment)
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) '(?\; ?\} ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  glish-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (glish-backward-to-start-of-continued-exp containing-sexp)
		   (+ glish-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (looking-at "[ \t]*{"))
			  glish-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position at last unclosed open.
	       (goto-char containing-sexp)
	       (or
		 ;; If open paren is in col 0, close brace is special
		 (and (bolp)
		      (save-excursion (goto-char indent-point)
				      (looking-at "[ \t]*}"))
		      glish-indent-level)
		 ;; Is line first statement after an open-brace?
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   ;; Skip over comments.
		   (while (progn
			    (skip-chars-forward " \t\f\n")
			    (cond ((looking-at ";?#")
				   (forward-line 1) t))))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(current-column)))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open paren in column zero, don't let statement
		 ;; start there too.  If glish-indent-level is zero,
		 ;; use glish-brace-offset + glish-continued-statement-offset
		 ;; For open-braces not the first thing in a line,
		 ;; add in glish-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop glish-indent-level))
			(+ glish-brace-offset glish-continued-statement-offset)
		      glish-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the glish-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 glish-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun glish-backward-to-noncomment ()
  "Move point backward to after the first non-white-space, skipping comments."
  (interactive)
  (let (opoint stop)
    (while (not stop)
      (setq opoint (point))
      (beginning-of-line)
      (if (re-search-forward comment-start-skip opoint 'move 1)
	  (progn (goto-char (match-end 1))
		 (skip-chars-forward ";")))
      (skip-chars-backward " \t\f")
      (setq stop (or (bobp)
		     (not (bolp))
		     (forward-char -1))))))

(defun glish-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t\f"))

;; note: this may be slower than the c-mode version, but I can understand it.
(defun indent-glish-exp ()
  "Indent each line of the Glish grouping following point."
  (interactive)
  (let* ((case-fold-search nil)
	 (oldpnt (point-marker))
	 (bof-mark (save-excursion
		     (end-of-line 2)
		     (glish-beginning-of-function)
		     (point-marker)))
	 eol last-mark lsexp-mark delta)
;    (if (= (char-after (marker-position bof-mark)) ?=)
;	(message "Can't indent a format statement")
    (message "Indenting Glish expression...")
    (save-excursion (end-of-line) (setq eol (point)))
    (save-excursion			; locate matching close paren
      (while (and (not (eobp)) (<= (point) eol))
	(parse-partial-sexp (point) (point-max) 0))
      (setq last-mark (point-marker)))
    (setq lsexp-mark bof-mark)
    (beginning-of-line)
    (while (< (point) (marker-position last-mark))
      (setq delta (glish-indent-line nil (marker-position bof-mark)))
      (if (numberp delta)		; unquoted start-of-line?
	  (progn
	    (if (eolp)
		(delete-horizontal-space))
	    (setq lsexp-mark (point-marker))))
      (end-of-line)
      (setq eol (point))
      (if (nth 4 (parse-partial-sexp (marker-position lsexp-mark) eol))
	  (progn			; line ends in a comment
	    (beginning-of-line)
	    (if (or (not (looking-at "\\s-*;?#"))
		    (listp delta)
		    (and (/= 0 delta)
			 (= (- (current-indentation) delta) comment-column)))
		(if (re-search-forward comment-start-skip eol t)
		    (indent-for-comment))))) ; indent existing comment
      (forward-line 1))
    (goto-char (marker-position oldpnt))
    (message "Indenting Glish expression...done")))

; JAU: Needs work for nested functions.
(defun glish-beginning-of-function (&optional arg)
  "Move backward to next beginning-of-function, or as far as possible.
With argument, repeat that many times; negative args move forward.
Returns new value of point in all cases."
  (interactive "p")
  (or arg
      (setq arg 1))
  (and (< arg 0)
       (forward-char 1))
  (and (/= arg 0)
       (re-search-backward
	"^\\s(\\|^\\s-*\\(func\\(tion\\)?\\|subseq\\(uence\\)?\\)\\b[^{]+{"
	nil 'move arg)
       (goto-char (1- (match-end 0))))
  (point))

;; note: this routine is adapted directly from emacs lisp.el,
;; end-of-defun; no bugs have been removed :-)
(defun glish-end-of-function (&optional arg)
  "Move forward to next end-of-function.
The end of a function is found by moving forward from the beginning of one.
With argument, repeat that many times; negative args move backward."
  (interactive "p")
  (or arg
      (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)))
	(while (progn
		 (if (and first
			  (progn
			    (forward-char 1)
			    (glish-beginning-of-function 1)
			    (not (bobp))))
		     nil
		   (or (bobp)
		       (forward-char -1))
		   (glish-beginning-of-function -1))
		 (setq first nil)
		 (forward-list 1)
		 (skip-chars-forward " \t")
		 (and (looking-at "[#\n]")
		      (forward-line 1))
		 (<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(glish-beginning-of-function 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (progn (glish-beginning-of-function 2) (not (bobp)))
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (and (looking-at "[#\n]")
		       (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defun mark-glish-function ()
  "Put mark at end of Glish function, point at beginning."
  (interactive)
  (push-mark (point))
  (glish-end-of-function)
  (push-mark (point))
  (glish-beginning-of-function)
  (backward-paragraph))