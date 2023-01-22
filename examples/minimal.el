;; A mode for editing Minimal files

;; customizable variables
(defvar mal-default-indent 2
  "Default indentation in Minimal")
(defvar mal-case-indent 4
  "Indentation for case blocks in Minimal")
(defvar mal-program "minimal"
  "The interpret to run as a subprocess")
(defvar mal-lookback-limit 2000
  "*How far to look back for comments in Minimal mode.")

;; code
(if (or (not (fboundp 'indent-line-to))
	(not (fboundp 'buffer-substring-no-properties)))
    (require 'caml-compat))

(defvar mal-mode-syntax-table nil
  "Syntax table in use in Mal mode buffers.")
(if mal-mode-syntax-table
    ()
  (setq mal-mode-syntax-table (make-syntax-table))
  ; backslash is an escape sequence
  (modify-syntax-entry ?\\ "\\" mal-mode-syntax-table)
  (modify-syntax-entry ?\( "()1" mal-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" mal-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" mal-mode-syntax-table)
  ; quote and underscore are part of words
  (modify-syntax-entry ?' "w" mal-mode-syntax-table)
  (modify-syntax-entry ?_ "w" mal-mode-syntax-table)
  ; ISO-latin accented letters and EUC kanjis are part of words
  (let ((i 160))
    (while (< i 256)
      (modify-syntax-entry i "w" mal-mode-syntax-table)
      (setq i (1+ i)))))

(defvar caml-shell-active nil
  "*Non nil when a subshell is running.")

(defvar mal-mode-map nil
  "Keymap used in Mal mode.")
(if mal-mode-map
    ()
  (setq mal-mode-map (make-sparse-keymap))
  (define-key mal-mode-map "\t" 'mal-indent-line)
  (define-key mal-mode-map "\177" 'backward-delete-char-untabify)
  (define-key mal-mode-map "\M-\C-h" 'mal-mark-phrase)
  (define-key mal-mode-map "\M-\C-q" 'mal-indent-phrase)
  (define-key mal-mode-map "\M-\C-s" 'mal-start-process)
  (define-key mal-mode-map "\M-\C-x" 'mal-eval-phrase)
  (define-key mal-mode-map "\C-c\C-c" 'compile)
  (define-key mal-mode-map "\C-c\C-s" 'mal-show-subshell)
  (let ((map (make-sparse-keymap "Minimal")))
    (define-key mal-mode-map [menu-bar] (make-sparse-keymap))
    (define-key mal-mode-map [menu-bar minimal] (cons "Minimal" map))
    (define-key map [start-process] '("Start subshell..." . mal-start-process))
    (define-key map [compile] '("Compile..." . compile))
;    (define-key map [switch-view] '("Switch view" . caml-find-alternate-file))
    (define-key map [separator-format] '("--"))
;    (define-key map [forms] (cons "Forms" forms))
    (define-key map [show-subshell] '("Show subshell" . mal-show-subshell))
    (put 'mal-show-subshell 'menu-enable 'caml-shell-active)
    (define-key map [eval-phrase] '("Eval phrase" . mal-eval-phrase))
    (put 'mal-eval-phrase 'menu-enable 'caml-shell-active)
    (define-key map [indent-phrase] '("Indent phrase" . mal-indent-phrase))))

(defun minimal-mode ()
  "Major mode for editing Minimal code

\\{mal-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mal-mode-map)
  (setq major-mode 'minimal-mode)
  (setq mode-name "Minimal")
  (set-syntax-table mal-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mal-indent-line)
  (setq comment-indent-function 'mal-indent-comment)
  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search nil)
  ;; Font lock support
  (cond
   ((boundp 'global-font-lock-mode)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
	  '(minimal-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
   (t
    (make-local-variable 'font-lock-keywords)
    (setq font-lock-keywords minimal-font-lock-keywords)))
  ;; Imenu support
  (require 'imenu)
  (setq imenu-prev-index-position-function 'mal-prev-index-position-function)
  (setq imenu-extract-index-name-function 'mal-extract-index-name-function)
  (if (< (buffer-size) 10000) (imenu-add-to-menubar "Defs"))
  (run-hooks 'minimal-mode-hook))

(defconst mal-imenu-search-regexp
  (concat "\\<end\\>\\|^[ \t]*\\(and\\|class\\|fun\\|type\\|va[lr]\\)"
	  "[ \t]+\\(\\('[a-zA-Z0-9]+\\|([^)]+)\\)[ \t]+\\)?"
	  "\\([a-zA-Z][a-zA-Z0-9_']*\\)"))
(defun mal-prev-index-position-function ()
  (let (found pos data)
    (while (and (not found)
		(mal-search mal-imenu-search-regexp))
      (setq data (match-data))
      (cond
       ((looking-at "end")
	(setq pos (point))
	(mal-search-def mal-start-keywords)
	(if (looking-at "class") (goto-char pos)))
       (t
	(setq found t))))
    (set-match-data data)
    found))
(defun mal-extract-index-name-function ()
  (mal-match-string 4))

(defun mal-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
    (let* ((stcol (save-excursion
		    (re-search-backward "(\\*" nil t)
		    (1+ (current-column)))))
      (if arg stcol
	(delete-horizontal-space)
	(indent-to stcol))))

(defconst mal-all-keywords
  (concat "\\<\\(and\\|begin\\|c\\(ase\\|lass\\)\\|do\\(wnto\\)?"
	  "\\|e\\(lse\\|nd\\|x\\(ception\\|te\\(nds\\|rnal\\)\\)\\)"
	  "\\|f\\(n\\|or\\|un\\)\\|h\\(andle\\|ide\\)\\|if\\|meth"
	  "\\|o\\(bject\\|f\\|pen\\)"
	  "\\|t\\(hen\\|o\\|ype\\)\\|va[lr]\\|wh\\(en\\|ile\\)\\)\\>"
	  "\\|[][{}();=:]\\|<-")
  "All Minimal keywords")
(defconst mal-def-keywords
  (concat "\\<\\(class\\|ex\\(ception\\|ternal\\)\\|fun\\|h\\(andle\\|ide\\)"
	  "\\|meth\\|o\\(bject\\|pen\\)\\|type\\|va[lr]\\)\\>")
  "Keywords for defining something in Minimal")
(defconst mal-case-keywords
  "^[ \t]*|[^]]\\|\\<\\(case\\|handle\\)\\>"
  "Keywords dominating a case block in Minimal")
(defconst mal-dom-keywords
  (concat mal-def-keywords "\\|\\<\\(and\\|begin\\)\\>\\|=>")
  "Keywords dominating a definition in Minimal")
(defconst mal-bar-keywords
  (concat mal-case-keywords "\\|\\<type\\>")
  "Keywords domination a | in Minimal")
(defconst mal-start-keywords
  "\\<\\(begin\\|c\\(ase\\|lass\\)\\)\\>"
  "Keywords dominating end in Minimal")
(defconst mal-semi-keywords
  (concat mal-case-keywords "\\|\\<\\(begin\\|class\\)\\>\\|=>")
  "Keywords dominating ; in minimal")
(defconst mal-and-keywords
  "\\<\\(fun\\|type\\|va[lr]\\)\\>"
  "Keywords dominating an and keyword in Minimal")
(defconst mal-indent-keywords
  (concat mal-def-keywords "\\|" mal-case-keywords
	  "\\|\\<\\(if\\|else\\|f\\(n\\|or\\)\\|while\\|do\\(wnto\\)?"
	  "\\|and\\|begin\\|t\\(hen\\|o\\)\\)\\>\\|[[({]\\|=>")
  "Keywords indenting following text in Minimal")
(defconst mal-prefix-keywords
  (concat mal-indent-keywords "\\|\\<of\\>\\|[=;,]")
  "Keywords introducing an expression")

(defun mal-in-indentation ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))
  
(defun mal-indent-line ()
  "Indent current line as a Minimal statement."
  (interactive)
  (if (mal-in-indentation)
      (indent-line-to (save-excursion (mal-compute-indent)))
    (save-excursion
      (indent-line-to (save-excursion (mal-compute-indent))))))

(defun mal-in-literal-p ()
  "Returns non-nil if point is inside a Minimal literal."
  (let* ((start-literal "[\"']")
	 (char-literal "\\([^\\]\\|\\\\\\.\\|\\\\[0-9][0-9][0-9]\\)'")
	 (pos (point))
	 (eol (progn (end-of-line 1) (point)))
	 state in-str)
    (beginning-of-line 1)
    (while (and (not state)
		(re-search-forward start-literal eol t)
		(<= (point) pos))
      (cond
       ((= (preceding-char) ?\")
	(setq in-str t)
	(while (and in-str (not state)
		    (re-search-forward "\"\\|\\\\\"" eol t))
	  (if (> (point) pos) (setq state t))
	  (goto-char (match-beginning 0))
	  (if (= (following-char) ?\") (setq in-str nil))
	  (goto-char (match-end 0)))
	(if in-str (setq state t)))
       ((looking-at char-literal)
	(if (and (>= pos (match-beginning 0)) (< pos (match-end 0)))
	    (setq state t)
	  (goto-char (match-end 0))))))
    (goto-char pos)
    state))

(defun mal-in-comment-p ()
  (or
   (looking-at "(\\*") 
   (let* ((pos (point))
	  (level 0)
	  (pos-min (min (point-min) (- pos mal-lookback-limit))))
     (while (and (>= level 0) (> (point) pos-min)
		 (re-search-backward "\\*)\\|(\\*" pos-min t))
       (if (not (mal-in-literal-p))
	   (if (looking-at "\\*)")
	       (setq level (1+ level))
	     (setq level (1- level)))))
     (goto-char pos)
     (< level 0))))

(defconst mal-def-end-keywords
  (concat "\\<\\(begin\\|c\\(lass\\|ase\\)\\|end\\|fun\\|hide"
	  "\\|type\\|va[lr]\\)\\>\\|;"))
(defun mal-search-def-end ()
  "Search the end of the phrase starting at point"
  (let ((class (looking-at "\\<class\\>"))
	found
	pos
	(balance 0))
    (forward-char)
    (while (and (not found)
		(re-search-forward
		 (if (and (= balance 0) (not class))
		     mal-def-end-keywords
		   "\\<\\(begin\\|c\\(ase\\|lass\\)\\|end\\)\\>")
		 nil 'move))
      (setq pos (point))
      (goto-char (match-beginning 0))
      (cond
       ((or (mal-in-literal-p) (mal-in-comment-p)))
       ((or (looking-at "begin\\|case")
	    (and class (looking-at "class")))
	(goto-char pos)
	(setq balance (1+ balance)))
       ((looking-at "end")
	(cond
	 ((> balance 0)
	  (goto-char pos)
	  (setq balance (1- balance)))
	 (class
	  (goto-char pos)
	  (setq found t))
	 (t
	  (setq found t))))
       ((= balance 0)
	(setq found t))
       (t
	(goto-char pos))))
    (skip-chars-backward " \t\n\r")
    (while (and (= (preceding-char) ?\)) (= (char-after (- (point) 2)) ?*))
      (backward-char)
      (mal-skip-comment-backward)
      (skip-chars-backward " \t\n\r"))))

(defun mal-search-def-begin ()
  (let (found
	candidate
	(search-reg (concat ";\\|=>\\|\\<\\(begin\\|case\\)\\>\\|"
			   mal-def-keywords)))
    (while (and (not found)
		(mal-search-def search-reg))
      (cond
       ((looking-at "\;")
	(if (not candidate) (setq candidate (1+ (point)))))
       ((looking-at "=>")
	(mal-search-def "\\<\\(begin\\|case\\|fn\\)\\>")
	(setq candidate nil))
       ((looking-at "begin\\|case")
	(setq candidate nil))
       ((looking-at "class")
	(if (not candidate) (setq candidate (point)))
	(setq found t))
       (t
	(if (not candidate) (setq candidate (point))))))
    (if candidate (goto-char candidate))
    (skip-chars-forward " \t\n\r")
    (while (looking-at "\(\\*")
      (mal-skip-comment-forward)
      (skip-chars-forward " \t\n\r"))))

(defun mal-mark-phrase ()
  "Mark the current Minimal phrase"
  (interactive)
  (forward-word 1)
  (mal-search-def-begin)
  (let ((pos (point)))
    (mal-search-def-end)
    (push-mark)
    (goto-char pos)
    (cons pos (mark))))

(defun mal-indent-phrase ()
  "Indent the current Minimal phrase"
  (interactive)
  (let ((bounds (mal-mark-phrase)))
    (indent-region (car bounds) (cdr bounds) nil)))

(defun mal-eval-region (start end)
  "Send the current region to the inferior Minimal process."
  (interactive"r")
  (comint-send-region "*inferior-caml*" start end)
  (comint-send-string "*inferior-caml*" ";\n")
  (if (not (get-buffer-window "*inferior-caml*" t))
      (display-buffer "*inferior-caml*")))

(defun mal-eval-phrase ()
  "Send the current Minimal phrase to the inferior Minimal process."
  (interactive)
  (save-excursion
    (let ((bounds (mal-mark-phrase)))
    (mal-eval-region (car bounds) (cdr bounds)))))

(defun mal-start-process ()
  "Call run-caml to start a Minimal subprocess."
  (interactive)
  (require 'inf-caml)
  (run-caml mal-program))

(defun mal-show-subshell ()
  (interactive)
  (inferior-caml-show-subshell))

(defun mal-search (reg)
  (let (found data)
    (while (and (setq found (re-search-backward reg nil 'move))
		(setq data (match-data))
		(or (mal-in-literal-p) (mal-in-comment-p))))
    (set-match-data data)
    found))

(defconst mal-paren-alist
  '((?\] . "[[]") (?\) . "(") (?\} . "{")))

(defun mal-search-def (reg)
  (let (found
	(search-reg (concat reg "\\|\\<end\\>\\|[])}]")))
    (while (and (not found) (mal-search search-reg))
      (cond
       ((looking-at reg)
	(setq found t))
       ((looking-at "end")
	(mal-search-def mal-start-keywords))
       ((looking-at "[])}]")
	(mal-search-def (cdr (assoc (following-char) mal-paren-alist))))
       ))
    found))

(defun mal-search-expr (reg)
  (let (found
	(search-reg
	 (concat reg "\\|\\<\\(do\\|e\\(lse\\|nd\\)\\|then\\)\\>\\|[])};,]")))
    (while (and (not found) (mal-search search-reg))
      (cond
       ((looking-at reg) (setq found t))
       ((looking-at ";")
	(mal-search-def mal-semi-keywords)
	(setq found t))
       ((looking-at ",")
	(mal-search-expr "[[({]")
	(setq found t))
       ((looking-at "end")
	(mal-search-def mal-start-keywords))
       ((looking-at "[])}]")
	(mal-search-expr (cdr (assoc (following-char) mal-paren-alist))))
       ((looking-at "then")
	(mal-search-expr "\\<if\\>"))
       ((looking-at "else")
	(mal-search-expr "\\<then\\>")
	(mal-search-expr "\\<if\\>"))
       ((looking-at "do")
	(mal-search-expr "\\<\\(\\(down\\)?to\\|while\\)\\>")
	(if (looking-at "\\(down\\)?to") (mal-search-expr "for")))
       ((looking-at "\\(down\\)?to")
	(mal-search-expr "for"))))
    found))

(defun mal-skip-comment-forward ()
  (forward-char)
  (let (found)
    (while (and (not found) (re-search-forward "\\*)\\|(\\*" nil t))
      (if (not (mal-in-literal-p))
	  (cond
	   ((= (preceding-char) ?*)
	    (mal-skip-comment-forward))
	   ((= (preceding-char) ?\))
	    (setq found t)))))))

(defun mal-skip-comment-backward ()
  (let (found)
    (while (and (not found) (re-search-backward "\\*)\\|(\\*" nil t))
      (if (not (mal-in-literal-p))
	  (cond
	   ((= (following-char) ?*)
	    (mal-skip-comment-backward))
	   ((= (following-char) ?\()
	    (setq found t)))))))

(defun mal-keyword-indent ()
  (cond
   ((looking-at "=>")
    (mal-search-expr (concat "\\<fn\\>\\|" mal-case-keywords))
    (let ((indent (if (looking-at "fn") mal-default-indent mal-case-indent)))
      (back-to-indentation)
      (+ indent (current-column))))
   ((looking-at "type")
    (back-to-indentation)
    (+ mal-case-indent (current-column)))
   ((looking-at "and")
    (mal-search-def mal-and-keywords)
    (+ (if (looking-at "type") mal-case-indent mal-default-indent)
       (current-column)))
   ((looking-at "else")
    (back-to-indentation)
    (+ (if (looking-at "\\<if\\>") 0 mal-default-indent) (current-column)))
   ((looking-at "[[({]")
    (cond
     ((looking-at ".|?[ \t]*$")
      (back-to-indentation)
      (+ mal-default-indent (current-column)))
     ((looking-at "(")
      (1+ (current-column)))
     (t
      (+ 2 (current-column)))))
   (t
    (back-to-indentation)
    (+ mal-default-indent (current-column)))))

(defun mal-match-string (n)
  (buffer-substring-no-properties (match-beginning n) (match-end n)))

(defun mal-looking-at (reg)
  "Same as looking-at, but ignores leading spaces"
  (looking-at (concat "[ \t]*" reg)))

(defconst mal-skip-alist
  (list '("then"	"\\<if\\>"		nil nil)
	'("do"		"\\<\\(for\\|while\\)\\>" nil t)
	'("downto"	"\\<for\\>"		nil t)
	'("to"		"\\<for\\>"		nil t)
	'("handle"	"\\<begin\\>"		t   t)
	(list "and"	mal-and-keywords	t   t)
	(list "end"	mal-start-keywords 	t   t)
	(list ";"	mal-def-keywords 	t   t))
  "Each record contains the regexp for a keyword, the node to align it on,
wether to use mal-search-def, and wether to go back to indentation or not")
(defconst mal-skip-keywords
  "\\(\\<\\(and\\|do\\(wnto\\)?\\|end\\|handle\\|t\\(o\\|hen\\)\\)\\>\\|;\\)"
  "Keywords matched in mal-skip-alist")

(defun mal-compute-indent ()
  (beginning-of-line)
  (let ((pos (point)))
    (cond
     ((= pos (point-min)) 0)
     ((mal-looking-at mal-def-keywords)
      (mal-search-def mal-dom-keywords)
      (cond
       ((looking-at "\\<\\(begin\\|class\\)\\>")
	(back-to-indentation)
	(+ mal-default-indent (current-column)))
       ((looking-at "=>")
	(mal-search mal-case-keywords)
	(back-to-indentation)
	(+ mal-case-indent (current-column)))
       ((looking-at mal-dom-keywords)
	(back-to-indentation)
	(current-column))
       (t 0)))
     ((mal-looking-at "[ \t]*|\\([^]]\\|$\\)")
      (mal-search-def mal-bar-keywords)
      (cond
       ((looking-at "type")
	(back-to-indentation)
	(+ (- mal-case-indent mal-default-indent) (current-column)))
       (t
	(back-to-indentation)
	(current-column))))
     ((mal-looking-at "\\<else\\>")
      (mal-search-expr "\\<then\\>")
      (if (looking-at "then[ \t]*$") (mal-search-expr "\\<if\\>"))
      (let ((then-pos (current-column)))
        (back-to-indentation)
        (if (looking-at "else") (current-column)
          then-pos)))
     ((mal-looking-at mal-skip-keywords)
      (let ((info (assoc (mal-match-string 1) mal-skip-alist)))
	(if (nth 2 info) (mal-search-def (nth 1 info))
	  (mal-search-expr (nth 1 info)))
	(if (nth 3 info) (back-to-indentation))
	(current-column)))
     (t
      (mal-search mal-prefix-keywords)
      (looking-at mal-prefix-keywords)
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n\r")
      (while (and (< (point) pos) (looking-at "(\\*"))
	(mal-skip-comment-forward)
	(skip-chars-forward " \t\n\r"))
      (let ((prefixed (>= (point) pos)))
	(goto-char pos)
	(mal-search-expr mal-indent-keywords)
	(+ (if prefixed 0 mal-default-indent) (mal-keyword-indent)))
     ))))


(defconst minimal-font-lock-keywords
  (list
;inferior
    '("^[#-]" . font-lock-comment-face)
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-comment-face)
;character literals
   (cons (concat "'\\(\\\\\\([ntbr'\\]\\|[0-9][0-9][0-9]\\)\\|.\\)'"
		 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\"")
	 'font-lock-string-face)
;definition and operators
   (cons (concat
	  "\\<\\(use\\|require"
	  "\\|and\\|fun\\|hide\\|meth"
	  "\\|v\\(a[lr]\\|irtual\\)\\|object"
	  "\\|type\\|exception\\|mutable\\)\\>"
	  "\\|<[>= \t]\\|[ \t]>=?\\|!=\\|==\\|::\\|[@#]"
	  "\\|<[A-Za-z0-9]*>")
	 'font-lock-type-face)
;labels and assignments
   '("\\(\\<[A-Za-z][A-Za-z0-9_']*:\\)\\([^:=]\\|\\'\\|$\\)" 1
     font-lock-variable-name-face)
   (cons (concat ":?=>?\\|<-\\|->\\||\\|\\<\\(open\\|when\\)\\>")
	 'font-lock-variable-name-face)
;blocking
   (cons (concat
	  "\\<\\(begin\\|c\\(ase\\|lass\\)\\|end\\|of\\|safe"
	  "\\|handle\\|extends\\|reparent\\)\\>")
	 'font-lock-keyword-face) 
;control
   (cons (concat
	  "\\<\\(do\\(wnto\\)?\\|else\\|f\\(or\\|n\\)\\|if\\|t\\(hen\\|o\\)"
	  "\\|while\\)\\>")
	 'font-lock-reference-face)
;uppercase and or
   '("\\<\\([A-Z][A-Za-z0-9_']*\\|or\\)\\>\\|&"
     . font-lock-function-name-face)
   '("\\<raise\\>" . font-lock-comment-face)))
