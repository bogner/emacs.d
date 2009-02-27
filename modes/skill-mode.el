;--- mode for SKILL ---

; created    07/02/07 by M.Masar
; modified   2009/2/27 Justin Bogner

; load this from your ~/.emacs file
; Skill-mode, associated with the .il .ocn and .cdf files
; To use the skill mode for another file, use "M-x skill-mode"

(defvar skill-mode-hook nil)

(defvar skill-mode-map
  (let ((skill-mode-map (make-keymap)))
    (define-key skill-mode-map "\C-j" 'newline-and-indent)
    skill-mode-map)
  "Keymap for SKILL major mode")

;binding with file endings
(add-to-list 'auto-mode-alist '("\\.il\\'" . skill-mode))
(add-to-list 'auto-mode-alist '("\\.ocn\\'" . skill-mode))
(add-to-list 'auto-mode-alist '("\\.cdf\\'" . skill-mode))

(defconst skill-font-lock-keywords-1
  (list
   (cons
    (concat
     "\\<"
     (regexp-opt
      '("add1" "and" "apply" "argc" "argv" "arrayp" "arrayref" "atof" "atoi"
        "atom" "band" "begin" "bitfield1" "bitfield" "blankstrp" "bnand"
        "bnor" "bnot" "booleanp" "bor" "boundp" "bxnor" "bxor" "case" "caseq"
        "ceiling" "concat" "cond" "cons" "constar" "copy" "copyDefstructDeep"
        "declare" "declareLambda" "declareNLambda" "declareSQNLambda" "define"
        "defmacro" "defprop" "defstruct" "defstructp" "defun"
        "defUserInitProc" "defvar" "do" "dtpr" "eq" "equal" "eqv" "err"
        "error" "errset" "errsetstring" "eval" "evalstring" "evenp" "exit"
        "expandMacro" "fboundp" "fixp" "floatp" "for" "forall" "foreach"
        "funcall" "funobj" "gc" "gensym" "geqp" "get" "get_pname" "get_string"
        "getd" "getFnWriteProtect" "getFunType" "getq" "getqq"
        "getVarWriteProtect" "greaterp" "if" "then" "else" "inportp"
        "inScheme" "inSkill" "integerp" "intToChar" "isCallable" "isInfinity"
        "isNaN" "lambda" "leftshift" "leqp" "lessp" "let" "letrec" "letseq"
        "listp" "listToVector" "load" "loadi" "loadstring" "makeTable"
        "makeVector" "map" "mapc" "mapcan" "mapcar" "maplist" "minus" "minusp"
        "mod" "modulo" "mprocedure" "needNCells" "negativep" "neq" "nequal"
        "newline" "nindex" "nlambda" "not" "nprocedure" "null" "numberp"
        "oddp" "onep" "openportp" "or" "otherp" "outportp" "pairp" "plist"
        "plus" "plusp" "portp" "postdecrement" "postincrement" "predecrement"
        "preincrement" "procedure" "procedurep" "prog" "prog1" "prog2" "progn"
        "putd" "putprop" "putpropq" "putpropqq" "quote" "quotient" "realp"
        "regExitAfter" "regExitBefore" "remainder" "remExitProc" "remprop"
        "return" "rexCompile" "rexExecute" "rexMagic" "rexMatchAssocList"
        "rexMatchList" "rexMatchp" "rexReplace" "rexSubstitute" "rightshift"
        "round" "set" "setarray" "setFnWriteProtect" "setplist" "setq"
        "setqbitfield1" "setqbitfield" "setVarWriteProtect" "stringp" "sub1"
        "sxtd" "symbolp" "symeval" "symstrp" "system" "tablep" "tailp" "times"
        "truncate" "type" "typep" "unalias" "unless" "vector" "vectorp" "warn"
        "when" "which" "while" "xdifference" "xplus" "xquotient" "xtimes"
        "zerop" "zxtd") t)
     "\\>") 'font-lock-keyword-face)
   (cons (concat "\\<" (regexp-opt '("nil" "t") t) "\\>")
         'font-lock-constant-face)
   )
  "Basic SKILL functions - arithmetic, logic, types, and control flow")

(defconst skill-font-lock-keywords-2
  (list
   (cons
    (concat
     "\\<"
     (regexp-opt
      '("append" "append1" "bcdp" "buildString" "caar" "caaar" "caadr" "cadr"
        "caddr" "cdar" "cddr" "car" "cdr" "cdsGetInstPath" "changeWorkingDir"
        "charToInt" "clearExitProcs" "close" "compress" "defMathConstants"
        "deleteDir" "deleteFile" "difference" "display" "drain" "exists"
        "fileLength" "fileSeek" "fileTell" "fileTimeModified" "fprintf"
        "fscanf" "scanf" "sscanf" "get_filename" "getc" "getchar"
        "getDirFiles" "gets" "getWorkingDir" "infile" "instring" "isDir"
        "isExecutable" "isFile" "isFileEncrypted" "isFileName" "isLargeFile"
        "isLink" "isMacro" "isReadable" "isWritable" "last" "lconc" "length"
        "lineread" "linereadstring" "list" "makeTempFileName" "member" "memq"
        "memv" "nconc" "ncons" "nth" "nthcdr" "nthelem" "numOpenFiles"
        "outfile" "parseString" "pprint" "print" "printf" "printlev" "println"
        "read" "readstring" "readTable" "renameFile" "reverse" "rplaca"
        "rplacd" "setcar" "setcdr" "setof" "simplifyFilename" "sort" "sortcar"
        "sprintf" "strcat" "strcmp" "stringToFunction" "stringToSymbol"
        "stringToTime" "strlen" "strncat" "strncmp" "subst" "substring"
        "symbolToString" "tconc" "upperCase" "vectorToList" "write"
        "writeTable" "xcons" "xCoord" "yCoord") t)
     "\\>") 'font-lock-keyword-face))
  "Additional SKILL functions - mostly string, list and file operations")

(defconst skill-font-lock-keywords-3
  (list
   (cons
    (concat
     "\\<"
     (regexp-opt
      '("abs" "acos" "addDefstructClass" "alias" "alphalessp" "alphaNumCmp"
        "asin" "assoc" "assq" "assv" "atan" "atan2" "compareTime" "cos"
        "cputime" "createDir" "csh" "ed" "edi" "edit" "edl" "envobj" "exp"
        "expt" "fix" "float" "floor" "getCurrentTime" "getInstallPath"
        "getLogin" "getPrompts" "getTempDir" "getShellEnvVar" "getSkillPath"
        "getSkillVersion" "getVersion" "getWarn" "go" "help" "importSkillVar"
        "index" "log" "log10" "lowerCase" "max" "measureTime" "min"
        "prependInstallPath" "random" "range" "remd" "remdq" "remove" "remq"
        "rindex" "schemeTopLevelEnv" "setPrompts" "setShellEnvVar"
        "setSkillPath" "sh" "shell" "sin" "sqrt" "srandom" "sstatus" "status"
        "tableToList" "tan" "theEnvironment" "timeToString" "timeToTm"
        "tmToTime" "vi" "vii" "vil") t)
     "\\>")  'font-lock-keyword-face))
  "Other SKILL functions")

(defvar skill-font-lock-keywords
  (append skill-font-lock-keywords-1
          skill-font-lock-keywords-2
          skill-font-lock-keywords-3)
  "Default highlighting expressions for SKILL mode.")

(defvar skill-mode-syntax-table
  (let ((skill-mode-syntax-table (make-syntax-table)))

    ; block comments
    (modify-syntax-entry ?/ ". 14" skill-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" skill-mode-syntax-table)

    ; line comments
    (modify-syntax-entry ?\; "< b" skill-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" skill-mode-syntax-table)

   skill-mode-syntax-table)
  "Syntax table for skill-mode")

(defun skill-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map skill-mode-map)
  (set-syntax-table skill-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(skill-font-lock-keywords))
  ;; Register our indentation function
  (setq major-mode 'skill-mode)
  (setq mode-name "SKILL")
  (run-hooks 'skill-mode-hook))

(provide 'skill-mode)
