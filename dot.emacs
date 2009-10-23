;; -*- mode: emacs-lisp; -*-

;;; personal site lisp
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/site-lisp"))

;;; my own lisp
(add-to-list 'load-path
             (expand-file-name "~/elisp"))


;;; enable visual feedback on selections
(setq transient-mark-mode t)

;;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

(setq x-select-enable-clipboard t)
(scroll-bar-mode nil)

;;; default to unified diffs
(setq diff-switches "-u")

;;; always end a file with a newline
;; (setq require-final-newline nil)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(utf-translate-cjk-load-tables)

;;; expand tab to space
(setq-default indent-tabs-mode nil)

;;; global tab-width 2
(setq default-tab-width 4)

;;; tab stops
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30) )

;;; show line and column number
(column-number-mode t)
(line-number-mode t)

;;; kill whole line if at column 0
(setq-default kill-whole-line t)

;;; highlight matched parenthesises
(show-paren-mode t)

;;; syntax on
(global-font-lock-mode t)

;;; highlight current line
;; (global-hl-line-mode t)

;;; line-by-line scroll
(setq scroll-step 1)

;;; disable menu bar and tool bar
(menu-bar-mode 0)
(tool-bar-mode 0)

(iswitchb-mode t)

(ffap-bindings)

;;; brief confirm
(fset 'yes-or-no-p 'y-or-n-p)

;;; diable backup
(setq make-backup-files nil)

;;; default major mode
(setq default-major-mode 'text-mode)

;;; default coding system
(prefer-coding-system 'utf-8)

;;; start a server if none
(let ((ret (call-process "nc"
                         nil nil nil
                         "-U"
                         (format "/tmp/emacs%d/server" (user-uid))
                         "-w"
                         "1")) )
  (if (and (integerp ret) (= ret 0))
      (message "%s" "server already started")
    (message "%s" "server not started, start one")
    (server-start) ))


;;; completion-auto-help off
;(setq completion-auto-help nil)

;;; set major mode for special files
;; (setq auto-mode-alist
;;    (cons '("Make\\." . makefile-mode) auto-mode-alist))
;; (setq auto-mode-alist
;;    (cons '("\\.cppf" . c++-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.jsp" . html-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.jspf" . html-mode) auto-mode-alist))

;;; key binding
;; (define-key global-map [f7] 'previous-error)
;; (define-key global-map [f8] 'next-error)
(global-set-key "\M-p" 'previous-error)
(global-set-key "\M-n" 'next-error)

(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cm" 'man)

(global-set-key "\C-xra" 'append-to-register)
(global-set-key "\C-xrp" 'prepend-to-register)
(global-set-key "\C-xrv" 'view-register)

;; history search in shell buffer
(global-set-key "\C-c\M-p" 'comint-previous-matching-input)
(global-set-key "\C-c\M-n" 'comint-next-matching-input)

;; find tag in other window
(global-set-key "\C-c\M-." 'find-tag-other-window)

(defun my-greptag (symbol)
  "try to run `tags-apropos' with `current-word'"
  (interactive (list
                (read-string
                 (concat 
                  "Tags apropos (regex)"
                  (if (current-word nil t)
                    (concat " (default " (current-word nil t) ")")
                    nil)
                  ": ") )))
  (let ((tagname symbol))
    (if (string= tagname "")
        (setq tagname (current-word nil t) ))
    (unless tagname
        (error "tag symbol not specified") )
    (message (concat "--search tag '" tagname))
    (tags-apropos tagname) ))
(global-set-key "\M-]" 'my-greptag)

(defun my-abbrev-expand (arg)
  "Expand previous word, a special case of `dabbrev-expand'.

To expand word, this function set `dabbrev-abbrev-char-regexp' to \"\\\\sw\"
and invoke `dabbrev-expand', then restore `dabbrev-abbrev-char-regexp'."
  (interactive "*P")
  (let ((old-reg (if (boundp 'dabbrev-abbrev-char-regexp)
                     dabbrev-abbrev-char-regexp
                   nil)))
    (setq dabbrev-abbrev-char-regexp "\\sw")
    (unwind-protect
        (dabbrev-expand arg)
      (setq dabbrev-abbrev-char-regexp
            (if old-reg
                old-reg
              nil )))))
(global-set-key "\M-[" 'my-abbrev-expand)

(defun my-zap-until-char (arg char)
  "Kill up to ARG'th (exclude last one) occurrences of CHAR.
Variant of `zap-to-char'."
  (interactive "p\ncZap until char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) 
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 ;; exclude last CHAR
                 (if (>= arg 0) (backward-char) (forward-char))
                 (point))))
(global-set-key "\C-\M-z" 'my-zap-until-char)

;;; i used to split a frame to two window vertically, i need a quick restore
;;; of another window
(defun my-restore-other-window ()
  "switch buffer in other window and return to current buffer"
  (interactive)
  (progn
    (switch-to-buffer-other-window nil)
    (other-window 1) ) )
(global-set-key "\C-co" 'my-restore-other-window)

(defun my-put-buffer-file-name (reg)
  "put buffer file name to a register"
  (interactive "cPut filename to register: ")
  (set-register reg buffer-file-name) )
(global-set-key "\C-cf" 'my-put-buffer-file-name)

(defun my-file-info ()
  "show info of file being editing"
  (interactive)
  (message "%s" buffer-file-name) )
(global-set-key "\C-cg" 'my-file-info)



;;; setting when emacs in X window
(if (equal t (framep (selected-frame)))
    (message "gui not started")
  (global-set-key [mouse-1] nil)
  (global-set-key [down-mouse-1] nil)
  (global-set-key [double-mouse-1] nil)
  (global-set-key [mouse-2] nil)
  (global-set-key [down-mouse-2] nil)
  (set-foreground-color "white")
  (set-background-color "black")
  (setq default-frame-alist
        '(
          (top . 0)
          (left . 0)
          (height . 50)
          (width . 120)
          ))
  (require 'my-font)
  (my-set-font)
  )

(require 'compile)

;;; use my c style
(require 'my-c-style)
(add-hook 'c-mode-common-hook
          '(lambda()
             (my-set-c-style)
             (my-make-newline-indent)
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [?\r] 'newline-and-indent)
             (setq python-indent-list '(4 . "") )
             (setq tab-width 4) ) )

;; set tab-width to 2 for java-mode
(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 2) ) )

;;; sh-mode config:
;;;   set tab-width to 2
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2)))

;;; syntax check for php file upon save buffer
;;; TODO: bind to compilation mode to locate error quickly
(defun my-php-syntax-check (&optional silent)
  "run php syntax check on current buffer"
  (interactive)
  (let ((isphp (equal "php-mode" (prin1-to-string major-mode))))
    (if (not isphp)
        (unless silent
          (message "current buffer is not in php-mode"))
      (let ((outbuffer "*php-syntax-check*")
            (filename buffer-file-name)
            (ret (call-process "php"
                               nil nil nil
                               "-l"
                               buffer-file-name)))
        (if (and (integerp ret) (= ret 0))
            (unless silent
              (message "No syntax error in %s" filename))
          (if (buffer-live-p (get-buffer outbuffer))
              (kill-buffer outbuffer))
          (get-buffer-create outbuffer)
          (set-buffer outbuffer)
          (insert "-*- mode: compilation; default-directory: \"" default-directory "\" -*-\n")
          (insert "Syntax checking started at " (current-time-string) "\n\n")
          (insert "php -l " filename "\n")
          (call-process "php"
                        nil (current-buffer) nil
                        "-l"
                        filename)
          (compilation-mode)
          (pop-to-buffer (current-buffer))
          )))))

(require 'cl)
(pushnew '(php "^PHP Parse error:.*in +\\(.+\\.php\\) +on +line +\\([0-9]+\\) *$" 1 2)
         compilation-error-regexp-alist-alist)
(pushnew 'php compilation-error-regexp-alist)

(add-hook 'after-save-hook
          '(lambda ()
             (my-php-syntax-check t)
             ))

;;; php-mode config:
;;;   indent with tab, tab-width set to 4
;;;   use c++ style '//' line comment
(add-hook 'php-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode t)
             (setq comment-start "// ")
             (setq comment-end "")
             (setq php-completion-file 
                   (expand-file-name "~/doc/php_manual/php_func_file"))
             (local-set-key "\C-ccp" 'php-complete-function)
             ;;; need to line-up
             (c-set-offset 'arglist-cont-nonempty 'my-php-lineup-argcon)
             (c-set-offset 'defun-close 'my-php-defun-close)
             ))


;;; enable ansi color in shell mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook
          '(lambda ()
             ;; enable color prompt
             (ansi-color-for-comint-mode-on)
             (setq tab-width 8)
             (setq shell-pushd-regexp "pushd\\|pd") ))

;;; key settings for `hs-minor-mode'
(defun my-set-hs-keys ()
  (define-key hs-minor-mode-map [(control o)(h)] 'hs-hide-block)
  (define-key hs-minor-mode-map [(control o)(s)] 'hs-show-block)
  (define-key hs-minor-mode-map [(control o)(l)] 'hs-hide-level)
  (define-key hs-minor-mode-map [(control o)(t)] 'hs-toggle-hiding)
  (define-key hs-minor-mode-map [(control o)(control h)] 'hs-hide-all)
  (define-key hs-minor-mode-map [(control o)(control s)] 'hs-show-all) )

(add-hook 'hs-minor-mode-hook 'my-set-hs-keys)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)


;; sdcv
(require 'sdcv-mode)
(global-set-key (kbd "C-c d") 'sdcv-search)

(require 'actionscript-mode)
(add-hook 'actionscript-mode-hook
          '(lambda()
             (setq standard-indent 2) ))
(setq auto-mode-alist
   (cons '("\\.as" . actionscript-mode) auto-mode-alist))

;; tail-file
(require 'tail)

;; nxhtml (horrible face in terminal)
;; (if (file-readable-p "/home/carleo/tools/nxhtml/autostart.el")
;;     (load "/home/carleo/tools/nxhtml/autostart.el") )


;;; todo: show `default-directory' in mode line
;; (defun my-mode-line-dirtrack ()
;;   (add-to-list 'mode-line-buffer-identification 
;;                '(:propertize (" " default-directory " ") face dired-directory)))
(defun my-mode-line-dirtrack ()
  "show `default-directory' at the end (before '--' padding) of `mode-line'"
  ;; (message "%s" (concat "-------- trackdirectory for mode: " mode-name))
  ;;; TODO: can not make only buffer local change, all buffer affected
  ;; (make-variable-buffer-local 'mode-line-format)
  (let ((preelt nil) (lastelt nil))
    (setq preelt (cdr mode-line-format))
    (if (and (cdr preelt) (cddr preelt))
        (setq lastelt (cdr preelt)))
    (while (cdr lastelt)
      (setq preelt (cdr preelt))
      (setq lastelt (cdr preelt)))
    ;;; insert `default-directory' if it is not inserted
    (when lastelt
;;       (message "%s" (type-of (car preelt)))
;;       (message "%s" (car preelt))
      ;;; why this function will be invoked twice by the mode hook?
      ;;; so we have to check if we have inserted it or not
      (unless (equal (car preelt) '("  " default-directory))
;;         (message "%s" "+++++++")
        (setcdr preelt (cons '("  " default-directory) (cdr preelt))))
      )
    ))

(add-hook 'php-mode-hook 'my-mode-line-dirtrack)
(add-hook 'html-mode-hook 'my-mode-line-dirtrack)


;;; insert modification tag ( -- by carleo at `date')
(defun my-insert-mytag()
  "insert a modification tag"
  (interactive)
  (princ
   (concat "-- changed by carleo at " (current-time-string) " --")
   (current-buffer)))
(global-set-key "\C-ci" 'my-insert-mytag)

(global-set-key "\C-cv" 'view-file)
(global-set-key "\C-c/" 'dabbrev-completion)
;; (global-set-key "\C-\M-/" 'dabbrev-completion)

;;; column highlight
(require 'col-highlight)
(defun my-toggle-col-highlight()
  "toggle highlight current column"
  (interactive)
  (column-highlight-mode)
  )
(global-set-key "\C-ccc" 'my-toggle-col-highlight)

    
;;; skeleton-pair-insert
(setq skeleton-pair t)
(add-hook 'c-mode-common-hook
          '(lambda()
             ;; rebind '{' cause auto-newline does't work
             ;;(local-set-key "{" 'skeleton-pair-insert-maybe)
             ;;(local-set-key "(" 'skeleton-pair-insert-maybe)
             (local-set-key "[" 'skeleton-pair-insert-maybe)
             (local-set-key "'" 'skeleton-pair-insert-maybe)
             (local-set-key "\"" 'skeleton-pair-insert-maybe)
             ))
(add-hook 'python-mode-hook
          '(lambda()
             ;; rebind '{' cause auto-newline does't work
             ;;(local-set-key "{" 'skeleton-pair-insert-maybe)
             ;;(local-set-key "(" 'skeleton-pair-insert-maybe)
             (local-set-key "[" 'skeleton-pair-insert-maybe)
             (local-set-key "'" 'skeleton-pair-insert-maybe)
             (local-set-key "\"" 'skeleton-pair-insert-maybe)
             ))
(add-hook 'html-mode-hook
          '(lambda()
             (setq tab-width 4)
;;              (local-set-key "{" 'skeleton-pair-insert-maybe)
;;              (local-set-key "(" 'skeleton-pair-insert-maybe)
             (local-set-key "[" 'skeleton-pair-insert-maybe)
             (local-set-key "'" 'skeleton-pair-insert-maybe)
             (local-set-key "\"" 'skeleton-pair-insert-maybe)
;;              (local-set-key "<" 'self-insert-command)
             ))



(require 'javascript-mode)
(add-to-list 'auto-mode-alist '("\\.js" . javascript-mode))
(add-hook 'javascript-mode-hook
          '(lambda()
             (setq javascript-indent-level 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             ))

(require 'hide-region)
;; (global-set-key [(control o)(r)(h)] 'hide-region-hide)
;; (global-set-key [(control o)(r)(s)] 'hide-region-show)

(defun my-kill-line()
  "kill whole line if there is only spaces before point"
  (interactive)
  (unless (bolp)
    (let ((need-move nil)
          (end (point))
          (start (save-excursion
                   (beginning-of-line)
                   (point)))
          )
      (unless (eq nil
              (string-match "^[ \t]+$"
                            (buffer-substring start end)))
          (beginning-of-line)
          )
      ))
  (kill-line))
(global-set-key "\C-k" 'my-kill-line)

(global-set-key [(control tab)] 'hippie-expand)

;;; gnuserv --- disabled, as emacsserver/emacsclient can do this
;; (require 'gnuserv-compat)
;; (gnuserv-start)
;; (autoload 'gnuserv-start "gnuserv-compat"
;;  "Allow this Emacs process to be a server for client processes."
;;  t)

;;; Eim
(add-to-list 'load-path (expand-file-name "~/tools/eim-2.4"))
(autoload 'eim-use-package "eim" "Another emacs input method")
(setq eim-use-tooltip nil)
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")
;; 用 ; 暂时输入英文
;; (require 'eim-extra)
;; (global-set-key ";" 'eim-insert-ascii)


(unless t

(load-file "/home/carleo/tools/cedet/common/cedet.el")
(setq-default semanticdb-default-save-directory
              (expand-file-name "~/.semanticdb"))
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
(require 'semantic-ia)
(require 'semantic-gcc)
(global-semanticdb-minor-mode 1)
(global-semantic-tag-folding-mode 1)
;; (global-semantic-highlight-edits-mode 1)

;; (setq qt4-base-dir "/usr/include")
;; ;; (semantic-add-system-include qt4-base-dir 'c++-mode)
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

;; (semantic-add-system-include (concat qt4-base-dir "/Qt") 'c++-mode)
;; (semantic-add-system-include (concat qt4-base-dir "/QtCore") 'c++-mode)
;; (semantic-add-system-include (concat qt4-base-dir "/QtGui") 'c++-mode)
;; (semantic-add-system-include (concat qt4-base-dir "/QtUiTools") 'c++-mode)
;; (add-to-list 'auto-mode-alist (cons (concat qt4-base-dir "/Qt") 'c++-mode))
;; (add-to-list 'auto-mode-alist (cons (concat qt4-base-dir "/QtCore") 'c++-mode))
;; (add-to-list 'auto-mode-alist (cons (concat qt4-base-dir "/QtGui") 'c++-mode))
;; (add-to-list 'auto-mode-alist (cons (concat qt4-base-dir "/QtUiTools") 'c++-mode))

(autoload 'senator-try-expand-semantic "senator")
(setq hippie-expand-try-functions-list
      '(
        senator-try-expand-semantic
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-list-all-buffers
        try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        )
)
) ;;; unless
