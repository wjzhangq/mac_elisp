;;; from google-c-style.el

(defun my-c-lineup-to-previous-line (langelem)
  "Indents to the beginning of the first syntagm on the previous line.

Suitable for inclusion in `c-offsets-alist'.  Works with: Any syntactic symbol."
  (save-excursion (vector (c-langelem-col c-syntactic-element))))

;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun my-c-lineup-open-paren (langelem)
  "Indents to the beginning of the current C statement plus double `c-basic-offset'.

This implements title \"Function Declarations and Definitions\" of the Google
C++ Style Guide for the case where the previous line ends with an open
parenthese.

Suitable for inclusion in `c-offsets-alist'."
  (vector (+ (* 2 c-basic-offset)
             (elt (my-c-lineup-to-previous-line langelem) 0))))

;;  to do: php arglist-intro
;;  array init is taken as arglist-intro
;;     $a = array(
;;             'xx' => 'x',
;;                );
;;  don't indent++ for this statements, and closing ')' of `arglist-close'
;;  should indent to start-col
(defun my-php-lineup-argcon (langelem)
  "Indent + for function, while ++ for condition"
  (save-excursion
    ;;(vector (+ c-basic-offset (c-langelem-col c-syntactic-element)))))
    (let ((start-col (c-langelem-col c-syntactic-element))
          (start-pos (c-langelem-pos c-syntactic-element)))
      (goto-char start-pos)
      (if (or (equal "if" (current-word)) (equal "while" (current-word)))
          (vector (+ start-col (* 2 c-basic-offset)))
        (vector (+ start-col c-basic-offset))))
    ))

(defun my-php-defun-close (langelem)
  "Set proper indent for closing '}'.

when a function is defined at the very beginning of a file, default indent is
strange, for following php code:

<?php
function func($arg) {
	echo $arg;
  }

`c-show-syntatic-information' show that syntactic analysis is:
  ((defun-close 3))
and we get a strange indent 2, so we need to force it to 0 for such situation,
if word at start-col is not `function', `priviate', `public' or `protected'"
  (save-excursion
    (let ((start-col (c-langelem-col c-syntactic-element))
          (start-pos (c-langelem-pos c-syntactic-element)))
      (goto-char start-pos)
      (if (or (equal "function" (current-word)) (equal "priviate" (current-word))
              (equal "public" (current-word)) (equal "protected" (current-word))
              )
          (vector start-col)
        (vector 0)))
    ))

(defun my-snug-class-close (syntax pos)
  "Dynamically calculation brace hanginess for class-close.
For c-mode and c++-mode, class (include struct and c++ class) need end
with `;', so just insert newline before close brace, while in other mode,
insert newline both before and after close brace."
  (if (or (equal "c-mode" (prin1-to-string major-mode))
          (equal "c++-mode" (prin1-to-string major-mode)))
      '(before)
    '(before after)))

(defconst my-c-style
  `(;;"gnu"
    ;; show syntactic context to help me understand indentation calculation
    ;; (c-echo-syntactic-information-p t)
    (c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    ;; disable 'gnu' style minimum indention (default 1), which add a space
    ;; at column 0 when indent line contains only '}' and spaces, even if 
    ;; indentation is 0
    (c-label-minimum-indentation 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close . my-snug-class-close)
                               ;; (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro my-c-lineup-open-paren)
                        ;; indent++ for mutil-line condtion
                        ;;(arglist-cont-nonempty my-c-lineup-argcon)
                        ;; (arglist-cont-nonempty c-lineup-argcont)
                        (func-decl-cont . ++)
                        ;;(member-init-intro . ++)
                        (member-init-intro . +)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        ;; no indent for c++ access modifier
                        ;; (access-label . /)
                        (access-label . -)
                        (innamespace . 0))))
  "My C/C++ Programming Style")

(defun my-set-c-style ()
  "Set the current buffer's c-style to My C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (c-toggle-auto-newline 1)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Carleo" my-c-style t))

(defun my-make-newline-indent ()
  "Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
  (interactive)
  ;;(define-key c-mode-base-map "\C-m" 'newline-and-indent)
  ;;(define-key c-mode-base-map [ret] 'newline-and-indent))
  (define-key c-mode-base-map [?\r] 'newline-and-indent))

(provide 'my-c-style)
;;; end
