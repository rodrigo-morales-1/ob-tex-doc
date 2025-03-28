(defvar org-babel-default-header-args:tex-doc
  '((:build . (("pdflatex" _)))
    ;; By default, we set :results to "silent", because we don't show
    ;; anything in the #+RESULTS block because the build command is
    ;; executed asynchronously.
    (:results . "silent")))

(defun ob-tex-doc-check-executables-installed (cmds)
  "Given the content of the :cmd header argument. A one-liner
for executing all those commands is returned."
  (mapcar
   (lambda (cmd)
     (unless (executable-find (car cmd))
       (error "Binary %s was not found." (car cmd))))
   cmds))

(defun ob-tex-doc-build-command (cmds)
  "Given the content of the :cmd header argument. A one-liner
for executing all those commands is returned."
  (string-join
   (mapcar
    (lambda (x)
      (mapconcat
       (lambda (arg)
         (cond
          ((eq arg '_) "main")
          (t (shell-quote-argument arg))))
       x " "))
    cmds)
   " && "))

(defvar ob-tex-doc-tmp-dir nil
  "This variable is not intended to be modified since its value
  is automatically set.")

(defun ob-tex-doc-open-pdf-file ()
  "Open the resulting PDF file"
  (interactive)
  (unless ob-tex-doc-tmp-dir
    (error "The temporary directory hasn't been initialized yet, so there is no PDF file."))
  ;; In Ubuntu 24.04.1 using GNU Emacs 29.4, using make-process as
  ;; shown in the first code block below didn't open the file. However
  ;; using call-process as shown in second code block below did open
  ;; the file.  The problem with using call-process is that Emacs
  ;; freezes until the process finishes.
  ;;
  ;; #+BEGIN_SRC elisp
  ;; (make-process
  ;;  :name "xdg-open"
  ;;  :command `("xdg-open"
  ;;             ,(concat
  ;;               (file-name-as-directory ob-tex-doc-tmp-dir)
  ;;               "main.pdf"))))
  ;;
  ;; #+END_SRC
  ;;
  ;; #+BEGIN_SRC elisp
  ;; (call-process "xdg-open" nil nil nil (concat (file-name-as-directory ob-tex-doc-tmp-dir) "main.pdf"))
  ;; #+END_SRC
  ;;
  ;; UPDATE 2024-12-09: I used the sexp below and the file was
  ;; opened. I was using GNU Emacs 29.4 in Ubuntu 24.04.1. In a
  ;; previous occassion, I was using the same Emacs version and the
  ;; same Ubuntu version but the command didn't open the file, I don't
  ;; know what caused that issue.
  ;;
  ;; #+BEGIN_SRC elisp
  ;; (make-process
  ;;  :buffer "*foo*"
  ;;  :name "xdg-open"
  ;;  :command `("xdg-open" ,(concat (file-name-as-directory ob-tex-doc-tmp-dir) "main.pdf")))
  ;; #+END_SRC
  (make-process
   :name "xdg-open"
   :command `("xdg-open" ,(concat (file-name-as-directory ob-tex-doc-tmp-dir) "main.pdf"))))
(defun ob-tex-doc-set-temp-dir ()
  "Set the directory where code blocks are tangled, output
files are saved and log files created by TeX compilers are
saved.

The directory is fixed so that you can point a PDF viewer to
the main.pdf file in that directory."
  (unless ob-tex-doc-tmp-dir
    (setq ob-tex-doc-tmp-dir (make-temp-file "babel-" t))))

(defun ob-tex-doc-tmp-dir-in-tmp ()
  "Check directory, which contain the files that are generated
when compiling, is under the /tmp/ directory.

This is done to ensure that `ob-tex-doc-tmp-dir-clean' doesn't
remove unintended files."
  ;; TODO: Check directory doesn't contain symbolic links.
  (if (or (not (string-match "^/tmp/" ob-tex-doc-tmp-dir))
          (string-match "/../" ob-tex-doc-tmp-dir))
      nil
    t))

(defun ob-tex-doc-tmp-dir-clean ()
  (unless (ob-tex-doc-tmp-dir-in-tmp)
    (error "Value of ob-tex-doc-tmp-dir is not under /tmp/. Not
    proceeding to delete files."))

  ;; TODO: Include directories since some packages create them such as
  ;; minted.
  (dolist (file (directory-files-recursively ob-tex-doc-tmp-dir ""))
    (delete-file file)))

(defun org-babel-expand-body:tex-doc (body params)
  ;; If the header argument :expand is set to "no", then exit the
  ;; function because nothing is to be done.
  (let ((expand (or (cdr (assq :expand params))
                    (cdr (assq :expand org-babel-default-header-args:tex-doc))))
        ;; We initialize prologue and epilogue in this let form,
        ;; because they are going to be used in the returned body
        ;; regardless of the value of :expand
        (prologue (or (cdr (assq :prologue params))
                      (cdr (assq :prologue org-babel-default-header-args:tex-doc))))
        (epilogue (or (cdr (assq :epilogue params))
                      (cdr (assq :epilogue org-babel-default-header-args:tex-doc)))))
    (cond
     ((or
       ;; If :expand is not set in the header arguments of the code
       ;; block or in org-babel-default-header-args:tex-doc
       (null expand)
       ;; If :expand is explicitly set to "no"
       (equal expand "no"))
      (concat prologue body epilogue))
     ((equal expand "yes")
      (let ((cls
             (or (cdr (assq :cls params))
                 (cdr (assq :cls org-babel-default-header-args:tex-doc))))
            (preamble
             (or (cdr (assq :preamble params))
                 (cdr (assq :preamble org-babel-default-header-args:tex-doc))))
            (enclose
             (or (cdr (assq :enclose params))
                 (cdr (assq :enclose org-babel-default-header-args:tex-doc))))
            (pkg
             (or (cdr (assq :pkg params))
                 (cdr (assq :pkg org-babel-default-header-args:tex-doc))))
            (env
             (or (cdr (assq :env params))
                 (cdr (assq :env org-babel-default-header-args:tex-doc))))
            (comment-command
             (or (cdr (assq :cmd params))
                 (cdr (assq :cmd org-babel-default-header-args:tex-doc))))
            (comment
             (or (cdr (assq :comment params))
                 (cdr (assq :comment org-babel-default-header-args:tex-doc)))))
        ;; If the header arugment :comment is "no", there's no need to
        ;; build the command that is shown at the top of the expanded
        ;; buffer and that lists the required commnds for compiling the
        ;; document.
        (if (or
             (null comment)
             (equal comment "no"))
            (setq comment-command nil)
          (setq comment-command
                (string-join
                 `("%% This file is intended to be compiled by executing the following"
                   "%% commands:"
                   ,@(mapcar
                      (lambda (x)
                        (concat "%% $ "
                                (mapconcat
                                 (lambda (arg)
                                   (cond
                                    ((eq arg '_) "main")
                                    (t (shell-quote-argument arg))))
                                 x " ")))
                      comment-command))
                 "\n")))
        (when pkg
          (unless (listp pkg)
            (error "The parameter :pkg needs to be a list"))
          (setq pkg
                (string-join
                 (mapcar
                  (lambda (x)
                    (if (eq (string-to-char x) ?\[)
                        (concat "\\usepackage" x )
                      (concat "\\usepackage{" x "}")))
                  pkg)
                 "\n")))
        (when cls
          (setq cls
                (cond
                 ((or (equal (string-to-char cls) ?\{)
                      (equal (string-to-char cls) ?\[))
                  (concat "\\documentclass" cls))
                 ;; At this point, we know that "cls" is either an
                 ;; arbitrary value that doesn't start with a parentheses
                 ;; or square brackets, so we enclose it in square
                 ;; brackets.
                 (t
                  (concat "\\documentclass{" cls "}")))))
        (unless (equal enclose "no")
          (if (or (equal env "no")
                  (eq env nil))
              ;; If there is no environment, the body need to have empty
              ;; lines before and after it in order for body to be one
              ;; line separated from the document environment.
              (setq body (concat "\n" body "\n"))
            (setq body
                  (string-join (list (concat "\\begin{" env "}")
                                     body
                                     (concat "\\end{" env "}"))
                               "\n\n")))
          (setq body
                (string-join (list (concat "\\begin{document}")
                                   body
                                   (concat "\\end{document}"))
                             "\n")))
        (string-join
         ;; nil is deleted to ensure that string-join doesn't insert
         ;; newlines when some header arguments haven't been provided.
         (delq nil `(,comment-command
                     ,prologue
                     ,cls
                     ,pkg
                     ,preamble
                     ,body
                     ,epilogue))
         "\n\n"))))))

(defun org-babel-execute:tex-doc (body params)
  (let ((build (or (cdr (assq :build params))
                   (cdr (assq :build org-babel-default-header-args:tex-doc)))))
    (when (null build)
      (error "The header argument :build is nil"))
    (unless (listp build)
      (error "The value of :build must be a list"))
    (ob-tex-doc-set-temp-dir)
    (ob-tex-doc-check-executables-installed build)
    (setq build (ob-tex-doc-build-command build))
    (let ((buffer-name "*Async Shell Command* (tex-doc)")
          (async-shell-command-buffer 'confirm-kill-process)
          (default-directory ob-tex-doc-tmp-dir)
          (org-babel-default-header-args:tex-doc
           `(,@org-babel-default-header-args:tex-doc
             ;; We need to explicitly set the value for :tangle
             (:tangle . ,(concat ob-tex-doc-tmp-dir "/main.tex"))))
          (display-buffer-overriding-action
           '(display-buffer-no-window)))
      ;; Remove auxiliary and log files to ensure that automatically
      ;; created files by previous compilations doesn't interfere with
      ;; the current one.
      (ob-tex-doc-tmp-dir-clean)
      ;; Tangle the source code block at point.
      (let ((current-prefix-arg '(4)))
        (call-interactively 'org-babel-tangle))
      ;; Compile the document
      (message "ob-tex-doc: Executing build command: %s" build)
      (async-shell-command build buffer-name))))

(add-to-list 'org-src-lang-modes '("tex-doc" . latex))

(provide 'ob-tex-doc)
