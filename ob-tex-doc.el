(defcustom ob-tex-doc-compilation-buffer-name "*Async Shell Command* (tex-doc)"
  "Name of the buffer that shows the compilation output.")

(defcustom ob-tex-doc-force-display-compilation-buffer t
  "Boolean value that determines whether the compilation buffer should be
displayed whenever a tex-doc code block is run.")

(defvar org-babel-default-header-args:tex-doc
  '((:build . (("pdflatex" jobname)))
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
          ((eq arg 'jobname) "main")
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

(defun ob-tex-doc-set-tmp-dir ()
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
  (cond
   ((eq system-type 'windows-nt)
    (if (string-match "/AppData/Local/Temp/" ob-tex-doc-temp-dir)
	t
      nil))
   ((eq system-type 'gnu/linux)
    (if (string-match "^/tmp/" ob-tex-doc-temp-dir)
	t
      nil))))

(defun ob-tex-doc-tmp-dir-clean ()
  (unless (ob-tex-doc-tmp-dir-in-tmp)
    (error "Value of ob-tex-doc-tmp-dir is not under /tmp/. Not
    proceeding to delete files."))

  ;; TODO: Include directories since some packages create them such as
  ;; minted.
  (dolist (file (directory-files-recursively ob-tex-doc-tmp-dir ""))
    (delete-file file)))

(defun ob-tex-doc-display-compilation-buffer ()
  "Display compilation buffer."
  (interactive)
  (display-buffer ob-tex-doc-compilation-buffer-name))

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
      (let ((documentclass
             (or (cdr (assq :documentclass params))
                 (cdr (assq :documentclass org-babel-default-header-args:tex-doc))))
            (preamble
             (or (cdr (assq :preamble params))
                 (cdr (assq :preamble org-babel-default-header-args:tex-doc))))
            (usepackage
             (or (cdr (assq :usepackage params))
                 (cdr (assq :usepackage org-babel-default-header-args:tex-doc))))
            (environment
             (or (cdr (assq :environment params))
                 (cdr (assq :environment org-babel-default-header-args:tex-doc))))
            (tangle-build-command
             (or (cdr (assq :tangle-build-command params))
                 (cdr (assq :tangle-build-command org-babel-default-header-args:tex-doc))))
             comment-build-command)
        ;; If the header arugment :comment is "no", there's no need to
        ;; build the command that is shown at the top of the expanded
        ;; buffer and that lists the required commnds for compiling the
        ;; document.
        (if (or
             (null tangle-build-command)
             (equal tangle-build-command "no"))
            (setq comment-build-command nil)
          (let ((build-command (or (cdr (assq :build-command params))
                                   (cdr (assq :build-command org-babel-default-header-args:tex-doc)))))
            (setq comment-build-command
                  (if (null build-command)
                      nil
                    (string-join
                     `(,(concat "%% This file is intended to be compiled by executing the commands in\n"
                                "%% the following order:\n"
                                "%%")
                       ,@(mapcar
                          (lambda (x)
                            (concat "%% $ "
                                    (mapconcat
                                     (lambda (arg)
                                       (cond
                                        ((eq arg 'jobname) "main")
                                        (t (shell-quote-argument arg))))
                                     x " ")))
                          build-command))
                     "\n")))))
        (when usepackage
          (unless (listp usepackage)
            (error "The parameter :usepackage needs to be a list"))
          (setq usepackage
                (string-join
                 (mapcar
                  (lambda (x)
                    (if (eq (string-to-char x) ?\[)
                        (concat "\\usepackage" x )
                      (concat "\\usepackage{" x "}")))
                  usepackage)
                 "\n")))
        (when documentclass
          (setq documentclass
                (cond
                 ((or (equal (string-to-char documentclass) ?\{)
                      (equal (string-to-char documentclass) ?\[))
                  (concat "\\documentclass" documentclass))
                 ;; At this point, we know that "documentclass" is either an
                 ;; arbitrary value that doesn't start with a parentheses
                 ;; or square brackets, so we enclose it in square
                 ;; brackets.
                 (t
                  (concat "\\documentclass{" documentclass "}")))))
        (unless (or (equal environment "no")
                    (eq environment nil))
              ;; If there is no environment, the body need to have empty
              ;; lines before and after it in order for body to be one
              ;; line separated from the document environment.
            (setq body
                  (string-join (list
                                (concat "\\begin{" environment "}")
                                body
                                (concat "\\end{" environment "}"))
                               "\n")))
          (setq body (string-join
                      (list
                       "\\begin{document}"
                       body
                       "\\end{document}")
                       "\n"))
        (string-join
         ;; nil is deleted to ensure that string-join doesn't insert
         ;; newlines when some header arguments haven't been provided.
         (delq nil `(,comment-build-command
                     ,prologue
                     ,documentclass
                     ,usepackage
                     ,preamble
                     ,body
                     ,epilogue))
         "\n\n"))))))

(defun org-babel-execute:tex-doc (body params)
  (let ((build-command (or (cdr (assq :build-command params))
                           (cdr (assq :build-command org-babel-default-header-args:tex-doc))))
        build-command-string)
    (when (null build-command)
      (error "The header argument :build-command is nil"))
    (unless (listp build-command)
      (error "The value of :build-command must be a list"))
    (ob-tex-doc-set-tmp-dir)
    (ob-tex-doc-check-executables-installed build-command)
    (setq build-command-string (ob-tex-doc-build-command build-command))
    (let ((buffer-name ob-tex-doc-compilation-buffer-name)
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
      (message "ob-tex-doc: Executing build command: %s" build-command-string)
      (async-shell-command build-command-string buffer-name)
      (when ob-tex-doc-force-display-compilation-buffer
        (ob-tex-doc-display-compilation-buffer)))))

(add-to-list 'org-src-lang-modes '("tex-doc" . latex))

(provide 'ob-tex-doc)
