(defcustom ob-tex-doc-default-cmd '(("pdflatex" _))
  "Default TeX compiler command")

(defcustom ob-tex-doc-cmd-separator "&&"
  "Separator for the commands provided through the :cmd header
  argument.")

(defvar org-babel-default-header-args:tex-doc nil)

(defun ob-tex-doc-check-executables-installed (cmds)
  "Given the content of the :cmd header argument. A one-liner
for executing all those commands is returned."
  (mapcar
   (lambda (cmd)
     (unless (executable-find (car cmd))
       (error "Executable %s is not installed" (car cmd))))
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
  (make-process
   :name "xdg-open"
   :command `("xdg-open"
              ,(concat
                (file-name-as-directory ob-tex-doc-tmp-dir)
                "main.pdf"))))

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
  (catch 'done
    ;; If the header argument :expand is set to "no", then exit the
    ;; function because nothing is to be done.
    (let ((expand (cdr (assq :expand params))))
      (when (equal expand "no")
        (throw 'done body)))
    (let ((prologue (cdr (or (assq :prologue params)
                             (assq :prologue org-babel-default-header-args:tex-doc))))
          (epilogue (cdr (or (assq :epilogue params)
                             (assq :epilogue org-babel-default-header-args:tex-doc))))
          (cls (cdr (or (assq :cls params)
                        (assq :cls org-babel-default-header-args:tex-doc))))
          (preamble (cdr (or (assq :preamble params)
                             (assq :preamble org-babel-default-header-args:tex-doc))))
          (enclose (cdr (or (assq :enclose params)
                            (assq :enclose org-babel-default-header-args:tex-doc))))
          (pkg (cdr (or (assq :pkg params)
                        (assq :pkg org-babel-default-header-args:tex-doc))))
          (env (cdr (or (assq :env params)
                        (assq :env org-babel-default-header-args:tex-doc))))
          (cmd (cdr (or (assq :cmd params)
                        (assq :cmd org-babel-default-header-args:tex-doc))))
          (comment (cdr (or (assq :comment params)
                            (assq :comment org-babel-default-header-args:tex-doc))))
          content)
      ;; If the header arugment :comment is "no", there's no need to
      ;; build the command that is shown at the top of the expanded
      ;; buffer and that lists the required commnds for compiling the
      ;; document.
      (if (equal comment "no")
          (setq cmd nil)
        (progn
          (unless cmd
            (setq cmd ob-tex-doc-default-cmd))
          (setq cmd
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
                      cmd))
                 "\n"))))
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
      ;; nil is deleted to ensure that string-join doesn't insert
      ;; newlines when some header arguments haven't been provided.
      (setq content (delq nil `(,cmd
                                ,prologue
                                ,cls
                                ,pkg
                                ,preamble
                                ,body
                                ,epilogue)))
      (string-join content "\n\n"))))

(defun org-babel-execute:tex-doc (body params)
  (let ((cmd (cdr (assq :cmd params)))
        (cmd-pre (cdr (or (assq :cmd-pre params)
                          (assq :cmd-pre org-babel-default-header-args:tex-doc))))
        command)
    (ob-tex-doc-set-temp-dir)
    (unless cmd
      (setq cmd ob-tex-doc-default-cmd))
    (unless (listp cmd)
      (error "The value of :cmd needs to be a list"))
    ;; Even if we are using `ob-tex-doc-default-cmd', we need to check
    ;; that the executables are installed.
    (ob-tex-doc-check-executables-installed cmd)
    (setq cmd
          (concat
           cmd-pre
           (ob-tex-doc-build-command cmd)))
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
      (message "Executing %s" cmd)
      (async-shell-command cmd buffer-name))))

(add-to-list 'org-src-lang-modes '("tex-doc" . latex))

(provide 'ob-tex-doc)
