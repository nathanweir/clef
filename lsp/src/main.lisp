(defpackage :clef-lsp/src/main
  (:use :cl)
  (:local-nicknames
    (:handlers :clef-lsp/src/lsp/handlers)
    (:lint :clef-lsp/src/lint)
    (:scaffold :clef-lsp/src/scaffold)
    (:server :clef-lsp/src/lsp/server))
  (:export
   #:main
   #:start-server))

(in-package :clef-lsp/src/main)

;; claim-protocol-stream needs sb-posix at read time. Required here rather than
;; via an ASDF (:require ...) dependency: clef's own workspace-system loader
;; assumes every :depends-on entry is a symbol or string, so a list form there
;; breaks initialize for anyone whose project uses one.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun env-log-settings ()
       "Logging config for the shipped binary, from the environment.

        Normal use writes nothing to disk. Set CLEF_LOG_FILE=/path/to/log to opt
        in when debugging -- no rebuild needed. A relative path is resolved
        against the process's working directory."
       (let ((path (sb-posix:getenv "CLEF_LOG_FILE")))
            (if (and path (plusp (length path)))
                (values :file (merge-pathnames path (uiop:getcwd)))
                (values :none nil))))

(defun start-server (&key (input *standard-input*) (output *standard-output*)
                          (log-mode :none) log-file-path)
       ;; Nothing may be logged before clef-lsp/src/lsp/server:start calls clef-lsp/src/log:init.
       (server:start :input input :output output
                              :log-mode log-mode :log-file-path log-file-path
                              :register #'handlers:register-handlers))

(defun claim-protocol-stream ()
       "Move the real stdout to a private fd and point fd 1 at stderr.

        Returns a stream on the private fd; it is the only thing that may carry
        LSP framing. Everything else in the process -- stray format t, foreign
        library chatter, subprocesses that inherit fd 1 -- lands on stderr and
        cannot corrupt the client's stream."
       (let ((protocol-stream (sb-sys:make-fd-stream (sb-posix:dup 1)
                                                     :output t
                                                     :element-type :default
                                                     :buffering :full)))
            (sb-posix:dup2 2 1)
            protocol-stream))

;;; ---------------------------------------------------------------------------
;;; The umbrella
;;;
;;; One binary, per roadmap §W8½. Invoked bare over PIPES -- how every editor
;;; launches a language server -- it serves LSP exactly as it always has, so no
;;; editor configuration changes. Invoked bare on a TERMINAL it prints help
;;; instead of silently swallowing the keyboard as a protocol stream, which is
;;; what a curious `clef` at a shell prompt used to do.
;;; ---------------------------------------------------------------------------

(defparameter *usage*
  "clef -- Common Lisp development tooling

Usage:
  clef                    serve LSP over stdio (when stdin is a pipe)
  clef lsp                serve LSP over stdio, explicitly
  clef run FILE [...]     run a program with humane errors and real exit codes
                          (all clef-run options apply; try `clef run --help')
  clef new DIR [k=v...]   scaffold a golden-path project named after DIR
                          (a new or existing directory, or `.'; parameters:
                          author=..., description=..., license=...)
  clef lint [DIR]         check the golden-path package convention
  clef version            print version
  clef help               this text

An editor pointed at this binary needs no arguments; bare invocation on a pipe
is the server.
")

(defun binary-version ()
       (or (ignore-errors
            (asdf:component-version (asdf:find-system :clef-lsp)))
           "unknown"))

(defun serve-lsp ()
       "Serve LSP over stdio. Never returns except by exit."
       (let ((protocol-stream (claim-protocol-stream)))
            (handler-case
                (multiple-value-bind (log-mode log-file-path) (env-log-settings)
                  (start-server :output protocol-stream
                                :log-mode log-mode
                                :log-file-path log-file-path))
              (sb-sys:interactive-interrupt () (sb-ext:exit :code 0 :abort t))
              (error (e)
                     (format *error-output* "clef: fatal: ~A~%" e)
                     (finish-output *error-output*)
                     (sb-ext:exit :code 1 :abort t))))
       (sb-ext:exit :code 0))

(defun parse-new-args (args)
       "(values name params-alist error-message) for `clef new' arguments."
       (let ((name (first args))
             (params '()))
            (if (null name)
                (values nil nil "clef new needs a project name")
                (loop for arg in (rest args)
                      for eq = (position #\= arg)
                      if eq
                        do (push (cons (subseq arg 0 eq) (subseq arg (1+ eq)))
                                 params)
                      else
                        do (return (values nil nil
                                           (format nil "~S is not a key=value parameter" arg)))
                      finally (return (values name (nreverse params) nil))))))

(defun dispatch (command args)
       "Run COMMAND and return an exit code."
       (cond
         ((string= command "lsp") (serve-lsp))
         ((string= command "run")
          ;; Resolved at run time, not compile time: the runner is a separate
          ;; ASDF system that build.lisp loads into the shipped image alongside
          ;; this one. Keeping it out of :clef-lsp's :depends-on means every
          ;; from-source entry point (start-server.sh, the test runner, the
          ;; experiment scripts) keeps working unchanged -- they load only the
          ;; LSP, and only this subcommand needs the runner.
          (if (find-package :clef-runner)
              (uiop:symbol-call :clef-runner :main args)
              (progn
                (format *error-output*
                        "clef run: the runner is not loaded in this image.~%~
                         (Running from source? Use runner/clef-run, or build ~
                         the full binary with `mise run build'.)~%")
                1)))
         ((string= command "new")
          (multiple-value-bind (name params err) (parse-new-args args)
            (cond
              (err (format *error-output* "clef new: ~A~%" err) 2)
              (t (handler-case
                     (multiple-value-bind (dir kept) (scaffold:new-project name :params params)
                       (let ((here (uiop:pathname-equal dir (uiop:getcwd))))
                            (format t "Created ~A from the clef golden-path template.~%"
                                    (uiop:native-namestring dir))
                            (when kept
                                  (format t "Kept your ~{~A~^ and ~}; the template's ~
                                             ~:[copy was~;copies were~] not written.~%"
                                          kept (rest kept))
                                  (when (member ".gitignore" kept :test #'string=)
                                        (format t "The template's .gitignore ignores ~
                                                   ocicl/ (vendored deps) and *.fasl -- ~
                                                   make sure yours does.~%")))
                            (format t "Next: ~:[cd ~A && ~;~*~]make test~%"
                                    here
                                    (uiop:native-namestring
                                     (uiop:enough-pathname dir (uiop:getcwd))))
                            0))
                   (error (e)
                          (format *error-output* "clef new: ~A~%" e)
                          1))))))
         ((string= command "lint")
          (handler-case (lint:main args)
            (error (e)
                   (format *error-output* "clef lint: ~A~%" e)
                   1)))
         ((member command '("version" "--version" "-V") :test #'string=)
          (format t "clef ~A~%" (binary-version))
          0)
         ((member command '("help" "--help" "-h") :test #'string=)
          (write-string *usage*)
          0)
         (t (format *error-output* "clef: unknown command ~S~%~%" command)
            (write-string *usage* *error-output*)
            2)))

(defun main ()
       "Toplevel entry point for the saved executable.

        The image already has every system loaded, so starting up touches no
        ASDF, no `make', no compiler and nothing from the dev shell. It also
        writes no log file unless CLEF_LOG_FILE asks for one."
       (sb-ext:disable-debugger)          ; never drop into a REPL on a pipe
       ;; Re-initialise ASDF against the environment we were actually launched
       ;; in; the image was dumped with its configuration cleared.
       (uiop:call-image-restore-hook)
       (let ((argv (rest sb-ext:*posix-argv*)))
            (cond
              ;; Bare on a pipe: the language server, exactly as always.
              ((and (null argv) (not (interactive-stream-p *standard-input*)))
               (serve-lsp))
              ;; Bare at a terminal: a human exploring. Help, not a hang.
              ((null argv)
               (write-string *usage*)
               (sb-ext:exit :code 0))
              (t (sb-ext:exit :code (or (dispatch (first argv) (rest argv)) 0))))))
