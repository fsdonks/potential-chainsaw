(defun cider-rick-roll (&optional prompt-project cljs-too)
  "A suboptimal version of `cider-jack-in' that calls `java'
directly, bypassing `lein'."
  (interactive "P")
  (setq cider-current-clojure-buffer (current-buffer))
  (let* ((project-type "lein") ;<- Only for "lein" project type. :(
         (command "java")
         (script-name (cider-jack-in-resolve-command project-type))
         (jar-path
          (concat (replace-regexp-in-string
                   "\\\\" "/" (getenv "USERPROFILE")) ;<- hack, Windoze only. :(
                  "/.lein/self-installs/leiningen-2.7.1-standalone.jar"))
         (command-resolved
          (concat "java -Xbootclasspath/a:" jar-path
                  " -Dfile.encoding=UTF-8 -Dmaven.wagon.http.ssl.easy=false"
                  " -Dmaven.wagon.rto=10000 -XX:+TieredCompilation -XX:TieredStopAtLevel=1"
                  " -Dleiningen.original.pwd=" default-directory
                  " -Dleiningen.script=" script-name
                  " -classpath " jar-path
                  " clojure.main -m leiningen.core.main"))
         (command-params (cider-jack-in-params project-type)))
    (if command-resolved
        (let* ((project (when prompt-project
                          (read-directory-name "Project: ")))
               (project-dir (clojure-project-dir
                             (or project (cider-current-dir))))
               (params (if prompt-project
                           (read-string (format "nREPL server command: %s "
                                                command-params)
                                        command-params)
                         command-params))
               (params (if cider-inject-dependencies-at-jack-in
                           (cider-inject-jack-in-dependencies params project-type)
                         params))

               (cmd (format "%s %s" command-resolved params)))
          (if (or project-dir cider-allow-jack-in-without-project)
              (progn
                (when (or project-dir
                          (eq cider-allow-jack-in-without-project t)
                          (and (null project-dir)
                               (eq cider-allow-jack-in-without-project 'warn)
                               (y-or-n-p "Are you sure you want to run `cider-jack-in' without a Clojure project? ")))
                  (when-let ((repl-buff (cider-find-reusable-repl-buffer nil project-dir)))
                    (let ((nrepl-create-client-buffer-function  #'cider-repl-create)
                          (nrepl-use-this-as-repl-buffer repl-buff))
                      (nrepl-start-server-process
                       project-dir cmd
                       (when cljs-too #'cider-create-sibling-cljs-repl))))))
            (user-error "`cider-jack-in' is not allowed without a Clojure project")))
      (user-error "The %s executable isn't on your `exec-path'" command))))

(fset 'cider-jack-in-orig 'cider-jack-in)
(fset 'cider-jack-in 'cider-rick-roll)

(provide 'dot-cider)
