((org-mode . ((mode . flyspell)
              (mode . olivetti)
              (ispell-local-dictionary . "american")
              (org-export-with-toc . nil)
              (org-export-with-todo-keywords . nil)
              (org-export-with-section-numbers . nil)
              (org-global-properties . ((header-args . ":results output")))
              (eval . (add-hook 'geiser-repl-startup-hook
                                (lambda ()
                                  (geiser-load-file "defs.scm"))))
              (eval . (advice-add
                       'org-babel-insert-result
                       :filter-args
                       (lambda (args)
                         (let ((result (car args))
                               (result-params (cadr args))
                               (others (cddr args)))
                           (apply 'list
                                  result
                                  (if (or (string-empty-p result) (not result))
                                      (progn (org-babel-remove-result) '("silent"))
                                    result-params)
                                  others))))))))
