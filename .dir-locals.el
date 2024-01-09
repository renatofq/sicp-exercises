((nil
  . ((eval
      . (set 'geiser-repl-startup-hook
             (let* ((dir (dir-locals-find-file "."))
                    (full-path (expand-file-name
                                "defs.scm"
                                (if (stringp dir) dir (car dir)))))
               (lambda () (geiser-load-file full-path)))))))
 (org-mode . ((mode . flyspell)
              (mode . olivetti)
              (ispell-local-dictionary . "american")
              (org-export-with-toc . nil)
              (org-export-with-section-numbers . nil)
              (org-html-head . "<style type=\"text/css\">.org-svg { max-width: 100%; }</style>")
              (org-global-properties . ((header-args . ":results output")))
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
