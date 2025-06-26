(define driver
    (lambda (program)
        (with-output-to-file "t.s"
            (lambda ()
                (generate-x86-64 (verify-scheme program))))))