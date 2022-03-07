#lang racket

; lib-grade.rkt
; Written by Shriram Krishnamurthi
; Updated by Samuel A. Rebelsky (mostly the commented parts)

(require racket/undefined)
(require lang/prim)
(require rackunit)
(require json)

(provide produce-report/exit
         define-var
         extract-variable
         generate-results
         id-exists?
         proc-or-default
         proc-tests
         proc-tests-core
         set-context!
         test-equal?
         test-=
         test-true
         test-false
         validate-code-file
         validate-file
         value-of)

(provide mirror-macro)

; +--------------------------+---------------------------------------
; | New versions of test-xxx |
; +--------------------------+

(define context "")
(define set-context!
  (lambda (new-context)
    (if (string=? new-context "")
        (set! context "")
        (set! context (string-append new-context ": ")))))

;;; (define-shortcut test-form check-form) -> macro
;;;   test-form : list? (e.g., `(test-equal? expr1 expr2)`)
;;;   check-form : list? (e.g., `(check-equal? expr1 expr2)`)
;;; Makes a test form.
;;;
;;; Note:
;;;   `define-shortcut` is borrowed and modified from 
;;;      rackunit/private/test.rkt
;;;   in the Racket source code.  The new version includes a context and
;;;   prints out the original expression.
(define-syntax (define-shortcut stx)
  (syntax-case stx ()
    [(_ (name param ...) expr)
     (with-syntax ([expected-form (syntax->datum
                                   #`(#,(syntax name)
                                      test-desc
                                      #,@(syntax (param ...))))])
       (syntax/loc stx
         (define-syntax (name name-stx)
           (syntax-case name-stx ()
             [(name test-desc param ...)
              (with-syntax ([name-expr (syntax/loc name-stx expr)])
                (syntax/loc name-stx
                  (test-case (format "†~a~a [expression: ~v]"
                                     context test-desc
                                     (car (syntax->datum (syntax (param ...)))))
                             name-expr)))]
             [_
              (raise-syntax-error
               #f
               (format "Correct form is ~a" (quote expected-form))
               name-stx)]))))]
    [_
     (raise-syntax-error
      #f
      "Correct form is (define-shortcut (name param ...) expr)"
      stx)]))

(define-shortcut (test-pred pred expr)
  (check-pred pred expr))

(define-shortcut (test-equal? expr1 expr2)
  (check-equal? expr1 expr2))

(define-shortcut (test-eq? expr1 expr2)
  (check-eq? expr1 expr2))

(define-shortcut (test-eqv? expr1 expr2)
  (check-eqv? expr1 expr2))

(define-shortcut (test-= expr1 expr2 epsilon)
  (check-= expr1 expr2 epsilon))

(define-shortcut (test-within expr1 expr2 epsilon)
  (check-within expr1 expr2 epsilon))

(define-shortcut (test-true expr)
  (check-true expr))

(define-shortcut (test-false expr)
  (check-false expr))

(define-shortcut (test-not-false expr)
  (check-not-false expr))

(define-shortcut (test-exn pred thunk)
  (check-exn pred thunk))

(define-shortcut (test-not-exn thunk)
  (check-not-exn thunk))

; +------------------------+-----------------------------------------
; | Support for SamR style |
; +------------------------+

;;; (sprintf form v ...) -> string?
;;;   form : string?
;;;   v : any/c
;;; Print formatted output to a string
(define sprintf
  (lambda (form . v)
    (let ([str-port (open-output-string)])
      (apply fprintf (cons str-port (cons form v)))
      (let ([str (get-output-string str-port)])
        (close-output-port str-port)
        str))))

;;; (val->str v) -> string?
;;;   v : any?
;;; Generate the string representation of any value
;;; (the one from print).
(define val->str
  (lambda (val)
    (sprintf "~v" val)))

;;; (proc-tests problem procname fname tests) -> void?
;;; Run a bunch of tests.  Each test has the
;;; form `(output param1 param2 ...)`.
(define proc-tests
  (lambda (problem procname fname tests)
    (proc-tests-core problem procname fname 
                     (map (lambda (test)
                            (cons (list (car test)) (cdr test)))
                          tests))))
      
;;; (proc-tests-core problem procname fname tests) -> void?
;;; Run a bunch of tests.  Each test has the form
;;; `((list-of-possible-outputs) param1 param2 ...)`.
(define proc-tests-core
  (lambda (problem procname fname tests)
    (let ([proc
           (if (id-exists? procname fname)
               (extract-variable procname fname)
               #f)]
          [exists
           (sprintf "~a: Procedure ~a exists"
                    problem
                    procname)])
      (if proc
          (test-true exists #t)
          (test-true exists #f))
      (for-each
       (lambda (test)
         (let* ([input (cdr test)]
                [expected (car test)]
                [mess
                 (sprintf "~a: ~v"
                          problem
                          (cons procname input))]
                [message (string-replace mess "#" "#")])
           (if proc
               (test-true message
                          (and (member (apply proc input) expected) #t))
               (test-true message #f))))
       tests))))


;;; (extract-variable varname fname) -> any/c?
;;;   varname : symbol?
;;;   fname : string?
;;; Extract the given variable from the file.
;;; Throw an error if it's not there.
(define extract-variable
  (lambda (varname fname)
    (dynamic-require `(file ,fname) varname
                     (thunk
                      (dynamic-require `(file ,fname) #f)
                      (define ns (module->namespace `(file ,fname)))
                      (eval `(,#'first-order->higher-order ,varname) ns)))))

;;; (validate-code-file codefile) -> (void)
;;;   codefile : string
;;; Runs a test to ensure that the given file exists.
(define validate-code-file
  (lambda (codefile)
    ; Make sure the file exists.
    (when (not (file-exists? codefile))
      (produce-report/exit
       `#hasheq((score . "0")
                (tests . (#hasheq((name . ,(string-append "Code file '"
                                                          codefile
                                                          "' is missing."))
                                  (output . "Please make sure that you've named your code file correctly.")))))))
    ; Make sure that the file is in the right form.
    (when (binary-file? codefile)
      (produce-report/exit
       `#hasheq((score . "0")
                (tests . (#hasheq((name . ,(string-append "Cannot read '"
                                                          codefile
                                                          "'."))
                                  (output . "Your Racket file may not be in readable form.\nPlease save it with\n  File -> Save Other -> Save Definitions As Text...\nThen resubmit.  If that still doesn't work, contact one of the course staff.")))))))

    ; Make sure the file loads properly
    (with-handlers
        ([exn:fail? 
          (lambda (e)
            (produce-report/exit
             '#hasheq((score . "0")
                      (tests . (#hasheq((name . "Code does not run")
                                        (output .
                                         "Your code file does not run.\nThe most likely issues are (a) missing files, (b) inclusion of nonstandard libraries, or (c) a Racket file that with an error.\nPlease download your complete submission to a new folder,\ntry to run it from there, and see if you can tell what went wrong.\nIf not, ask someone for help.")))))))])
      (extract-variable 'cons codefile))))

;;; (validate-file fname) -> (void)
;;;   fname: string
;;; Runs a test to ensure that the given file exists.
(define validate-file
  (lambda (fname)
    (test-true (string-append "General: File '" fname "' exists")
               (file-exists? fname))))

;;; (id-exists? id fname) -> boolean?
;;;   id : symbol?
;;;   fname : string?
;;; Determines if the id names something in the given file.
(define id-exists?
  (let ([kernel
         (lambda (id fname)
           (with-handlers ([exn:fail? (lambda (e) #f)])
             (extract-variable id fname)
             #t))])
    (lambda (id fname)
      ; The first time you read a file, if there's
      ; an error, (extract-variable ...) does not work.
      ; But the second time, it does.  Go figure.
      (or (kernel id fname)
          (kernel id fname)))))

;;; (proc-or-default procname fname) -> procedure?
;;;   procname : symbol?
;;;   fname : string?
;;; Look up the procedure in the given file.  If the procedure does
;;; not exist, returns a procedure that reports errors.  (Useful
;;; once we know a procedure exists.)
(define proc-or-default
  (lambda (procname fname)
    (with-handlers
        ([exn:fail? (lambda (e)
                      (lambda params
                        (error 'error
                               "procedure ~s is not defined in ~v"
                               procname
                               fname)))])
      (extract-variable procname fname))))

;;; (value-of id fname) -> any/c?
;;;   id : symbol?
;;;   fname : string?
;;; Look up the identifier in the given file.  If the identifier
;;; does not exist, uses undefined.
(define value-of
  (lambda (id fname)
    (with-handlers
        ([exn:fail? (lambda (e) undefined)])
      (extract-variable id fname))))

; +------------------+-----------------------------------------------
; | Other procedures |
; +------------------+

;;; (binary-file? filename) -> boolean?
;;; Determines if a file appears to be a Racket binary file.
(define binary-file?
  (lambda (filename)
    (let* ([port (open-input-file filename)]
           [opening (read-line port)])
     (close-input-port port)
     (string-prefix? opening "#reader"))))

;;; default-filename : string?
;;; The default file name for the report file.
(define default-filename
  (if (directory-exists? "/autograder/results")
      "/autograder/results/results.json"
      "./results.json"))

;;; (produce-report grade-hash [fname]) -> void?
;;;   grade-hash : Test results
;;;   fname : string?
;;; Write the report to the given file (or the default file).
(define (produce-report grade-hash [fname default-filename])
  (with-output-to-file fname
    #:exists 'replace
    (lambda ()
      (write-json grade-hash))))

;;; (test-result result tr) -> hash
;;;   result : string?
;;;   tr : result of a test
;;; Create an appropriate hash for describing a test result.
(define test-result
  (lambda (result tr)
    (let* ([tmp (test-result-test-case-name tr)]
           [name (if (string? tmp)
                     (substring tmp 1)
                     "Unspecified test")])
      `#hasheq((output . ,name)
               (name . ,result)))))

;;; (reportable-test? tr) -> boolean?
;;;   tr : test-result?
;;; Determines if tr is a result we want to report
(define (reportable-test? tr)
  (let ([name (test-result-test-case-name tr)])
    (and (string? name)
         (>= (string-length name) 1)
         (equal? (string-ref name 0) #\†))))

;;; (generate-results test-suite) -> void?
;;;    test-suite : A Rackunit test suite
;;; Generate results for the given test suite, storing them to the
;;; default file for test results.
(define (generate-results test-suite)
  (let* ([raw-test-results (fold-test-results cons empty test-suite)]
         [test-results (filter reportable-test? raw-test-results)]
         [tests-passed (length (filter test-success? test-results))]
         [tests-conducted (length test-results)]
         [raw-score (min 0.9999
                         (/ (round (* 100 (/ tests-passed tests-conducted))) 100))]
         [score-str (number->string (exact->inexact raw-score))]
         [count 0]
         [number! (lambda () (set! count (+ 1 count)) count)])
    (produce-report
     `#hasheq((score . ,score-str)
              (tests . ,(append
                         (list
                          `#hasheq((name . "Summary")
                                   (output
                                    . ,(string-append (number->string tests-passed)
                                                      " of "
                                                      (number->string tests-conducted)
                                                      " tests passed."))))
                         (map (lambda (t)
                                (test-result (format "Test ~a ~a"
                                                     (number!)
                                                     (cond
                                                       [(test-error? t)
                                                        "FAILED by producing an error"]
                                                       [(test-failure? t)
                                                        "FAILED"]
                                                       [(test-success? t)
                                                        "PASSED"]
                                                       [else
                                                        "GAVE A STRANGE RESULT"]))
                                             t))
                              (reverse test-results))))))))

; +---------------+--------------------------------------------------
; | Original code |
; +---------------+

; The original code, more or less.

(define (produce-report/exit grade-hash [fname default-filename])
  (produce-report grade-hash fname)
  (exit))

(define-syntax define-var
  (syntax-rules (from)
    [(_ var-name from base-filename)
     (define var-name
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (produce-report/exit
                           `#hasheq((score . 0)
                                    (output . ,(string-append "Run failed with error\n"
                                                              (exn-message e))))))])
         (define bfn base-filename)
         (define filename (string-append "/autograder/submission/" bfn))
         (if (file-exists? filename)
             (with-handlers ([exn:fail?
                              (lambda (e)
                                (produce-report/exit
                                 `#hasheq((score . 0)
                                          (output . ,(string-append "Loading failed with error\n"
                                                                    (exn-message e))))))])
               (dynamic-require `(file ,filename) 'var-name
                                (thunk
                                 (dynamic-require `(file ,filename) #f)
                                 (define ns (module->namespace `(file ,filename)))
                                 (eval `(,#'first-order->higher-order var-name) ns))))
             (produce-report/exit
              `#hasheq((score . 0)
                       (output . ,(string-append "File " bfn " not found: please check your submission")))))))]))

(define-syntax mirror-macro
  (syntax-rules (from)
    [(_ macro-name from base-filename)
     (define-syntax macro-name
       (syntax-rules ()
         [(_ E (... ...))
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (produce-report/exit
                              `#hasheq((score . 0)
                                       (output . ,(string-append "Run failed with error\n"
                                                                 (exn-message e))))))])
            (define bfn base-filename)
            (define filename (string-append "/autograder/submission/" bfn))
            (if (file-exists? filename)
                (with-handlers ([exn:fail?
                                 (lambda (e)
                                   (produce-report/exit
                                    `#hasheq((score . 0)
                                             (output . ,(string-append "Loading failed with error\n"
                                                                       (exn-message e))))))])
                  (eval '(macro-name E (... ...))
                        (module->namespace
                         (begin
                           (dynamic-require `(file , filename) #f)
                           `(file ,filename)))))
                (produce-report/exit
                 `#hasheq((score . 0)
                          (output . ,(string-append "File " bfn " not found: please check your submission"))))))]))]))


(define (generate-results-old test-suite)
  (let* ([test-results (fold-test-results cons empty test-suite)]
         [tests-passed (length (filter test-success? test-results))]
         [tests-conducted (length test-results)]
         [raw-score (/ (round (* 100 (/ tests-passed tests-conducted))) 100)]
         [score-str (number->string (exact->inexact raw-score))])
    (if (= raw-score 1)
        (produce-report/exit
         `#hasheq((score . "0")
                  (output . "All tests passed.")))
        (produce-report/exit
         `#hasheq((score . ,score-str)
                  (tests . ,(append
                             (list
                              `#hasheq((name . "Summary")
                                       (output
                                        . ,(string-append (number->string tests-passed)
                                                          " of "
                                                          (number->string tests-conducted)
                                                          " tests passed."))))
                             (map (λ (t)
                                    (test-result "Test failed with error" t))
                                  (filter test-error? test-results))
                             (map (λ (t)
                                    (test-result "Test failed" t))
                                  (filter test-failure? test-results)))))))))

