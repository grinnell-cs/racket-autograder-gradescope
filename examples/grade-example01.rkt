#lang racket

; This is an example autograder file, used for `example01.rkt`, which
; is supposed to include `increment`, a procedure that adds one to an
; exact value.

; Here's our grading library
(require "lib-grade.rkt")

; Since `lib-grade` overrides some of the tests, we need a special
; require command for rackunit that excludes those.
(require (except-in rackunit test-equal? test-= test-true test-false))

; We work in the student's directory when possible.
(when (directory-exists? "/autograder/submission")
  (current-directory "/autograder/submission"))

; Set up the prefix for local files
(define dir (if (directory-exists? "/autograder/source")
                "/autograder/source/"
                ""))

; It's easier to name the code file, since we may be using it
; multiple times.
(define code "example01.rkt")

; We build a test suite in the normal way.
(define-test-suite tests
  ; I tend to include some note at the beginning.
  (test-true "Warning!: This test suite is likely incomplete." #t)

  ; This is intended to be a quick test to ensure that the
  ; required file exists.
  (validate-code-file code)

  ; `set-context!` provides a context for each test.  I often use exercise
  ; or problem numbers.
  (set-context! "Exercise 1 (increment)")

  ; We need to extract procedures from the code file.  We do that
  ; with `proc-or-default`, usually in `let`.
  (let ([increment (proc-or-default 'increment code)])
    (test-equal? "zero" (increment 0) 1)
    (test-equal? "negative input" (increment -5) -4)
    (test-equal? "complex number" (increment 3+5i) 4+5i)
    (test-equal? "fraction" (increment 1/2) 3/2)))

; Here's where we actually run the tests.
(generate-results tests)

; The `(exit)` may not be strictly necessary, but it seems useful.
; (exit)