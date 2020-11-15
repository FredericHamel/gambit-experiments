
(define-library (github.com/FredericHamel/gambit-experimental test)

  (import (..))     ;; relative import of hello (preserves the version)
  (import (_test))  ;; for test-equal and test-error-tail
  (import (gambit)) ;; for lambda, with-output-to-string, and
                    ;; wrong-number-of-arguments-exception?

  (begin

    (define http-codes
      '((200 . "ok")
        (404 . "not found")
        (501 . "internal error")))

    (define (http-code->string code)
      (cond
        ((assq code http-codes)
         => (lambda (x) (list (number->string code) (cdr x))))
        (else
          (pp 'a)
          (list (string->number code)))))

    (define (set_http_status conn code)
      (let ((result (pipe code
                          (http-code->string)
                          (append-strings " "))))
        (println port: conn result)
        conn))

    (define (set_header conn field value)
      (let ((line (append-strings (list field (object->string value)) ": ")))
        (println port: conn line)
        conn))


    (define (set_content_type conn type)
      (println port: conn
               (append-strings (list "Content-Type" type) ": "))
      conn)


    (define (end_header conn)
      (newline conn)
      conn)

    (define (set_body conn body)
      (print port: conn body)
      conn)

    (define (send_resp conn resp)
      (pipe
        conn
        (set_header "Content-Length" (string-length resp))
        (end_header)
        (set_body resp)))

    (test-assert
      "Test 1"
      (pipe
        #f
        (not)))

    (test-assert
      "Test 2"
      (not
        (pipe 2 (< 2))))

    (test-assert
      "HTTP request building with pipe macro"

      (pipe
        (open-string)
        (set_http_status 200)
        (set_content_type "text/plain")
        (send_resp "Hello, world")
        (get-output-string)
        (string=? "200 ok\nContent-Type: text/plain\nContent-Length: 12\n\nHello, world")))))
