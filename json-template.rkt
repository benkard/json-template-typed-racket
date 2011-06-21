;;; -*- mode: scheme; coding: utf-8 -*-
;;; Copyright 2011, Matthias Andreas Benkard.
#lang racket

(provide read-template
         formatters)

(define (template-string->chunks input meta-left meta-right)
  (let* ([meta-left-re   (regexp-quote meta-left)]
         [meta-right-re  (regexp-quote meta-right)]
         [re (regexp
              (string-append "(" meta-left-re ")|(" meta-right-re ")"))])
    (regexp-split re input)))

(define (flip-flop-map f1 f2 lst)
  "Like map, but alternate between f1 and f2 as the function to apply."
  (define (flip items)
    (match items
      ['()          '()]
      [(list* x xs) (cons (f1 x) (flop xs))]))
  (define (flop items)
    (match items
      ['()          '()]
      [(list* x xs) (cons (f2 x) (flip xs))]))
  (flip lst))

(define (classify-chunks chunks format-char)
  (flip-flop-map (λ (x) x)
                 (λ (x) (parse-directive x format-char))
                 chunks))

(define (parse-directive directive format-char)
  (match directive
    [(regexp #rx"^#")
     #f]
    [(regexp #px"^\\.section\\s+(.*)$" (list _ x))
     (list 'section x)]
    [(regexp #px"^\\.repeated\\s+section\\s+(.*)$" (list _ x))
     (list 'repeated-section x)]
    [#".end"
     (list 'end)]
    [#".or"
     (list 'or)]
    [(regexp #px"^\\.alternates\\s+with$")
     (list 'alternates-with)]
    [_
     (list* 'substitute (regexp-split (regexp-quote format-char)
                                      directive))]))
  
(define (remove-spurious-newlines-from-token-groups groups)
  (let ([last-was-directive? #f])
    (for/list ([group groups])
      (if last-was-directive?
          (begin
            (set! last-was-directive? (pair? group))
            (if (or (string? group) (bytes? group))
                (regexp-replace #rx"^\n" group "")
                group))
          (begin
            (set! last-was-directive? (pair? group))
            group)))))

(struct section
  (name
   body
   alternative)
  #:transparent)

(struct repeated-section section
  (alternates-with)
  #:transparent)

(struct substitution
  (name
   formatter
   arguments)
  #:transparent)

(define (parse-structure parsed-groups)
  (let loop ([parsed-groups parsed-groups]
             [clauses '()])
    (if (or (null? parsed-groups)
            (and (pair? (car parsed-groups))
                 (memq (caar parsed-groups) '(end or alternates-with))))
        (values (reverse clauses)
                (and (pair? parsed-groups) (caar parsed-groups))
                (if (pair? parsed-groups) (cdr parsed-groups) '()))
        (match (car parsed-groups)
          [(list 'section x)
           (let-values ([(stuff reason rest)
                         (parse-structure (cdr parsed-groups))])
             (case reason
               [(or)
                (let-values ([(stuff2 _ rest2)
                              (parse-structure rest)])
                  (loop rest2 (cons (section x stuff stuff2) clauses)))]
               [(end)
                (loop rest (cons (section x stuff #f) clauses))]))]
          [(list 'repeated-section x)
           (let inner-loop ([subsections '()]
                            [rest (cdr parsed-groups)])
             (let-values ([(stuff reason new-rest)
                           (parse-structure rest)])
               (when (false? reason)
                 (error "Premature end of file."))
               (if (eq? reason 'end)
                   (let inner-inner-loop
                       ([subsections (cons (cons 'end stuff) subsections)]
                        [alternative #f]
                        [alternates-with #f])
                     (if (null? (cdr subsections))
                         (loop new-rest
                               (cons (repeated-section x
                                                       (cdar subsections)
                                                       alternative
                                                       alternates-with)
                                     clauses))
                         (case (caadr subsections)
                           [(alternates-with)
                            (inner-inner-loop (cdr subsections)
                                              alternative
                                              (cdar subsections))]
                           [(or)
                            (inner-inner-loop (cdr subsections)
                                              (cdar subsections)
                                              alternates-with)]
                           [else
                            (error "Oh no, I don't know what I'm doing here!  Subsections:" subsections)])))
                   (inner-loop (cons (cons reason stuff) subsections) new-rest))))]
          [(list 'substitute x)
           (loop (cdr parsed-groups)
                 (cons (substitution x #f '()) clauses))]
          [(list 'substitute x y arg ...)
           (loop (cdr parsed-groups)
                 (cons (substitution x y arg) clauses))]
          [x
           (loop (cdr parsed-groups)
                 (cons x clauses))]))))

(define (parse-structure* x)
  (let-values ([(stuff reason rest) (parse-structure x)])
    stuff))

(struct template (expander)
  #:property prop:procedure (struct-field-index expander))

(define (read-template #:meta-left         [meta-left "{"]
                       #:meta-right        [meta-right "}"]
                       #:default-formatter [default-formatter "raw"]
                       #:format-char       [format-char "|"])
  (let ([template-data
         (parse-structure*
          (remove-spurious-newlines-from-token-groups
           (classify-chunks (template-string->chunks (current-input-port)
                                                     meta-left meta-right)
                            format-char)))])
    (template
     (λ (context)
       (expand-template template-data (list context) default-formatter)))))

(define (name->path bytename)
  (let ([name (bytes->string/utf-8 bytename)])
    (if (string=? name "@")
        '()
        (regexp-split #rx"\\." name))))

(define (resolve-path stack path)
  (if (null? stack)
      #f
      (let-values ([(value success?)
                    (resolve-path-in-object (car stack) path)])
        (if success?
            value
            (resolve-path (cdr stack) path)))))

(define (resolve-path-in-object context path)
  (let ([nothing (gensym)])
    (cond [(null? path)
           (values context #t)]
          [(dict? context)
           (let ([y (dict-ref context
                              (car path)
                              (λ ()
                                (dict-ref context
                                          (string->symbol (car path))
                                          nothing)))])
             (if (eq? y nothing)
                 (values #f #f)
                 (resolve-path-in-object y (cdr path))))]
          [else
           (values #f #f)])))

(define (find-formatter name)
  (cdr (assoc (if (string? name)
                  name
                  (bytes->string/utf-8 name))
              (formatters))))

(define (expand-template template stack default-formatter)
  (for ([thing template])
    (match thing
      [(repeated-section name body alternative alternates-with)
       (let ([context (resolve-path stack (name->path name))])
         (if (or (false? context)
                 (null? (sequence->list context)))
             (when alternative
               (expand-template alternative
                                (cons context stack)
                                default-formatter))
             (let ([first-iteration? #t])
               (for ([value context])
                 (when alternates-with
                   (if first-iteration?
                       (set! first-iteration? #f)
                       (expand-template alternates-with
                                        stack
                                        default-formatter)))
                 (expand-template body
                                  (cons value stack)
                                  default-formatter)))))]
      [(section name body alternative)
       (let ([context (resolve-path stack (name->path name))])
         (if context
             (expand-template body
                              (cons context stack)
                              default-formatter)
             (when alternative
               (expand-template alternative
                                (cons context stack)
                                default-formatter))))]
      [(substitution name formatter args)
       (display ((find-formatter (or formatter default-formatter))
                 (resolve-path stack (name->path name))))]
      [_
       (display thing)])))



(define (make-escaper replacements)
  (let* ([escapees  (map car replacements)]
         [escapings (map cdr replacements)]
         [re        (regexp
                     (string-append "^(.*?)"
                                    "(?:("
                                    (foldl (λ (x acc)
                                             (string-append acc
                                                            ")|("
                                                            (regexp-quote (string x))))
                                           (regexp-quote (string (car escapees)))
                                           (cdr escapees))
                                    "))"
                                    "|$"))])
    (λ (thing)
        (with-output-to-string
         (λ ()
           (with-input-from-string
            (if (string? thing)
                thing
                (format "~a" thing))
            (λ ()
              (let loop ()
                (unless (eof-object? (peek-byte))
                  (match-let ([(list* _ raw-text escapee-matches)
                               (regexp-match re (current-input-port))])
                    (when raw-text
                      (display raw-text))
                    (for ([x escapee-matches]
                          [y escapings])
                      (when x
                        (display y)))
                    (loop)))))))))))


(define (escape-for-uri thing)
  (with-output-to-string
    (λ ()
      (for ([char (in-string (if (string? thing)
                                 thing
                                 (format "~a" thing)))])
        (let ((cnum (char->integer char)))
          (if (or (<= (char->integer #\A) cnum (char->integer #\Z))
                  (<= (char->integer #\a) cnum (char->integer #\z))
                  (<= (char->integer #\0) cnum (char->integer #\9))
                  (member char
                          '(#\$ #\- #\_ #\. #\+ #\! #\* #\( #\) #\')))
              (display char)
              ;; FIXME: This assumes that (< cnum 256).
              ;; Maybe we should interpret the data as a byte string
              ;; rather than as a string.  W3C says we ought to use
              ;; UTF-8 encoding, which is consistent with the Racket
              ;; default encoding:
              ;;
              ;;  http://www.w3.org/International/O-URL-code.html
              (if (< cnum 16)
                  (printf "%0~x" cnum)
                  (printf "%~x"  cnum))))))))


(define formatters
  (make-parameter
   `(("html"            . ,(make-escaper '((#\< . "&#60;")
                                           (#\> . "&#62;")
                                           (#\& . "&#38;"))))
     ("html-attr-value" . ,(make-escaper '((#\< . "&#60;")
                                           (#\> . "&#62;")
                                           (#\& . "&#38;")
                                           (#\' . "&#39;")
                                           (#\" . "&#34;"))))
     ("url-param-value" . ,escape-for-uri)
     ("raw"             . ,(λ (x) x)))))


;;#;
(let ([template (with-input-from-string
                 #<<EOF
<h1>{title|html}</h1>
{.section people}
<ul>
{.repeated section @}
  <li>{name} ({age} years)</li>
{.end}
</ul>
{.or}
<p>No one's registered.</p>
{.end}
EOF
                  (λ () (read-template)))])
  (template '((title . "<Registered People>")
              (people .
                      (((name . "Nathalie") (age . 24))
                       ((name . "Heinrich") (age . 28))
                       ((name . "Hans")     (age . 25)))))))
