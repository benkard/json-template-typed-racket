#lang typed/racket
;;; -*- mode: scheme; coding: utf-8 -*-
;;; Copyright 2011, Matthias Andreas Benkard.

(require/typed racket
               [regexp-split (Regexp String -> (Listof String))]
               ;;[sequence->list (All (a) (Sequenceof a) -> (Listof a))]
               [sequence->list (All (a) Any -> (Listof a))]  ;FIXME
               [regexp-replace (Regexp String String -> String)]
               [sequence? (Any -> Boolean)]
               [with-input-from-string (All (a) (String (-> a) -> a))]
               [dict-ref (Any Any Any -> Any)]
               [dict? (Any -> Boolean)])

(provide: [make-template     (String -> Template)]
          [formatters        (Parameterof (Listof (Pairof String (String -> String))))]
          [meta-left         (Parameterof String)]
          [meta-right        (Parameterof String)]
          [default-formatter (Parameterof String)]
          [format-char       (Parameterof String)])
(provide Template)


(define-type Template (Any -> Void))

(define meta-left         (make-parameter "{"))
(define meta-right        (make-parameter "}"))
(define default-formatter (make-parameter "raw"))
(define format-char       (make-parameter "|"))

(define: (template-string->chunks [input : String]
                                  [meta-left : String]
                                  [meta-right : String]) : (Listof String)
  (let* ([meta-left-re   (regexp-quote meta-left)]
         [meta-right-re  (regexp-quote meta-right)]
         [re (regexp
              (string-append "(" meta-left-re ")|(" meta-right-re ")"))])
    (regexp-split re input)))

(: flip-flop-map
   (All (a b)
        (a -> b) (a -> b) (Listof a) -> (Listof b)))
(define (flip-flop-map f1 f2 lst)
  "Like map, but alternate between f1 and f2 as the function to apply."
  (define: (flip [items : (Listof a)]) : (Listof b)
    (match items
      ['()          '()]
      [(list* x xs) (cons (f1 x) (flop xs))]))
  (define: (flop [items : (Listof a)]) : (Listof b)
    (match items
      ['()          '()]
      [(list* x xs) (cons (f2 x) (flip xs))]))
  (flip lst))


(struct: directive
  ([type : Symbol]
   [arguments : (Listof String)])
  #:transparent)
(define-type Group (U String directive))

(define: (classify-chunks [chunks : (Listof String)]
                          [format-char : String]) : (Listof Group)
  (flip-flop-map (λ: ([x : String]) x)
                 (λ: ([x : String]) (parse-directive x format-char))
                 chunks))

(define: (parse-directive [text : String]
                          [format-char : String]) : directive
  (match text
    [(regexp #px"^\\.section\\s+(.*)$" (list _ x))
     (case x
       [(#f) (error "Syntax error: Expected section name after \".section\"")]
       [else (directive 'section (list x))])]
    [(regexp #px"^\\.repeated\\s+section\\s+(.*)$" (list _ x))
     (case x
       [(#f) (error "Syntax error: Expected section name after \".repeated section\"")]
       [else (directive 'repeated-section (list x))])]
    [".end"
     (directive 'end '())]
    [".or"
     (directive 'or '())]
    [(regexp #px"^\\.alternates\\s+with$")
     (directive 'alternates-with '())]
    [_
     (directive 'substitute
                (regexp-split (regexp (regexp-quote format-char)) text))]))

(define: (remove-spurious-newlines-from-token-groups [groups : (Listof Group)])
  : (Listof Group)
  (let: ([last-was-directive? : Boolean #f])
    (for/list ([group groups])
      (if last-was-directive?
          (begin
            (set! last-was-directive? (directive? group))
            (if (or (string? group) (bytes? group))
                (regexp-replace #rx"^\n" group "")
                group))
          (begin
            (set! last-was-directive? (directive? group))
            group)))))

(define-type Part (U section substitution String))

(struct: section
  ([name : String]
   [body : (Listof Part)]
   [alternative : (Listof Part)])
  #:transparent)

(struct: repeated-section section
  ([alternates-with : (Listof Part)])
  #:transparent)

(struct: substitution
  ([name : String]
   [formatter : (U String #f)]
   [arguments : (Listof String)])
  #:transparent)

(define: (parse-structure [parsed-groups : (Listof Group)])
  : (values (Listof Part) (U Symbol False) (Listof Group))
  (let: loop : (values (Listof Part) (U Symbol False) (Listof Group))
        ([parsed-groups : (Listof Group) parsed-groups]
         [clauses       : (Listof Part)  '()])
    (if (or (null? parsed-groups)
            (and (directive? (car parsed-groups))
                 (memq (directive-type (car parsed-groups))
                       '(end or alternates-with))))
        (values (reverse clauses)
                (if (null? parsed-groups)
                    #f
                    (let ([first-group (car parsed-groups)])
                      (if (directive? first-group) (directive-type first-group) #f)))
                (if (pair? parsed-groups) (cdr parsed-groups) '()))
        (match (car parsed-groups)
          [(directive 'section (list x))
           (let-values ([(stuff reason rest)
                         (parse-structure (cdr parsed-groups))])
             (case reason
               [(or)
                (let-values ([(stuff2 _ rest2)
                              (parse-structure rest)])
                  (loop rest2 (cons (section x stuff stuff2) clauses)))]
               [(end)
                (loop rest (cons (section x stuff '()) clauses))]
               [else (error "Values of beta will give rise to dom!" reason)]))]
          [(directive 'repeated-section (list x))
           (let: inner-loop : (values (Listof Part) (U Symbol False) (Listof Group))
                 ([subsections% : (Listof (Pairof (U Symbol False) (Listof Part))) '()]
                  [rest         : (Listof Group)
                                (cdr parsed-groups)])
             (let-values ([(stuff reason new-rest)
                           (parse-structure rest)])
               (when (false? reason)
                 (error "Premature end of file."))
               (if (eq? reason 'end)
                   (let: inner-inner-loop : (values (Listof Part) (U Symbol False) (Listof Group))
                         ([subsections     : (Listof (Pairof (U Symbol False)
                                                             (Listof Part)))
                                           (let: ([tmp : (Pairof (U Symbol False)
                                                                 (Listof Part))
                                                       (cons 'end stuff)])
                                             (cons tmp subsections%))]
                          [alternative     : (Listof Part) (list)]
                          [alternates-with : (Listof Part) (list)])
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
                   (inner-loop (cons (ann (cons reason stuff)
                                          (Pairof (U Symbol False) (Listof Part)))
                                     subsections%)
                               new-rest))))]
          [(directive 'substitute (list x))
           (loop (cdr parsed-groups)
                 (cons (substitution x #f '()) clauses))]
          [(directive 'substitute (list x y arg ...))
           (let ([d (car parsed-groups)])
             (with-asserts ([d directive?])
               (loop (cdr parsed-groups)
                     (cons (substitution x y (cddr (directive-arguments d)))
                           ;"arg" doesn't work because Typed Racket thinks it's a (Listof Any).
                           clauses))))]
          [x
           (if (string? x)
               (loop (cdr parsed-groups)
                     (cons x clauses))
               (error "Expected a string here."))]))))

(define: (parse-structure* [x : (Listof Group)]) : (Listof Part)
  (let-values ([(stuff reason rest) (parse-structure x)])
    stuff))

;;(struct: template ([expander : (Any -> Void)])
;;  #:property prop:procedure (struct-field-index expander))


(define (make-template input-string)
  (let ([template-data
         (parse-structure*
          (remove-spurious-newlines-from-token-groups
           (classify-chunks (template-string->chunks input-string
                                                     (meta-left)
                                                     (meta-right))
                            (format-char))))])
    (let ([default-formatter% (default-formatter)])
      (λ (context)
        (expand-template template-data (list context) default-formatter%)))))

(define: (name->path [name : String]) : (Listof String)
  (if (string=? name "@")
      '()
      (regexp-split #rx"\\." name)))

(define: (resolve-path [stack : (Listof Any)] [path : (Listof String)]) : Any
  (if (null? stack)
      #f
      (let-values ([(value success?)
                    (resolve-path-in-object (car stack) path)])
        (if success?
            value
            (resolve-path (cdr stack) path)))))

(define: (resolve-path-in-object [context : Any] [path : (Listof String)]) : (values Any Boolean)
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

(define: (find-formatter [name : String]) : (String -> String)
  (let ([formatter (assoc name (formatters))])
    (cdr (if formatter
             formatter
             (error "Formatter \"~a\" not found" name)))))

(define: (expand-template [template : (Listof Part)]
                          [stack : (Listof Any)]
                          [default-formatter : String]) : Void
  (for ([thing template])
    (match thing
      [(repeated-section name body alternative alternates-with)
       (let ([context (resolve-path stack (name->path name))])
         (if (or (false? context)
                 (null? context))
             (when alternative
               (expand-template alternative
                                (cons context stack)
                                default-formatter))
             (let: ([first-iteration? : Boolean #t])
               (for ([value (in-list context)])
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
                 (format "~a" (resolve-path stack (name->path name)))))]
      [_
       (display thing)])))



(define: (make-escaper [replacements : (Listof (Pairof Char String))]) : (String -> String)
  (let* ([escapees  (map (inst car Char String) replacements)]
         [escapings (map (inst cdr Char String) replacements)]
         [re        (regexp
                     (string-append "^(.*?)"
                                    "(?:("
                                    (foldl (λ: ([x : Char]
                                                [acc : String])
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
            (let: loop : Void ()
                  (unless (eof-object? (peek-byte))
                    (match-let ([(list* _ raw-text escapee-matches)
                                 (regexp-match re (current-input-port))])
                      (when raw-text
                        (display raw-text))
                      (for ([x (in-list escapee-matches)]
                            [y (in-list escapings)])
                        (when x
                          (display y)))
                      (loop)))))))))))


(define: (escape-for-uri [thing : String]) : String
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
     ("raw"             . ,(λ: ([x : String]) x)))))


#;
(let* ([template-string #<<EOF
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
                        ]
       [template (make-template template-string)])
  (template '((title . "<Registered People>")
              (people .
                      (((name . "Nathalie") (age . 24))
                       ((name . "Heinrich") (age . 28))
                       ((name . "Hans")     (age . 25)))))))
