#lang setup/infotab
(define name "JSON Template for Racket")
(define blurb
  '(p ()
      "A Typed Racket implementation of "
      (a ((href "http://json-template.googlecode.com/svn/trunk/doc/Introducing-JSON-Template.html"))
         "JSON Template")
      ", a minimalistic, yet powerful, template language."))
(define categories '(misc))
(define version "1.0.4")
(define can-be-loadded-with 'all)
(define primary-file (list "main.rkt"))
(define homepage "http://matthias.benkard.de/software/json-template-for-racket")
(define scribblings '(("manual.scrbl" ())))

(define release-notes
  '(p ()
      "Fix primary file setting."))

#;
(define required-core-version
  ...)
