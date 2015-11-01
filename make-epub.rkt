#lang racket

(require "epub-files.rkt")
(require )
(define lang "en")

(define-values (title author spine-files output-file)
  (command-line
   #:once-each
   [("-l") l "set the language" (set! lang l)]
   #:args (title author spine-file output-file)
   (values
    title
    author
    (for/list ([l (in-lines (open-input-file spine-file))]) l)
    output-file)))


(make-container)
(make-package-doc spine-files title author lang)