#!/usr/bin/racket
#lang racket

(require "epub-files.rkt")
(require file/zip)

(define lang "en")
(define output-file #f)

(define-values (title author spine-files files)
  (command-line
   #:once-each
   [("-l") l "language" (set! lang l)]
   [("-o") o "output epub file" (set! output-file o)]
   [("-v") "verbose" (zip-verbose #t)]
   #:args (title author file-list spine-file-list)
   (values
    title
    author
    (for/list ([l (in-lines (open-input-file spine-file-list))]) l)
    (for/list ([l (in-lines (open-input-file file-list))]) l))))


(with-output-to-file "mimetype"
  (λ () (display "application/epub+zip"))
  #:exists 'truncate)

(make-container)

(make-nav-doc spine-files)

(make-package-doc files spine-files title author lang)

#;(and output-file
     (begin
       (and (file-exists? output-file)
            (delete-file output-file))
       (apply zip output-file
              "mimetype"
              "META-INF"
              "content.opf"
              files)))
;    #:path-prefix #t))