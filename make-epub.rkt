#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/path racket/system "epub.rkt")

(define (html-file? f)
  (member (filename-extension f) '(#"html" #"xhtml")))

(define (file->lines f)
  (for/list ([l (in-lines (open-input-file f))]) l))

(define lang "en")
(define keep #f)
(define author (getenv "USER"))
(define spine-files #f)
(define output-file #f)

(define-values (title files)
  (command-line
   #:once-each
   [("-o") o "output epub file" (set! output-file o)]
   [("-k") k "keep all generated epub files" (set! keep #t)]
   [("-a") a "author" (set! author a)]
   [("-l") l "language" (set! lang l)]
   [("-s") s "spine file" (set! spine-files (file->lines s))]
   #:args
   (title . files) (values title files)))


(current-directory output-dir)

(unless spine-files
  (set! spine-files (filter html-file? files)))

(make-epub-files files spine-files title author lang)

(when output-file
  (system (string-append "zip -X -Z store " output-file " mimetype"))
  (system (apply string-append "zip -X -r " output-file " META-INF content.opf nav.xhtml " files)))

(unless keep
  (map delete-file '("mimetype" "META-INF/container.xml" "content.opf" "nav.xhtml"))
  (delete-directory "META-INF"))
