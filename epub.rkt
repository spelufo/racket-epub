#lang at-exp racket/base

(provide make-epub-files)

(require racket/path
         racket/port
         racket/system
         scribble/text)

(define (map/lines f l)
  (add-newlines (map f l)))

(define ($ cmd) (string-trim (with-output-to-string (lambda () (system cmd)))))

(define (mimetype file)
  (case (filename-extension file)
    [(#"xhtml" #"html") "application/xhtml+xml"]
    [(#"jpeg" #"jpg") "image/jpeg"]
    [(#"png") "image/png"]
    [(#"css") "text/css"]
    [(#"svg") "image/svg+xml"]
    [(#"pls") "application/pls+xml"]))
      
(define (file->id file)
  (let* ([id (string-replace file "/" "")]
         [id (string-replace id "." "")])
    id))

;; http://www.idpf.org/epub/301/spec/epub-ocf.html#sec-container-metainf-container.xml
(define (container . rootfiles)
   @`{<?xml version="1.0" encoding="UTF-8"?>
      <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
        <rootfiles>
          @,(map/lines (λ (rf) @`{<rootfile full-path="@,rf" media-type="application/oebps-package+xml" />}) rootfiles)
        </rootfiles>
      </container>})

;; http://www.idpf.org/epub/301/spec/epub-publications.html#sec-package-documents
;; http://www.idpf.org/epub/301/schema/package-30.rnc
(define (package-doc files spine-files title author lang)

  (define (metadata title author lang)
    @`{<metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
         <dc:identifier id="pub-id">@,${uuidgen}</dc:identifier>
         <dc:title>@|,title|</dc:title>
         <dc:language>@|,lang|</dc:language>
         <dc:date>@,${date --utc -I}</dc:date>
         <meta property="dcterms:modified">@,${date --utc -Iseconds}</meta>
         <dc:creator id="creator">@|,author|</dc:creator>
         <meta refines="#creator" property="role" scheme="marc:relators">aut</meta>
       </metadata>})

  (define (manifest files)    
    (define (item file)
      @`{<item id="@,(file->id file)" href="@,file" media-type="@,(mimetype file)" />})

    @`{<manifest>
         <item id="nav" href="nav.xhtml" properties="nav" media-type="application/xhtml+xml"/>
         @,(map/lines item files)
       </manifest>})

  (define (spine files)
    (define (itemref file [hidden #f])
      @`{<itemref idref="@,(file->id file)" linear="@,(if hidden "no" "yes")" />})

    @`{<spine>
         @,(map/lines itemref spine-files)
       </spine>})
  
  @`{<?xml version="1.0" encoding="UTF-8"?>
     <package version="3.0" xml:lang="en" xmlns="http://www.idpf.org/2007/opf" unique-identifier="pub-id">
       @,(metadata title author lang)
       @,(manifest files)
       @,(spine files)
     </package>})

(define (nav-doc spine-files)
  (define (nav-li file)
    @`{<li id="@,(file->id file)"><a href="@,file">@,file </a></li>})
  
  @`{<!DOCTYPE html>
     <html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
       <head><meta charset="utf-8" /><title>Table of Contents</title></head>
       <body>
         <nav epub:type="toc" id="toc">
           <h1 class="title">Table of Contents</h1>
           <ol>
             @,(map/lines nav-li spine-files)
           </ol>
         </nav>
       </body>
     </html>})



(define (make-mimetype)
  (with-output-to-file "mimetype"
    (λ () (display "application/epub+zip"))
    #:exists 'truncate))

(define (make-container)
  (unless (directory-exists? "META-INF")
    (make-directory "META-INF"))
  (with-output-to-file "./META-INF/container.xml"
                    (λ () (output (container "content.opf")))
                    #:exists 'truncate))

(define (make-package-doc files spine-files title author lang)
  (with-output-to-file "./content.opf"
   (λ () (output (package-doc files spine-files title author lang)))
   #:exists 'truncate))

(define (make-nav-doc spine-files)
  (with-output-to-file "./nav.xhtml"
    (λ () (output (nav-doc spine-files)))
    #:exists 'truncate))


(define (make-epub-files files spine-files title author lang)
  (make-mimetype)
  (make-container)
  (make-nav-doc spine-files)
  (make-package-doc files spine-files title author lang))
