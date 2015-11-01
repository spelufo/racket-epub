#lang at-exp racket

(provide make-container make-package-doc)

(require scribble/text)
(require libuuid)

;(write "application/epub+zip" (open-output-file "./mimetype")

(define (map/lines f l)
  (add-newlines (map f l)))

;; http://www.idpf.org/epub/301/spec/epub-ocf.html#sec-container-metainf-container.xml
(define (container . rootfiles)
   @`{<?xml version="1.0"?>
      <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
        <rootfiles>
          @,(map/lines (λ (rf) @`{<rootfile full-path="@,rf" media-type="application/oebps-package+xml" />}) rootfiles)
        </rootfiles>
      </container>})

;; http://www.idpf.org/epub/301/spec/epub-publications.html#sec-package-documents
;; http://www.idpf.org/epub/301/schema/package-30.rnc
(define (package-doc files title author lang)

  (define (file->id file) file)

  (define (metadata title author lang)
    @`{<metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
         <dc:identifier id="pub-id">@,(uuid-generate)</dc:identifier>
         <dc:title>@|,title|</dc:title>
         <dc:language>@|,lang|</dc:language>
         <dc:date>@,(let* ([d (seconds->date (current-seconds))]
                           [year (date-year d)]
                           [month (date-month d)]
                           [day (date-day d)])
                      (format "~a-~a-~a" year month day))</dc:date>
         <dc:creator id="creator">@|,author|</dc:creator>
         <meta refines="#creator" property="role" scheme="marc:relators">aut</meta>
       </metadata>})

  (define (manifest files)    
    (define (item file)
      (define (mimetype file)
        (case (filename-extension file)
          [(#"xhtml") "application/xhtml+xml"]
          [(#"jpeg" #"jpg") "image/jpeg"]
          [(#"png") "image/png"]
          [(#"css") "text/css"]
          [(#"svg") "image/svg+xml"]
          [(#"pls") "application/pls+xml"]))
      
      @`{<item id="@,(file->id file)" href="@,file" media-type="@,(mimetype file)" />})

    @`{<manifest>
         @,(map/lines item files)
       </manifest>})

  (define (spine files)
    (define (itemref file [hidden #f])
      @`{<itemref idref="@,(file->id file)" linear="@,(if hidden "no" "yes")" />})

    @`{<spine>
         @,(map/lines itemref files)
       </spine>})
  
  @`{<?xml version="1.0"?>
     <package version="3.0" xml:lang="en" xmlns="http://www.idpf.org/2007/opf" unique-identifier="pub-id">
       @,(metadata title author lang)
       @,(manifest files)
       @,(spine files)
     </package>})



(define (make-container)
  (unless (directory-exists? "META-INF")
    (make-directory "META-INF"))
  (output (container "content.opf")
          (open-output-file "./META-INF/container.xml" #:exists 'truncate)))

(define (make-package-doc files title author lang)
  (output (package-doc files title author lang)
          (open-output-file "./content.opf"  #:exists 'truncate)))