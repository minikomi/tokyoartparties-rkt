#lang racket

(require sxml
         racket/date
         net/http-client
         web-server/servlet
         web-server/servlet-env
         )

(define-struct event
  (title
   day
   area
   address
   start
   finish
   description
   link
   image
   ) #:transparent)

(define xmlurl "http://www.tokyoartbeat.com/list/event_type_misc_party.en.xml")

(define (process-event ev)
  (define title
    (let ([v ((sxpath `(Name *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define day
    (let ([v ((sxpath `(Party @ date *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define area
    (let ([v ((sxpath `(Venue Area *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define address
    (let ([v ((sxpath `(Venue Address *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define start
    (let ([v ((sxpath `(Party @ start *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define finish
    (let ([v ((sxpath `(Party @ end *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define description
    (let ([v ((sxpath `(Description *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define link
    (let ([v ((sxpath `(@ href *text*)) ev)])
      (if (empty? v)
          ""
          (first v))))

  (define image
    (let ([v ((sxpath `(Image @ src *text*)) ev)])
      (if (empty? v)
          ""
          (last v))))

  (event title day area address start finish description link image))

(define (layout-event ev)
  `(div
    (h3 ,(event-title ev))
    (h4 ,(string-append (event-start ev) " - " (event-finish ev)))
    (h5 ,(event-area ev))
    (h6 ((class "address")) ,(event-address ev))
    (p ,(when (not (eq? "" (event-image ev)))
          `(img [(src ,(event-image ev))]))
       ,(event-description ev))
    ))

(define (handler req)

  (define partysxml (sxml:document xmlurl))

  (define raw-events ((sxpath '(// Event)) partysxml))

  (define party-events
    (map process-event
         (filter
          (λ (ev)
            (define party ((sxpath '(// Party)) ev))
            (not (empty? party)))
          raw-events)))

  (define days-hash
    (for/fold ([days (hash)])
              ([ev party-events])
      (hash-update days (event-day ev)
                   (λ (evs) (cons ev evs))
                   (list))))

  (define cd (current-date))

  (define today (~a
                 (date-year cd)
                 "-"
                 (~r (date-month cd) #:min-width 2 #:pad-string "0")
                 "-"
                 (~r (date-day cd) #:min-width 2 #:pad-string "0")))

  (define days-later-than-today-in-order
    (sort 
     (filter 
      (λ (day) (string<=? today day)) 
      (hash-keys days-hash)) 
     string<?))

  (response/xexpr
   `(html (head
           (title "Tokyo art events")
           (link ((rel "stylesheet") (href "/artparties.css"))))
     (body
      (h1 "Tokyo Art Parties")
      ,@(map
         (λ (day)
           `(div ([class "day"])
             (h2 ,day)
             ,@(map (lambda (ev) (layout-event ev))
                    (sort (hash-ref days-hash day)
                          (λ (ev1 ev2) (string<? (event-area ev1) (event-area ev2)))
                          ))))
         days-later-than-today-in-order)))))

(serve/servlet
 #:launch-browser? #f
 #:listen-ip       "0.0.0.0"
 #:port            9889
 #:servlet-path    "/"
 #:extra-files-paths (list (build-path "./static"))
 handler)
