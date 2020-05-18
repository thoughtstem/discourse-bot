#lang racket

(provide discourse-key
	 get-topic
	 get-post
	 links
	 make-post-on-topic!)

(require json)

(define (try-to-find-discourse-key)
  (define usual-place (build-path 
			(current-directory)
			".discourse-key"))
  (if (file-exists? usual-place)
      (begin
	(string-trim (file->string usual-place)))
      #f))

(define discourse-key (make-parameter (try-to-find-discourse-key)))

(define (rubyify s)
  (string->symbol
    (string-replace
      (symbol->string s) 
      "-"
      "_")))


(define (get ns id)
  (when (not discourse-key)
    (error "No API Key set.  You must set your (discourse-key) parameter." ))
  (string->jsexpr
    (with-output-to-string
      (thunk*
	(system
	  (~a
	    "curl -X GET 'https://forum.metacoders.org/"ns"/"id".json' -H 'Content-Type: multipart/form-data;' -H 'Api-Key: "
	    (discourse-key)
	    "' -H 'Api-Username: stephen' "))))))

(define (post! ns json)
  (when (not discourse-key)
    (error "No API Key set.  You must set your (discourse-key) parameter." ))
  (string->jsexpr
    (with-output-to-string
      (thunk*
	(system
	  (~a
	    "curl -X POST 'https://forum.metacoders.org/"ns".json' -H 'Content-Type: application/json;' -H 'Api-Key: " (discourse-key) "'" 
	    " -H 'Api-Username: stephen' "
	    " --data '" (jsexpr->string json) "'"
	    ))))))


(define-syntax-rule (define-hash-function name)
		    (begin
		      (provide name)
		      (define (name t)
			(hash-ref t
				  (rubyify 'name)))))



;TOPICS

(define (get-topic n)
  (get "t" n)
  )

(define (topic-url->id url)
  (last (string-split url "/")))

(define-hash-function title)
(define-hash-function fancy-title)
(define-hash-function id)
(define-hash-function post-stream)

;END TOPICS


;POST STREAMS?

(define-hash-function posts)

;END POST STREAMS


;POSTS

(define (get-post n)
  (get "posts" n))

(define (make-post-on-topic! topic-url message)
  (when (string-contains? message "'")
    (error "You can't post messages with apostrophes.  Want that feature?  Update this library to not use curl."))
  
  (define json
    (hash
      'topic_id (topic-url->id topic-url)
      'raw message
      ))
  (post!
    "posts"
    json))

(define-hash-function cooked)
(define-hash-function link-counts)
(define-hash-function raw)

(define (links p)
  (regexp-match
    #px"http[s]*://\\S*"
    (raw p)))

;END POSTS

;LINKS

(define-hash-function url)

;END LINKS
