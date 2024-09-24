;;;;;;;;;;;;;;;;;;;
;;; delta messenger
; (in-package :delta-messenger)

; (setf *delta-handlers* nil)
; (add-delta-logger)
; (add-delta-messenger "http://deltanotifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
(setf *backend* "http://virtuoso:8890/sparql"
      ;; (list "http://triplestore:8890/sparql"
      ;;       "http://triplestore1:8890/sparql"
      ;;       "http://triplestore2:8890/sparql"
      ;;       "http://triplestore3:8890/sparql"
      ;;       )
      )

(in-package :server)
(setf *log-incoming-requests-p* t)

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

(type-cache::add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")

(define-prefixes
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :cidoc "http://www.cidoc-crm.org/cidoc-crm/"
  :prov "http://www.w3.org/ns/prov#"
  :dct "http://purl.org/dc/terms/"
  :locn "http://www.w3.org/ns/locn#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :adres "http://data.vlaanderen.be/ns/adres#"
  :oa "http://www.w3.org/ns/oa#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :foaf "http://xmlns.com/foaf/0.1/"
)

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("cidoc:E22_Man-Made_Object" -> _)
  ("prov:Location" -> _)
  ("locn:Address" -> _)
  ("besluit:Besluit" -> _)
  ("skos:Concept" -> _)
  ("oa:Annotation" -> _)
  ("ext:AnnotationFeedback" -> _)
  ("ext:AnnotationType" -> _)
  ("foaf:Agent" -> _))

(supply-allowed-group "public")

(grant (read write)
       :to-graph public
       :for-allowed-group "public")

