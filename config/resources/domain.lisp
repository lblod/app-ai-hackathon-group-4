(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)
(defparameter sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *fetch-all-types-in-construct-queries* t)

;; reading in the domain.json

(define-resource aanduidingsobject ()
  :class (s-prefix "cidoc:E22_Man-Made_Object")
  :properties `((:title :string ,(s-prefix "cidoc:P102_has_title"))
                (:description :string ,(s-prefix "cidoc:P3_has_note"))
                (:identifier :string ,(s-prefix "dct:identifier"))
                ;; should actually be instance of the type http://www.cidoc-crm.org/cidoc-crm/E87_Curation_Activity
                (:style :string ,(s-prefix "ext:artStyle"))
                ;; should actually be instance of type http://www.cidoc-crm.org/cidoc-crm/E65_Creation
                (:creation :datetime ,(s-prefix "dct:created")))
  :resource-base (s-url "http://data.lblod.info/id/aanduidingsobjecten/")
  ;; address and typology should be has many in theory i guess but shortcutting for hackathon
  :has-one `((address :via ,(s-prefix "cidoc:P53_has_former_or_current_location")
                              :as "address")
             (concept :via ,(s-prefix "cidoc:P2_has_type")
                       :as "typology"))
  :has-many `((besluit :via ,(s-prefix "ext:relatedDecision")
                    :as "decisions"))
  :features '(include-uri)
  :on-path "aanduidingsobjects")


(define-resource location ()
  :class (s-prefix "prov:Location")
  :resource-base (s-url "http://data.lblod.info/id/locaties/")
  :features '(include-uri)
  :on-path "locations")

(define-resource address (location)
  :class (s-prefix "locn:Address")
  :properties `((:municipality-name :string ,(s-prefix "adres:gemeentenaam"))
                (:province :string ,(s-prefix "locn:adminUnitL2"))
                (:street-name :string ,(s-prefix "locn:thoroughfare"))
                (:postal-code :string ,(s-prefix "locn:postCode"))
                (:bus-number :string ,(s-prefix "adres:Adresvoorstelling.busnummer")))
  :has-many `((aanduidingsobject :via ,(s-prefix "cidoc:P53_has_former_or_current_location")
                          :inverse t
                          :as "references"))
  :resource-base (s-url "http://data.lblod.info/id/address-representations/")
  :features '(include-uri)
  :on-path "addresses")

(define-resource besluit ()
  :class (s-prefix "besluit:Besluit")
  :properties `(
    (:title :string ,(s-prefix "dct:title"))
    ;; as the api has different files that may have the same besluit uri...
    (:besluitUri :url ,(s-prefix "ext:oeBesluitUri"))
    (:download :url ,(s-prefix "ext:dowloadLink")))
  :resource-base (s-url "http://data.lblod.info/id/besluiten/")
  :has-one `((concept :via ,(s-prefix "dct:type")
                        :as "kind")
             (aanduidingsobject :via ,(s-prefix "ext:relatedDecision")
                        :inverse t
                        :as "aanduidingsobject"))
  ;; the uri is the location of the file
  :features '(include-uri)
  :on-path "besluits")

;; model says to use https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-object/#Type Entiteit but this seems too scary with the space
(define-resource concept ()
  :class (s-prefix "skos:Concept")
  :properties `((:title :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/concepten/")
  :features '(include-uri)
  :on-path "concepts")

(define-resource annotation ()
  :class (s-prefix "oa:Annotation")
  :properties `((:body :string ,(s-prefix "oa:hasBody"))
                (:created :datetime ,(s-prefix "dct:created"))
                ;; string for now, but we could make this much more involved as an instance, describing the model and the specific sections used etc
                (:motivation :string ,(s-prefix "oa:motivatedBy")))
  :has-one `((annotation-type :via ,(s-prefix "dct:type")
                               :as "annotation-type")
              (agent :via ,(s-prefix "dct:creator")
                                :as "creator")
              (besluit :via ,(s-prefix "oa:hasTarget")
                                 :as "about"))
  :has-many `((annotation-feedback :via ,(s-prefix "ext:hasFeedback")
                                   :as "feedback"))
  :resource-base (s-url "http://data.lblod.info/id/annotations/")
  :features '(include-uri)
  :on-path "annotations")

(define-resource annotation-feedback ()
  :class (s-prefix "ext:AnnotationFeedback")
  :properties `((:feedback :string ,(s-prefix "ext:feedback"))
                ;; simply +1 or -1 for now, see readme for explanation on shortcut
                (:rating :number ,(s-prefix "ext:rating"))
                (:created :datetime ,(s-prefix "dct:created")))
  :has-one `((annotation-feedback :via ,(s-prefix "ext:hasFeedback")
                                  :inverse t
                                  :as "annotation"))
  :resource-base (s-url "http://data.lblod.info/id/annotation-feedbacks/")
  :features '(include-uri)
  :on-path "annotation-feedbacks")

(define-resource annotation-type (concept)
  :class (s-prefix "ext:AnnotationType")
  :properties `((:title :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/annotation-types/")
  :features '(include-uri)
  :on-path "annotation-types")

(define-resource agent ()
  :class (s-prefix "foaf:Agent")
  :properties `((:name :string ,(s-prefix "foaf:name")))
  :has-many `((annotation :via ,(s-prefix "dct:creator")
                    :inverse t
                    :as "creator"))
  :resource-base (s-url "http://data.lblod.info/agents/")
  :features `(include-uri)
  :on-path "agents")
