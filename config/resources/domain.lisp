(in-package :mu-cl-resources)

(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)

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
  :resource-base (s-url "http://data.lblod.info/id/fracties/")
  ;; address and typology should be has many in theory i guess but shortcutting for hackathon
  :has-one `((address :via ,(s-prefix "cidoc:P53_has_former_or_current_location")
                              :as "address")
             (concept :via ,(s-prefix "cidoc:P2_has_type")
                       :as "typology"))
  :features '(include-uri)
  :on-path "aanduidingsobjects")


(define-resource location ()
  :class (s-prefix "prov:Location"))

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
  :on-path "address-representations")

(define-resource besluit ()
  :class (s-prefix "besluit:Besluit")
  :properties `((:title :string ,(s-prefix "dct:title")))
  :resource-base (s-url "http://data.lblod.info/id/besluiten/")
  :has-one `((concept :via ,(s-prefix "dct:type")
                        :as "kind"))
  ;; the uri is the location of the file
  :features '(include-uri)
  :on-path "besluiten")

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
  :resource-base (s-url "http://data.lblod.info/id/annotations/")
  :features '(include-uri)
  :on-path "annotations")

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