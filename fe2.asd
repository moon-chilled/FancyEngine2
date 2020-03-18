(defpackage #:fe2-asd
  (:use :cl :asdf))

(defsystem "fe2"
    :name "FancyEngine2"
    :serial t
    :pathname "src/"

    :serial t
    :depends-on (#:sdl2 #:cl-opengl)
    :components ((:file "fe2-entry"))

    :build-pathname "fancyengine2"
    :entry-point "fe2-entry:main")


;#+sb-core-compression
;(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
;  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "fe2/release"
    :depends-on (:fe2)

    :entry-point "fe2-entry:main"
    :build-pathname "fancyengine2"
    :build-operation "asdf:program-op")

(defsystem "fe2/image"
    :depends-on (:fe2)

    :build-pathname "fancyengine2"
    :build-operation "asdf:image-op")
