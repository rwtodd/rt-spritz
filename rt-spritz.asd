(defsystem "rt-spritz"
  :description "rt-spritz: spritz cipher utilities"
  :version "1.0"
  :author "Richard Todd <richard.wesley.todd@gmail.com>"
  :licence "MIT"
  :build-operation "program-op" ;; leave as is
  :build-pathname "spritz"
  :entry-point "rt-spritz:spritz-main"
  :depends-on ("rt-bintext")
  :serial t
  :components ((:file "packages")
               (:file "cipher")
               (:file "spritz")))

