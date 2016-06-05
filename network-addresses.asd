(asdf:defsystem #:network-addresses
  :description "A network addresses manipulation library."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "network-addresses"
                              :depends-on ("package"))))))
