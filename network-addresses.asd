(asdf:defsystem #:network-addresses
  :description "A network addresses manipulation library."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "network" :depends-on ("package"))
                             (:file "ipv4" :depends-on ("package" "network"))))))
