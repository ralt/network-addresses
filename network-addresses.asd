(asdf:defsystem #:network-addresses
  :description "A network addresses manipulation library."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "generic" :depends-on ("package"))
                             (:file "conditions" :depends-on ("package"))
                             (:file "address" :depends-on ("package"))
                             (:file "network" :depends-on ("package" "generic" "conditions" "address"))
                             (:file "ipv4" :depends-on ("package" "network"))))))
