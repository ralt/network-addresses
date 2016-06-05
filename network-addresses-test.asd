(asdf:defsystem #:network-addresses-test
  :description "Tests for network-addresses."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:network-addresses :fiveam)
  :components ((:module "t"
                :components ((:file "package")
                             (:file "ipv4" :depends-on ("package"))))))
