(defpackage #:network-addresses
  (:use #:cl)
  (:nicknames #:na)
  (:export :network
           :invalid-format
           :integer-value
           :subnet-length))

(defpackage #:network-addresses-ipv4
  (:use #:cl)
  (:nicknames #:na4)
  (:export :make-network-from-cidr))
