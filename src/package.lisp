(defpackage #:network-addresses
  (:use #:cl)
  (:nicknames #:na)
  (:export :network
           :invalid-format
           :as-int
           :as-str
           :subnet-length
           :netmask
           :netmask-int
           :hostmask
           :hostmask-int
           :broadcast
           :broadcast-int
           :width
           :max-value
           :address))

(defpackage #:network-addresses-ipv4
  (:use #:cl)
  (:nicknames #:na4)
  (:export :make-network-from-cidr
           :netmask
           :hostmask))
