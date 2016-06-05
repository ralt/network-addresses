(in-package #:network-addresses-test)

(def-suite ipv4 :description "IPv4 addresses")

(in-suite ipv4)

(defmacro signals-invalid-format (&body body)
  `(signals (na:invalid-format)
     ,@body))

(test invalid-formats
  (signals-invalid-format (na4:make-network-from-cidr "foo"))
  (signals-invalid-format (na4:make-network-from-cidr "100"))
  (signals-invalid-format (na4:make-network-from-cidr "100.100.100.100"))
  (signals-invalid-format (na4:make-network-from-cidr "300.100.100.100/24"))
  (signals-invalid-format (na4:make-network-from-cidr "100.100.100.100/33")))

(test sanity
  (let ((network (na4:make-network-from-cidr "255.255.255.255/32")))
    (is (= (na:subnet-length network) 32))
    (is (= (na:integer-value network) 4294967295))))
