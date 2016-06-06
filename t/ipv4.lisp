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
    (is (= (na:as-int network) 4294967295))))

(test netmask
  (let ((network (na4:make-network-from-cidr "192.168.0.0/16")))
    (is (string= (na:as-str (na:netmask network)) "255.255.0.0")))
  (let ((network (na4:make-network-from-cidr "192.168.0.0/18")))
    (is (string= (na:as-str (na:netmask network)) "255.255.192.0"))))

(test hostmask
  (let ((network (na4:make-network-from-cidr "192.168.0.0/16")))
    (is (string= (na:as-str (na:hostmask network)) "0.0.255.255"))))

(test str-network
  (let ((network (na4:make-network-from-cidr "192.168.0.0/16")))
    (is (string= (na:as-str network) "192.168.0.0/16")))
  (let ((network (na4:make-network-from-cidr "192.168.0.255/16")))
    (is (string= (na:as-str network) "192.168.0.255/16"))))

(test broadcast
  (let ((network (na4:make-network-from-cidr "192.168.0.0/16")))
    (is (string= (na:as-str (na:broadcast network)) "192.168.255.255"))))

(test loop-network-addresses
  (let* ((network (na4:make-network-from-cidr "192.168.0.0/16"))
         (addresses (na:addresses network)))
    (is (string= (na:as-str (first addresses)) "192.168.0.1"))
    (is (string= (na:as-str (first (last addresses))) "192.168.255.254"))))
