(in-package #:network-addresses-ipv4)

(defclass ipv4-network (na:network) ())

(defvar *ipv4-max-network-value* 4294967295)
(defvar *ipv4-max-network-length* 32)

(defmethod initialize-instance :after ((network ipv4-network) &key)
  (unless (and (and (>= (na:integer-value network) 0)
                    (<= (na:integer-value network) *ipv4-max-network-value*))
               (and (>= (na:subnet-length network) 0)
                    (<= (na:subnet-length network) *ipv4-max-network-length*)))
    (error 'na:invalid-format)))

(defvar *ipv4-cidr-regex* "(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)\\/(\\d+)")

(defun make-network-from-cidr (cidr)
  "Return a IPv4 network object based on the cidr."
  (multiple-value-bind (has-match vec)
      (ppcre:scan-to-strings *ipv4-cidr-regex* cidr)
    (unless (or has-match
                (= (length vec) 5))
      (error 'na:invalid-format))
    (make-instance 'ipv4-network
                   :integer-value (ipv4-integers-to-integer (subseq vec 0 4))
                   :subnet-length (parse-integer (elt vec 4) :junk-allowed t))))

(defun ipv4-integers-to-integer (strings)
  "Transforms a list of integers in strings into a single integer,
by having each integer going through its byte representation, then
concatenating all the bytes, and returning the value of these bits.
With math."
  (+ (* (parse-integer (elt strings 0)) (expt 2 24))
     (* (parse-integer (elt strings 1)) (expt 2 16))
     (* (parse-integer (elt strings 2)) (expt 2 8))
     (parse-integer (elt strings 3))))
