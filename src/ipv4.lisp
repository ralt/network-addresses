(in-package #:network-addresses-ipv4)

(defclass ipv4-network (na:network)
  ((na:width :reader na:width :initform 32)
   (na:max-value :reader na:max-value :initform #xFFFFFFFF)))

(defclass ipv4-address (na:address) ())

(defmethod initialize-instance :after ((network ipv4-network) &key)
  (unless (and (and (>= (na:as-int network) 0)
                    (<= (na:as-int network) (na:max-value network)))
               (and (>= (na:subnet-length network) 0)
                    (<= (na:subnet-length network) (na:width network))))
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

(defmethod print-object ((address ipv4-address) out)
  (print-unreadable-object (address out :type t)
    (format out (na:as-str address))))

(defmethod na:as-str ((address ipv4-address))
  (format nil "~A.~A.~A.~A"
          (ash (na:as-int address) -24)
          (logand (ash (na:as-int address) -16) #xFF)
          (logand (ash (na:as-int address) -8) #xFF)
          (logand (na:as-int address) #xFF)))

(defmethod print-object ((network ipv4-network) out)
  (print-unreadable-object (network out :type t)
    (format out (na:as-str network))))

(defmethod na:as-str ((network ipv4-network))
  (format nil "~A/~D"
          (na:as-str (make-instance 'ipv4-address :integer-value (na:as-int network)))
          (na:subnet-length network)))

(defmethod na:hostmask ((network ipv4-network))
  (make-instance 'ipv4-address :integer-value (na:hostmask-int network)))

(defmethod na:netmask ((network ipv4-network))
  (make-instance 'ipv4-address :integer-value (na:netmask-int network)))

(defmethod na:broadcast ((network ipv4-network))
  (make-instance 'ipv4-address :integer-value (na:broadcast-int network)))

(defmethod na:addresses ((network ipv4-network))
  (mapcar (lambda (address-int) (make-instance 'ipv4-address :integer-value address-int))
          (na:addresses-int network)))

(defmethod na:first-address ((network ipv4-network))
  (make-instance 'ipv4-address :integer-value (na:first-address-int network)))

(defmethod na:last-address ((network ipv4-network))
  (make-instance 'ipv4-address :integer-value (na:last-address-int network)))
