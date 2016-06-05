(in-package #:network-addresses)

(define-condition invalid-format (error) ())

(defclass network ()
  ((integer-value :initarg :integer-value :reader integer-value)
   (subnet-length :initarg :subnet-length :reader subnet-length)))
