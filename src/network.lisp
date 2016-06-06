(in-package #:network-addresses)

(define-condition invalid-format (error) ())

(defclass network ()
  ((integer-value :initarg :integer-value :reader as-int)
   (subnet-length :initarg :subnet-length :reader subnet-length)
   (width :reader width :initform (error "Not implemented."))
   (max-value :reader max-value :initform (error "Not implemented."))))

(defclass address ()
  ((integer-value :initarg :integer-value :reader as-int)))

(defgeneric netmask (network)
  (:documentation "Returns the netmask as an IP address."))

(defgeneric hostmask (network)
  (:documentation "Returns the hostmask as an IP address."))

(defun netmask-int (network)
  "Returns the netmask as an integer."
  (logxor (max-value network) (hostmask-int network)))

(defun hostmask-int (network)
  "Returns the hostmask as an integer."
  (- (ash 1 (- (width network) (subnet-length network)))
     1))

(defgeneric as-str (object)
  (:documentation "Returns the IP address in its string representation."))
