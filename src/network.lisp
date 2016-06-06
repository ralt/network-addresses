(in-package #:network-addresses)

(defclass network ()
  ((integer-value :initarg :integer-value :reader as-int)
   (subnet-length :initarg :subnet-length :reader subnet-length)
   (width :reader width :initform (error "Not implemented."))
   (max-value :reader max-value :initform (error "Not implemented."))))

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

(defgeneric broadcast (network)
  (:documentation "Returns the broadcast as an IP address."))

(defun broadcast-int (network)
  "Returns the broadcast as an integer."
  (logior (as-int network) (hostmask-int network)))
