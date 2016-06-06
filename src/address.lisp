(in-package #:network-addresses)

(defclass address ()
  ((integer-value :initarg :integer-value :reader as-int)))
