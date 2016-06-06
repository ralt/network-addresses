# network-addresses

Network addresses manipulation library.

This library lets you have an abstraction over IP addresses (ipv4 only
for now) to manipulate them. Namely, there are 2 main abstractions:

- IP networks
- IP addresses

An IP network consists of 2 things: a value and a subnet length,
e.g. `192.168.0.0/16`.

An IP address only consists of a value.

### API

#### Class hierarchy

There exists 4 classes, best explained with their definitions:

```lisp
(defclass network ()
  ((integer-value :initarg :integer-value :reader as-int)
   (subnet-length :initarg :subnet-length :reader subnet-length)
   (width :reader width :initform (error "Not implemented."))
   (max-value :reader max-value :initform (error "Not implemented."))))

(defclass ipv4-network (na:network)
  ((na:width :reader na:width :initform 32)
   (na:max-value :reader na:max-value :initform #xFFFFFFFF)))

(defclass address ()
  ((integer-value :initarg :integer-value :reader as-int)))

(defclass ipv4-address (na:address) ())
```


#### Packages

The `network-addresses` system provides 2 packages:
`network-addresses` (nicknamed `na`) and `network-addresses-ipv4`
(nicknamed `na4`). The `na` package holds most of the public symbols,
while `na4` has the ipv4-specific functions.

##### `network-addresses-ipv4` package

###### `make-network-from-cidr`

Takes a string representing a network in the CIDR notation, returns an
`ipv4-network`.

Example:

```lisp
CL-USER> (na4:make-network-from-cidr "192.168.0.0/16")
#<NETWORK-ADDRESSES-IPV4::IPV4-NETWORK 192.168.0.0/16>
```

##### `network-addresses` package

###### Conditions

This package can raise the `na:invalid-format` condition when an
invalid IP address is provided to `make-network-from-cidr`.

###### `network` methods/functions

List of methods and functions that act on `network` objects. This list
does not include the slot readers.

####### `as-str`

Returns the network value as a string.

Example:

```lisp
CL-USER> (na:as-str (na4:make-network-from-cidr "192.168.0.0/16"))
"192.168.0.0/16"
```

####### `netmask`

Returns the netmask of the network as an `address`.

Example:

```lisp
CL-USER> (na:netmask (na4:make-network-from-cidr "192.168.0.0/16"))
#<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 255.255.0.0>
```

####### `netmask-int`

Returns the netmask of the network as an integer.

Example:

```lisp
CL-USER> (na:netmask-int (na4:make-network-from-cidr "192.168.0.0/16"))
4294901760
```

####### `hostmask`

Returns the hostmask of the network as an `address`.

Example:

```lisp
CL-USER> (na:hostmask (na4:make-network-from-cidr "192.168.0.0/16"))
#<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 0.0.255.255>
```

####### `hostmask-int`

Returns the hostmask of the network as an integer.

Example:

```lisp
CL-USER> (na:hostmask-int (na4:make-network-from-cidr "192.168.0.0/16"))
65535
```

####### `broadcast`

Returns the broadcast of the network as an `address`.

Example:

```lisp
CL-USER> (na:broadcast (na4:make-network-from-cidr "192.168.0.0/16"))
#<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 192.168.255.255>
```

####### `broadcast-int`

Returns the broadcast of the network as an integer.

Example:

```lisp
CL-USER> (na:broadcast-int (na4:make-network-from-cidr "192.168.0.0/16"))
3232301055
```

####### `addresses`

Returns the list of IP addresses (excluding first and last) of the
network as a list of `address`.

Example:

```lisp
CL-USER> (na:addresses (na4:make-network-from-cidr "192.168.0.0/30"))
(#<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 192.168.0.1>
 #<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 192.168.0.2>)
```

####### `addresses-int`

Returns the list of IP addresses (excluding first and last) of the
network as a list of integers.

Example:

```lisp
CL-USER> (na:addresses-int (na4:make-network-from-cidr "192.168.0.0/30"))
(3232235521 3232235522)
```

####### `first-address`

Returns the first IP address of the network as an `address`.

Example:

```lisp
CL-USER> (na:first-address (na4:make-network-from-cidr "192.168.0.0/30"))
#<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 192.168.0.0>
```

####### `first-address-int`

Returns the first IP address of the network as an integer.

Example:

```lisp
CL-USER> (na:first-address-int (na4:make-network-from-cidr "192.168.0.0/30"))
3232235520
```

####### `last-address`

Returns the last IP address of the network as an `address`.

Example:

```lisp
CL-USER> (na:last-address (na4:make-network-from-cidr "192.168.0.0/30"))
#<NETWORK-ADDRESSES-IPV4::IPV4-ADDRESS 192.168.0.3>
```

####### `last-address-int`

Returns the last IP address of the network as an integer.

Example:

```lisp
CL-USER> (na:last-address-int (na4:make-network-from-cidr "192.168.0.0/30"))
3232235523
```

###### `address` methods/functions

List of methods and functions that act on `address` objects. This list
does not include the slot readers.

####### `as-str`

Returns the address value as a string.

Example:

```lisp
CL-USER> (na:as-str (na:first-address (na4:make-network-from-cidr "192.168.0.0/16")))
"192.168.0.0"
```




### License

This software is provided under the MIT license. Please see the [LICENSE](LICENSE) file.
