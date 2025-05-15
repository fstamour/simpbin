# simpbin

> A common lisp library to store data in a binary format

- Simple, optional 8 bytes header
- No size limits
- No index
- Unstructured
- Small API. Provide `read`/`write` functions for:
  - integers (32-bit or varint)
  - octets
  - strings
  - bit-vecors
  - optional header
- Operates on streams

## Design

At first, I just wanted to store a lot of strings without having to
worry about special characters and be able to iterate on them
afterwards.

So I came up with the simplest "format" I could think of: write the
`length of the string`, then the `string` and simply repeat
that. (Years later, I've learnt that this is often called a [flat-file
database](https://en.m.wikipedia.org/wiki/Flat-file_database).)

For good measure, I added a header consisting of a four bytes
signature `BINS` and another four reserved bytes so that I could have
different variants in the future.

## API

The API is very simple, if you want to do something more advanced you
can use the usual functions that deals with streams like flexi-streams
and nibbles (and the standard functions of course).

<!-- TODO the docstring in the readme are out of sync with the code -->

### Read/write an integer

```lisp
(defun read-integer (stream))
  "Read a 32 bit, little-endian integer from the stream." ...)

(defun write-integer (integer stream)
  "Write a 32 bit, little-endian integer to the stream." ...)
```

### Read/write an variable-length integer

```lisp
(defun read-varint (stream))
  "Read a varint-encoded integer from the stream." ...)

(defun write-varint (integer stream)
  "Write a varint-encoded integer to the stream." ...)
```

### Read/write a header

```lisp
(defun write-header (stream)
  "Write a header to the stream" ...)

(defun read-header (stream)
  "Read a header from the stream, verifies the signature and returns
the variant number." ...)
```

### Read/write octets (a sequence of bytes)

```lisp
(defun write-octets (octet-vector stream)
  "Write an octet-vector of octets to the stream.
First the size is written (using `write-integer'), then the content of the vector." ...)

(defun read-octets (stream)
  "Read an octet-vector of octets to the stream.
Read the length of the vector (using `read-integer') first, then the content." ...)
```

```lisp
(defun write-octets* (octet-vector stream)
  "Write an octet-vector of octets to the stream.
First the size is written (using `write-varint'), then the content of the vector." ...)

(defun read-octets* (stream)
  "Read an octet-vector of octets to the stream.
Read the length of the vector (using `read-varint') first, then the content." ...)
```

### Read/write strings

```lisp
(defun write-binary-string (string stream
          &key (encoding :utf8))
  "Write a string to stream." ...)

(defun read-binary-string (stream
         &key (encoding :utf8))
  "Read a string from stream." ...)
```

```lisp
(defun write-binary-string* (string stream
          &key (encoding :utf8))
  "Write a string to stream." ...)

(defun read-binary-string* (stream
         &key (encoding :utf8))
  "Read a string from stream." ...)
```

### Macros to open files

```lisp
(defmacro with-output-to-binary-file ((stream filespec
                &rest options
                &key
                if-exists
                if-does-not-exist
                external-format)
              &body body)
  "Helper macro to open a file for binary output." ...)
```

```lisp
(defmacro with-input-from-binary-file ((stream filespec
                &rest options
                &key
                if-exists
                if-does-not-exist
                external-format)
              &body body)
  "Helper macro to open a file for binary input."
```

## Examples

### Write a file
```lisp
(with-output-to-binary-file (output "/tmp/test.bins"
          :if-exists :overwrite
          :if-does-not-exist :create)
  (write-header output)
  (write-binary-string "Hello" output))
```

### Read a file
```lisp
(with-input-from-binary-file (input "/tmp/test.bins")
  (read-header input)
  (read-binary-string input))
```

## Examples of libraries that could be used with simpbin

- [cl-intbytes](https://quickdocs.org/cl-intbytes) - Convert between
  any-base integers and byte arrays interchangeably
- [ieee-floats](https://quickdocs.org/ieee-floats) - Convert floating
  point values to IEEE 754 binary representation
- [trivial-bit-streams](https://quickdocs.org/trivial-bit-streams) -
  Flexible buffered bit streams

## Alternatives

- [zcdb](http://www.xach.com/lisp/zcdb/) for immutable (but easy to
  rebuild) key-value database.
- [safe read](https://github.com/phoe/safe-read) to safely read
  s-expression.
- [cl-sql](http://clsql.kpe.io) or
  [postmodern](https://marijnhaverbeke.nl/postmodern/) to interface
  with most popular SQL databases.
- Or you can also write your data as text or json in a text file.
- CBOR

## License

GNU GPLv3
