(in-package "OBJC-TYPES")

(defvar typemap
  '((#\c :char)
    (#\i :int)
    (#\s :short)
    (#\l :long)
    (#\q :long-long)
    (#\C :unsigned-char)
    (#\I :unsigned-int)
    (#\S :unsigned-short)
    (#\L :unsigned-long)
    (#\Q :unsigned-long-long)
    (#\f :float)
    (#\d :double)
    (#\B objc-cffi:objc-bool)
    (#\v :void)
    (#\* :string)
    (#\@ objc-cffi:objc-id)
    (#\# objc-cffi:objc-class-pointer)
    (#\: objc-cffi:objc-sel)
    (#\? objc-unknown-type)))

(defun char-to-type (char)
  (loop for (type-char type) in typemap
     when (eql char type-char)
     return type))

(defvar method-code-map
  '((#\r :const)
    (#\n :in)
    (#\N :in-out)
    (#\o :out)
    (#\O :by-copy)
    (#\R :by-ref)
    (#\V :one-way)))

(defun char-to-methodcode (char)
  (loop for (mc-char mc) in method-code-map
     when (eql char mc-char)
     return mc))

(defun lex-typestr (typestr)
  (declare (optimize (debug 3)))
  (with-input-from-string (s typestr)
    (loop for idx-char = (read-char s nil nil)
       while (not (null idx-char))
       collecting
         (cond ((char-to-methodcode idx-char) (list 'methodcode (char-to-methodcode idx-char)))
               ((char-to-type idx-char) (list 'prim-type (char-to-type idx-char)))
               ((digit-char-p idx-char) (progn (unread-char idx-char s) (list 'alignment (read-num s))))
               ((eql #\[ idx-char) (list 'start-array (read-num s)))
               ((eql #\{ idx-char) (list 'start-struct (read-type-name s '(#\} #\=))))
               ((eql #\( idx-char) (list 'start-union (read-type-name s '(#\) #\=))))
               ((eql #\b idx-char) (list 'bitfield (list :bitfield (read-num s))))
               ((eql #\^ idx-char) (list 'pointer nil))
               ((eql #\= idx-char) (list 'name-separator nil))
               ((eql #\) idx-char) (list 'end-union nil))
               ((eql #\} idx-char) (list 'end-struct nil))
               ((eql #\] idx-char) (list 'end-array nil)))
         )))

(defun typestr-lexer (typestr)
  (let ((tokens (lex-typestr typestr)))
    #'(lambda ()
        (let ((v (pop tokens)))
          (if (null v)
              nil
              (values (first v)
                      (second v)))))))

(defun read-num (stream)
  (declare (optimize (debug 3)))
  (parse-integer
   (with-output-to-string (num)
     (loop for c = (read-char stream nil nil)
        while (not (null c))
        do 
          (if (digit-char-p c)
              (write-char c num)
              (progn
                (unread-char c stream)
                (loop-finish)))))))

(defun read-type-name (stream end-chars)
  (with-output-to-string (name)
    (loop for c = (read-char stream nil nil)
       while (not (null c))
       do 
         (if (not (member c end-chars))
             (write-char c name)
             (progn
               (unread-char c stream)
               (loop-finish))))))

(define-parser *objc-type-parser*
  (:start-symbol type-sequence)
  (:terminals (start-array end-array start-struct end-struct start-union end-union prim-type name-separator bitfield methodcode pointer alignment))
  
  (type-sequence
   (possibly-aligned-type type-sequence #'cons)
   (possibly-aligned-type #'list))

  (possibly-aligned-type
   (type alignment #'(lambda (a b) (list :align b a)))
   type)

  (type
   prim-type
   bitfield
   (methodcode type #'(lambda (a b) (list :method a b)))
   (pointer type #'(lambda (a b) (list :pointer b)))
   (start-array type end-array #'(lambda (a b c) (list :array a b)))
   (start-struct end-struct #'(lambda (a b) (list :struct a)))
   (start-union end-union #'(lambda (a b) (list :union a)))
   (start-struct name-separator end-struct #'(lambda (a b c) (list :struct a)))
   (start-struct name-separator type-sequence end-struct #'(lambda (a b c d) (list :struct a c)))
   (start-union name-separator type-sequence end-union #'(lambda (a b c d) (list :union a c)))))

(defun parse-objc-typestr (str)
  (parse-with-lexer (typestr-lexer str) *objc-type-parser*))

(defun parse-objc-method-signature (method)
  (let* ((types (parse-objc-typestr (objc-cffi:method-type-signature method)))
         (ret-val (nth 0 types))
         (class-arg (nth 1 types))
         (sel-arg (nth 2 types))
         (arguments (cdddr types)))
    `((:return ,ret-val)
      (:required-args ,class-arg ,sel-arg)
      (:args ,@arguments))))