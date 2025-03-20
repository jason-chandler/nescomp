(in-package :nescomp)

;; 6502 Assembly Formatter for ca65
;; Converts s-expression assembly to ca65-compatible text

;; (pprint (format-assembly (test)))

(defun format-assembly (code &optional (stream t))
  "Format compiled assembly code to ca65 syntax"
  (format stream ";;; Generated NES 6502 code for ca65 assembler~%~%")
  (format stream ".segment \"CODE\"~%~%")
  (dolist (statement code)
    (format-statement statement stream)
    (format stream "~%"))
  (values))

(defun format-statement (statement stream)
  "Format a single assembly statement"
  (cond
    ;; Label
    ((symbolp statement)
     (format stream "~a:" statement))
    
    ;; Directive (like .byte, .word)
    ((and (listp statement) (stringp (first statement)) (char= (char (first statement) 0) #\.))
     (format stream "~a ~{~a~^, ~}" 
             (first statement) 
             (mapcar #'format-operand (rest statement))))
    
    ;; Special case for defcode directive
    ((and (listp statement) (eq (first statement) 'defcode))
     (format stream ";;; Function: ~a~%" (second statement))
     (format stream ";;; Parameters: ~a~%" (third statement))
     (loop :for nested-statement :in (cdddr statement)
           :do (progn (format-statement nested-statement stream)
                      (format stream "~%"))))

    ;; Special case for segments
    ((and (listp statement) (eq (first statement) '.segment))
     (format stream "~a \"~a\"" (string-downcase (symbol-name (first statement))) (second statement)))
    
    ;; Normal instruction
    ((listp statement)
     (format stream "    ~a ~{~a~^, ~}" 
             (format-mnemonic (first statement))
             (mapcar #'format-operand (rest statement))))
    
    ;; Other (shouldn't happen)
    (t (format stream "; Unknown statement: ~a" statement))))

(defun format-mnemonic (mnemonic)
  "Format instruction mnemonic"
  (string-downcase (string mnemonic)))

(defun format-operand (operand)
  "Format an instruction operand"
  (cond
    ;; Register
    ((and (symbolp operand) (member operand '(a x y)))
     (string-downcase (string operand)))
    
    ;; Symbol reference (label)
    ((symbolp operand)
     (string operand))
    
    ;; Immediate value: (imm value)
    ((and (listp operand) (eq (first operand) 'imm))
     (format nil "#~a" (format-value (second operand))))
    
    ;; Immediate low byte of address: (lo address)
    ((and (listp operand) (listp (second operand)) (eq (first (second operand)) '<))
     (format nil "#<~a" (format-value (second (second operand)))))
    
    ;; Immediate high byte of address: (hi address)
    ((and (listp operand) (listp (second operand)) (eq (first (second operand)) '>))
     (format nil "#>~a" (format-value (second (second operand)))))
    
    ;; Immediate with #
    ((and (listp operand) (eq (first operand) '\#))
     (format nil "#~a" (format-value (second operand))))
    
    ;; Absolute indexed: (abs-x address) or (abs-y address)
    ((and (listp operand) (member (first operand) '(abs-x abs-y)))
     (let ((register (case (first operand) (abs-x 'x) (abs-y 'y))))
       (format nil "~a,~a" (format-value (second operand)) register)))
    
    ;; Zero page indexed: (zp-x address) or (zp-y address)
    ((and (listp operand) (member (first operand) '(zp-x zp-y)))
     (let ((register (case (first operand) (zp-x 'x) (zp-y 'y))))
       (format nil "~a,~a" (format-value (second operand)) register)))
    
    ;; Indirect: (ind address)
    ((and (listp operand) (eq (first operand) 'ind))
     (format nil "(~a)" (format-value (second operand))))
    
    ;; Indexed indirect: (ind-x address)
    ((and (listp operand) (eq (first operand) 'ind-x))
     (format nil "(~a,x)" (format-value (second operand))))
    
    ;; Indirect indexed: (ind-y address)
    ((and (listp operand) (eq (first operand) 'ind-y))
     (format nil "(~a),y" (format-value (second operand))))
    
    ;; Absolute address: (abs address) or just the address
    ((or (and (listp operand) (eq (first operand) 'abs-))
         (numberp operand)
         (symbolp operand))
     (let ((address (if (listp operand) (second operand) operand)))
       (format-value address)))
    
    ;; Quote expression
    ((and (listp operand) (eq (first operand) 'quote))
     (format-value (second operand)))
    
    ;; List as an operand (like memory addressing)
    ((listp operand)
     (format nil "~{~a~^, ~}" (mapcar #'format-operand operand)))
    
    ;; Other
    (t (format nil "~a" operand))))

(defun format-value (value)
  "Format a value (number, symbol, etc.)"
  (cond
    ;; Hexadecimal values with $ prefix
    ((and (numberp value) (> value 9))
     (format nil "$~X" value))
    
    ;; Binary values with % prefix
    ((and (numberp value) (> value 1) (< value 256)
          (find #\1 (format nil "~B" value))
          (every (lambda (c) (member c '(#\0 #\1))) (format nil "~B" value)))
     (format nil "%~8,'0B" value))
    
    ;; Regular numbers
    ((numberp value)
     (format nil "~D" value))
    
    ;; Symbols
    ((symbolp value)
     (string value))
    
    ;; Other
    (t (format nil "~a" value))))


;;(format-assembly (comp '(test)))

;; Example usage:
(defun example-output ()
  (let ((test-code 
          '(
            (defcode factorial (n)
              factorial
              (jsr save_context)
              (lda (imm 0))
              (cmp zp_param0)
              (bne not_zero)
              (lda (imm 1))
              (jmp done)
              not_zero
              (lda zp_param0)
              (pha)
              (dec zp_param0)
              (jsr factorial)
              (sta temp1)
              (pla)
              (jsr mul6502)
              done
              (jsr restore_context)
              (rts))
            )))
    (format-assembly test-code)))
(example-output)

;; Utility functions to write the output to a file
(defun write-assembly-to-file (code filename)
  "Write the formatted assembly to a file"
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format-assembly code stream)))

;; Function to combine the Lisp compiler with the formatter
(defun compile-lisp-to-6502-file (lisp-expr filename &optional (prelude nil))
  "Compile Lisp expression to 6502 and write it to a file"
  (let ((compiled-code (cdr (comp lisp-expr))))
    (if prelude
        (write-assembly-to-file (append prelude compiled-code) filename)
        (write-assembly-to-file compiled-code filename))))


;; (pprint (append (output-runtime-functions) (cadr (comp '(factorial)))))
(compile-lisp-to-6502-file nil "test.s" (example-nes-program))

(pprint (format-assembly (output-runtime-functions)))
(pprint (format-assembly (example-nes-program)))
(format-assembly `(,(example-factorial)))

(pprint (example-nes-program))