(in-package :nescomp)

(defmacro jsr (&rest args) (when args `(list ',@args)))
(defmacro lda (&rest args) (when args `(list ',@args)))
(defmacro pha (&rest args) (when args `(list ',@args)))
(defmacro sta (&rest args) (when args `(list ',@args)))
(defmacro pla (&rest args) (when args `(list ',@args)))
(defmacro rts (&rest args) (when args `(list ',@args)))
(defmacro defcode (name args &body body) `(defun ,name ,args ',body))

;; Compile a lisp function
(defun nes-compile (name)
  (if (eq (car (eval name)) 'lambda)
      (eval (comp (cons 'defun (cons name (cdr (eval name))))))
      (error "Not a Lisp function")))

;; (pprint (example-factorial))

;; The main compile routine - returns compiled code for x, prefixed by type :integer or :boolean
;; Leaves result in accumulator (a)
(defun comp (x &optional env)
  (cond
    ((null x) (type-code :boolean `((lda ,(imm 0)))))
    ((eq x t) (type-code :boolean `((lda ,(imm 1)))))
    ((symbolp x) (comp-symbol x env))
    ((atom x) (type-code :integer (list (list 'lda (imm x)))))
    (t (let ((fn (first x)) (args (rest x)))
         (case fn
           (defun (setq *label-num* 0)
               (setq env (mapcar #'(lambda (x y) (cons x y)) (second args) *locals*))
             (comp-defun (first args) (second args) (cddr args) env))
           (progn (comp-progn args env))
           (if    (comp-if (first args) (second args) (third args) env))
           (setq  (comp-setq args env))
           (t     (comp-funcall fn args env)))))))

;; Utilities

;; Like mapcon but not destructive
(defun mappend (fn lst)
  (apply #'append (mapcar fn lst)))

;; The type is prefixed onto the list of assembler code instructions
(defun type-code (type code) (cons type code))
(defun code-type (type-code) (car type-code))
(defun code (type-code) (cdr type-code))

(defun checktype (fn type check)
  (unless (or (null type) (null check) (eq type check))
    (error "Argument to '~a' must be ~a not ~a" fn check type)))

;; Allocate registers for 6502
;; 6502 has very limited registers, so we use zero page memory locations instead
(defvar *params* '(zp_param0 zp_param1 zp_param2 zp_param3))
(defvar *locals* '(zp_local0 zp_local1 zp_local2 zp_local3))

;; Generate a label
(defvar *label-num* 0)
(defun gen-label ()
  (read-from-string (format nil "label~d" (incf *label-num*))))

;; Subfunctions

(defun comp-symbol (x env)
  (let ((zploc (cdr (assoc x env))))
    (type-code nil (list (list 'lda zploc)))))

(defun comp-setq (args env)
  (let ((value (comp (second args) env))
        (zploc (cdr (assoc (first args) env))))
    (type-code 
     (code-type value) 
     (append (code value) (list (list 'sta zploc))))))

(defun comp-defun (name args body env)
  (let ((used (subseq *locals* 0 (length args))))
    (append 
     (list 'defcode name args)
     (list name)
     ;; Save return address
     (list '(jsr save_context))
     ;; Move parameters to their local variable locations
     (apply #'append 
            (mapcar #'(lambda (x y) 
                        (list (list 'lda y) (list 'sta x)))
                    used *params*))
     (code (comp-progn body env))
     ;; Restore context and return
     (list '(jsr restore_context) '(rts)))))

(defun comp-progn (exps env)
  (let* ((len (1- (length exps)))
         (nlast (subseq exps 0 len))
         (last1 (nth len exps))
         (start (mappend #'(lambda (x) (append (code (comp x env)))) nlast))
         (end (comp last1 env)))
    (type-code (code-type end) (append start (code end)))))

(defun comp-if (pred then else env)
  (let ((false-label (gen-label))
        (end-label (gen-label))
        (test (comp pred env)))
    (checktype 'if (car test) :boolean)
    (type-code :integer
               (append
                (code test) 
                (list '(cmp (imm 0)) (list 'beq false-label))
                (code (comp then env)) 
                (list (list 'jmp end-label) false-label)
                (code (comp else env)) 
                (list end-label)))))

(defun comp-funcall (f args env)
  ;; These are opposite because we're constructing the labels in the opposite order. I'm sorry.
  (let ((comparisons (assoc f '((> . blt) (>= . bcc) (= . bne) 
                                (<= . bcs) (< . bge) (/= . beq))))
        (logical (assoc f '((and . and) (or . ora))))
        (arith1 (assoc f '((1+ . inc) (1- . dec))))
        (arith+- (assoc f '((+ . add6502) (- . sub6502)))))
    (cond
      (comparisons
       (let ((label (gen-label))
             (end-label (gen-label)))
         (type-code :boolean
                    (append
                     (comp-args f args 2 :integer env)
                     ;; 6502 has different comparison semantics from ARM
                     ;; Compare A with memory, branch if condition
                     (list '(pla) '(sta temp1) ; Second arg
                           '(pla) '(cmp temp1) ; First arg compared with second
                           (list (cdr comparisons) label) 
                           '(lda (imm 1)) (list 'jmp end-label)
                           label '(lda (imm 0)) end-label)))))
      (logical 
       (type-code :boolean
                  (append
                   (comp-args f args 2 :boolean env)
                   (list '(pla) (list (cdr logical) '(imm temp1))))))
      (arith1
       (type-code :integer 
                  (append
                   (comp-args f args 1 :integer env)
                   ;; 6502 inc/dec work on memory, not accumulator
                   (list '(sta temp1) (list (cdr arith1) 'temp1) '(lda temp1)))))
      (arith+-
       (type-code :integer 
                  (append
                   (comp-args f args 2 :integer env)
                   (list '(sta temp1) '(pla) 
                         (list 'jsr (cdr arith+-))))))
      ((member f '(car cdr))
       (type-code :integer
                  (append
                   (comp-args f args 1 :integer env)
                   ;; We're using a custom implementation for linked lists
                   (if (eq f 'cdr) 
                       (list '(sta ptr) `(ldy ,(imm 2)) '(lda (ptr)\,y) '(tax)
                             '(iny) '(lda (ptr)\,y) '(sta ptr+1) '(stx ptr) '(lda (ptr)))
                       (list '(sta ptr) `(ldy ,(imm 0)) '(lda (ptr)\,y))))))
      (t ; function call
       (type-code :integer 
                  (append
                   (comp-args f args nil :integer env)
                   ;; Setup parameters in zero page
                   (when (> (length args) 0)
                     (let* ((n (length args))
                            (param-setup nil))
                       (dotimes (i n)
                         (push (list 'pla) param-setup)
                         (push (list 'sta (nth (- n i 1) *params*)) param-setup))
                       (reverse param-setup)))
                   (list (list 'jsr f))))))))

(defun comp-args (fn args n type env)
  (unless (or (null n) (= (length args) n))
    (error "Incorrect number of arguments to '~a'" fn))
  (let ((n (length args)))
    (mappend #'(lambda (y)
                 (let ((c (comp y env)))
                   (decf n)
                   (checktype fn type (code-type c))
                   (if (zerop n) (code c) (append (code c) '((pha))))))
             args)))

;; 6502-specific utility functions that will be included in the runtime

(defun output-runtime-functions ()
  `(
    ;; Save context (return address and registers)
    save_context
    (pla) (sta return_addr)
    (pla) (sta return_addr+1)
    (rts)
    
    ;; Restore context and return
    restore_context
    (lda return_addr+1) (pha)
    (lda return_addr) (pha)
    (rts)
    
    ;; Addition function for 6502
    add6502
    (clc)
    (adc temp1)
    (rts)
    
    ;; Subtraction function for 6502
    sub6502
    (sec)
    (sbc temp1)
    (rts)
    
    ;; Multiplication (8-bit)
    mul6502
    (sta multiplicand)
    (sty multiplier)
    (lda (imm 0))
    (sta product)
    (ldx (imm 8))
    mul_loop
    (lsr multiplier)
    (bcc mul_skip)
    (clc)
    (adc multiplicand)
    mul_skip
    (asl multiplicand)
    (dex)
    (bne mul_loop)
    (rts)
    
    ;; Initialize heap and allocate cons cells
    init_heap
    (lda ,(lo 'heap_start))  ; Low byte of heap_start address
    (sta heap_ptr)
    (lda ,(hi 'heap_start))  ; High byte of heap_start address
    (sta heap_ptr+1)
    (lda (imm 0))
    (sta heap_used)
    (rts)
    
    ;; Allocate memory - returns pointer in A/X (lo/hi)
    alloc
    (sta alloc_size)    ; Size in bytes
    (lda heap_ptr)
    (sta alloc_result)
    (lda heap_ptr+1)
    (sta alloc_result+1)
    ;; Update heap pointer
    (lda heap_ptr)
    (clc)
    (adc alloc_size)
    (sta heap_ptr)
    (lda heap_ptr+1)
    (adc \#0)         ; Add carry
    (sta heap_ptr+1)
    ;; Return pointer
    (lda alloc_result)
    (ldx alloc_result+1)
    (rts)
    
    ;; Create a cons cell - car in A, cdr in X/Y
    cons
    (sta cons_car)
    (stx cons_cdr)
    (sty cons_cdr+1)
    ;; Allocate 4 bytes
    (lda \#4)
    (jsr alloc)
    ;; Store car/cdr in cell
    (sta cons_ptr)
    (stx cons_ptr+1)
    (ldy \#0)
    (lda cons_car)
    (sta (cons_ptr)y)
    (iny)
    (lda \#0)          ; High byte of car (for numeric values)
    (sta (cons_ptr)y)
    (iny)
    (lda cons_cdr)
    (sta (cons_ptr)y)
    (iny)
    (lda cons_cdr+1)
    (sta (cons_ptr)y)
    ;; Return pointer to cons cell
    (lda cons_ptr)
    (ldx cons_ptr+1)
    (rts)
    
    ;; Define the zero page memory used by the runtime
    (.segment "ZEROPAGE")
    temp1 (.res 1)           ; Temporary storage
    temp2 (.res 1)
    ptr (.res 2)             ; General purpose pointer
    return_addr (.res 2)     ; Return address for functions
    
    ;; Arithmetic
    multiplicand (.res 1)
    multiplier (.res 1)
    product (.res 1)
    
    ;; Memory allocation
    heap_ptr (.res 2)        ; Current heap position
    heap_used (.res 1)       ; Number of used cells
    alloc_size (.res 1)      ; Size for allocation
    alloc_result (.res 2)    ; Result pointer from alloc
    
    ;; Cons cells
    cons_car (.res 1)
    cons_cdr (.res 2)
    cons_ptr (.res 2)
    
    ;; Function parameters (in zero page for speed)
    zp_param0 (.res 1)
    zp_param1 (.res 1)
    zp_param2 (.res 1)
    zp_param3 (.res 1)
    
    ;; Local variables (in zero page)
    zp_local0 (.res 1)
    zp_local1 (.res 1)
    zp_local2 (.res 1)
    zp_local3 (.res 1)
    
    ;; Heap starts after zero page
    (.segment "RAM")
    heap_start
    ))

;; Helper functions for addressing modes
(defun imm (value)
  "Immediate addressing mode: #value"
  (list '\# value))

(defun abs- (address)
  "Absolute addressing mode: address"
  address)

(defun abs-x (address)
  "Absolute,X addressing mode: address,X"
  (list address 'x))

(defun abs-y (address)
  "Absolutey addressing mode: addressy"
  (list address 'y))

(defun ind (address)
  "Indirect addressing mode: (address)"
  (list '() address))

(defun ind-x (address)
  "Indexed indirect addressing mode: (address,X)"
  (list '() (list address 'x)))

(defun ind-y (address)
  "Indirect indexed addressing mode: (address)y"
  (list (list '() address) 'y))

(defun zp (address)
  "Zero page addressing mode: address"
  address)

(defun zp-x (address)
  "Zero page,X addressing mode: address,X"
  (list address 'x))

(defun zp-y (address)
  "Zero pagey addressing mode: addressy"
  (list address 'y))

(defun lo (address)
  "Low byte of address: #<address"
  (list '\# (list '< address)))

(defun hi (address)
  "High byte of address: #>address"
  (list '\# (list '> address)))

;; Example usage - factorial function
(defun example-factorial ()
  (comp '(defun factorial (n)
          (if (= n 0)
              1
              (* n (factorial (1- n)))))))

(pprint (comp '(defun factorial (n)
                (if (= n 0)
                    1
                    (* n (factorial (1- n)))))))
(pprint (example-factorial))

;; Sample NES program with Lisp functions
(defun example-nes-program ()
  (append
   `(;; NES Header
     (.segment "HEADER")
     (.byte "\"EXAMPLE\"")   ; NES header identifier
     romspec
     (.byte $30)
     (.byte 0)
     (.byte $07)
     (.byte 0 0 0 0)
     (.word $AAAA $5555)
     
     ;; Vectors (required for NES)
     (.segment "VECTORS")
     (.word nmi)
     (.word reset)
     (.word irq)
            
     ;; Zero page variables           
     (.segment "ZEROPAGE")
     score (.res 1)
     
      
     
     ;; Include the zero page definitions
     ,@(output-runtime-functions)
     
     ;; Main program
       (.segment "CODE")
       
       reset
       (sei)               ; Disable IRQs
       (cld)               ; Clear decimal mode
       (ldx \#$ff)
       (txs)               ; Setup stack
       
       ;; Initialize heap for Lisp functions
       (jsr init_heap)
       
       ;; Clear RAM and setup NES
       (jsr clear_ram)
       (jsr init_ppu)
       
       ;; Run main game loop
       (jsr main_loop)
       (jmp reset)
       
       ;; Main Lisp functions compiled to 6502)
       )
    
   ;; Include compiled Lisp functions
    `(,(example-factorial))
   
   '(;; Game main loop (mostly assembly, calling Lisp functions)
     main_loop
     (jsr wait_vblank)
     
     ;; Call our Lisp function as part of game logic
     (lda (imm 5))         ; Calculate factorial of 5
     (sta zp_param0)
     (jsr factorial)       ; Result in accumulator
     
     ;; Store result for display
     (sta score)
     
     ;; Update sprites
     (jsr update_sprites)
     
     (jmp main_loop)
     
     ;; NES standard functions (PPU, sprites, etc)
     clear_ram
     ;; Clear memory from $0000-$07FF
     (lda (imm 0))
     (ldx (imm 0))
     clear_loop
     (sta (abs-x $0000))
     (sta (abs-x $0100))
     (sta (abs-x $0200))
     (sta (abs-x $0300))
     (sta (abs-x $0400))
     (sta (abs-x $0500))
     (sta (abs-x $0600))
     (sta (abs-x $0700))
     (inx)
     (bne clear_loop)
     (rts)
     
     init_ppu
     ;; Standard PPU initialization
     (lda (imm %10000000))
     (sta (abs- $2000))     ; Enable NMI
     (lda (imm %00011110))
     (sta (abs- $2001))     ; Enable sprites & background
     (rts)
     
     wait_vblank
     (bit (abs- $2002))
     wait_vblank_loop
     (bit (abs- $2002))
     (bpl wait_vblank_loop)
     (rts)
     
     update_sprites
     ;; Update sprites from game state
     ;; (implementation details omitted)
     (rts)
     
     nmi
     ;; NMI handler (vertical blank interrupt)
     (rti)
     
     irq
     ;; IRQ handler
     (rti))))

