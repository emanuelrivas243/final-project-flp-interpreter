#lang eopl
(require racket/list)

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <string>
;;                  ::= 
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;                  ::= var {<identifier> = <expression>}*(,) in <expression>
;;                      <var-exp (id expVal varBody)>
;;                  ::= const {<identifier> = <expression>}*(,) in <expression>
;;                      <const-exp (id expConst constBody)>
;;                  ::= rec {<identifier> ( {<identifier>} *(,) ) = <expression>}* in <expression>
;;                      <rec-exp (id param expRec recBody)>
;;                  ::= <cadena>
;;                      (a-string <identifier>)
;;                  ::= <bool>
;;                      (true-exp ())
;;                      (false-exp ())


;;  <circuit>       ::= <gate_list>
;;
;;  <gate_list>     ::= empty
;;                  ::= <gate> <gate_list>
;;
;;  <gate>          ::= <identifier> <type> <input_list>
;;
;;  <type>          ::= and | or | not | xor
;;
;;  <input_list>    ::= empty
;;                  ::= <bool> <input_list>
;;                  ::= <identifier> <input_list>
;;
;;  <bool>          ::= True | False
;;
;;  <primitive>     ::= + | - | * | add1 | sub1
;;                  ::= eval-circuit | connect-circuits | merge-circuits

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (numberInt
   (digit (arbno digit)) number)
  (numberInt
   ("-" digit (arbno digit)) number)
  (numberFloat
   (digit (arbno digit) "." digit (arbno digit)) number)
  (numberFloat
   ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (numberInt) int-exp)
    ;(expression (identifier) a-string)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression (identifier) var-exp)

    ;; Expresiones Definiciones
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression ";") vardecl-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression) 
                letrec-exp)
    ;(expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-exp)
    ;(expression ("rec" identifier "(" (arbno (separated-list identifier ","))  expression "in" expression) rec-exp)

    
    ;; Constructores de Datos Predefinidos
    (expression ("lista" "(" (separated-list expression ",") ")") list-exp)
    (expression ("lista()") emptylist-exp)
    (expression (primitive-list "(" (arbno expression) ")") prim-list-exp)
    
    (expression ("tupla" "(" (separated-list expression ",") ")") tuple-exp)
    
    ;(expression ("tupla[]") emptytuple-exp)
    
    (expression
     (pred-prim "(" expression "," expression ")" ) comparison-prim-exp)

    (expression
     (oper-bin-bool "(" expression "," expression ")") bool-binop-exp)
    
    (pred-prim ("<") less-pred-prim)
    (pred-prim (">") greater-pred-prim)
    (pred-prim ("<=") less-equal-pred-prim)
    (pred-prim (">=") greater-equal-pred-prim)
    (pred-prim ("==") equal-pred-prim)
    (pred-prim ("!=") not-equal-pred-prim)
    
    (expression
     (oper-un-bool "(" expression ")") bool-uniop-exp)
    
    (oper-bin-bool ("and") and-op-bool)
    (oper-bin-bool ("or") or-op-bool)
    (oper-un-bool ("not") not-op-bool)

    ;; Primitivas sobre listas -> Crear un eval-list para este caso, ya que es un nuevo datatype
    ;; ejemplo: vacio?(lista(1,2,3)) -> #f ;|; vacio?(lista()) -> #t
    
    (primitive-list ("vacio?") vacio?-prim-list)
    ;(primitive-list ("vacio") vacio-prim-list)
    ;(primitive-list ("crear-lista") crear-prim-list)
    
    (primitive-list ("lista?") lista?-prim-list)
    (primitive-list ("cabeza") cabeza-prim-list)
    (primitive-list ("cola") cola-prim-list)
    (primitive-list ("append") append-prim-list)
    ;(primitive-list ("ref-list") ref-list-prim)
    (primitive-list ("set-list") set-list-prim)
    
    
    ;; 
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    
    (expression ( "(" expression (arbno expression) ")")
                app-exp)


    ; características adicionales
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                set-exp)

    (expression (circuit) circuit-exp)


    ;; Definiciones

    ;; Circuit
    (circuit ("circuit" "(gate_list" gate_list ")") a-circuit)

    ;; GateList
    (gate_list () empty-gate-list)
    (gate_list ("(" gate ")" gate_list) a-gate-list)

    ;; Gate
    (gate ("gate" identifier "(type" type ")" "(input_list" input_list ")") a-gate)

    ;; InputList
    (input_list () empty-input-list)
    (input_list (bool input_list) bool-input-list)
    (input_list (identifier input_list) gateref-input-list)

    ;; Bool
    (bool ("True") true-bool)
    (bool ("False") false-bool)
    

    ;; Type
    (type ("and") and-type)
    (type ("or") or-type)
    (type ("not") not-type)
    (type ("xor") xor-type)


    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)))



;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (proc-names (list-of symbol?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?))
;  (begin-exp
;   (exp expression?)
;   (exps (list-of expression?)))
;  (set-exp
;   (id symbol?)
;   (rhs expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     '(const const var)
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (int-exp (datum) datum)
      ;(a-string (str) str)
      (true-exp () #t)
      (false-exp () #f)
      
      (var-exp (id) (apply-env env id))
      (vardecl-exp (ids expVar varBody)
                   (let ((args (eval-rands expVar env)))
                     (let ((tags (repeat-var-tags (length ids))))
                       (eval-expression varBody (extend-env ids args tags env)))))

      ;(const-exp (id expVal constBody) (list id expVal constBody))
      (list-exp (elements) (list->vector (eval-rands elements env) ))
      (emptylist-exp () (list))
      (prim-list-exp (prim lst) (eval-list-prim prim (list->vector (eval-rands lst env)) env))

      (tuple-exp (elements) 'porimplementar)
      ;(emptytuple-exp () (values))
      
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (eval-expression (car exps) 
                                              env)
                             (cdr exps)))))

      ;; Expresiones booleanas
      (comparison-prim-exp (pred exp1 exp2)
                           (eval-expr-bool pred (list (eval-rand exp1 env) (eval-rand exp2 env))))

      (bool-binop-exp (binop expr-bool1 expr-bool2)
                      (eval-expr-bool-binop binop (list (eval-rand expr-bool1 env) (eval-rand expr-bool2 env))))

      (bool-uniop-exp (uniop expr-bool) (eval-expr-bool-uniop uniop (eval-rand expr-bool env)))
      
      (circuit-exp (circ) (eval-circuit circ env)))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define (repeat-var-tags n)
  (if (= n 0)
      '()
      (cons 'var (repeat-var-tags (- n 1)))))


;apply-primitive: <primitiva> <list-of-expression> -> numero

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      
      ;; Primitivas adicionales
      #|
         Primitiva: eval-circuit-prim
         Llega un ambiente en args, que contiene los gates ya evaluados con sus valores de verdad,
         así que se usa el datatype de environment para procesarlo por casos.
         Se retorna el valor de verdad de el ultimo gate evaluado desde el ambiente.
      |#

      (eval-circuit-prim ()
                         (let* ((ambientes (car args))                      
                                (ultimo-env (car (last ambientes)))) ; el último ambiente extendido
                           (cases environment ultimo-env
                             (empty-env-record ()
                                               '())
                             (extended-env-record (syms vals type-val parent-env)
                                                  (if (null? vals)
                                                      '()
                                                      (car vals))))))
      
      (connect-circuits-prim () 
                             (apply connect-circuits args))
      
      (merge-circuits-prim () 
                           (apply merge-circuits args)))))

#|
Funcion auxiliar: last

Devuelve el último elemento de una lista.
Se usa para obtener el último valor evaluado de una lista,
útil cuando se encadenan evaluaciones o se quiere el valor final.

Ejemplos:
(last '(1 2 3 4))   ; => 4
(last '(#t #f #t))  ; => #t
|#

(define last
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (last (cdr lst)))))

#|
Funcion auxiliar: xor

Implementa la operación lógica "exclusive or" (XOR).
Retorna #t si exactamente uno de los dos valores es verdadero.

Ejemplos:
(xor #t #f)  ; => #t
(xor #t #t)  ; => #f
|#

(define (xor a b)
  (or (and a (not b))
      (and (not a) b)))

#|
Función Eval-Circuit
Permite recibir la sintaxis abstracta de circ y procesarla,
para así enviarlo a evaluar en eval-gate-list

|#

(define eval-circuit
  (lambda (circ env)
    (cases circuit circ
      (a-circuit (gl) (eval-gate-list gl env)))))

#|
Función: eval-gate-list

Evalúa una lista de compuertas recursivamente.
Cada compuerta gate es enviada a eval-gate, una vez evaluado
se guarda en una variable res, el resto de compuertas
usan el nuevo ambiente generado por res. El entorno se actualiza
a medida que se van generando nuevas compuertas.

|#

(define eval-gate-list
  (lambda (gl env)
    (cases gate_list gl
      (empty-gate-list () '())
      (a-gate-list (g rest)
                   (let* ((res (eval-gate g env)))
                     (cons res (eval-gate-list rest (car res))))))))

#|
Funcion: Eval-gate

Evalúa una compuerta gate individual.
Detecta su tipo (and, or, not, xor), evalúa sus entradas
y calcula el valor lógico correspondiente.

Luego, extiende el entorno para que futuras compuertas
puedan usar esta como entrada.

|#

;; Punto principal del taller, donde se junta todo
(define eval-gate
  (lambda (g env)
    (cases gate g
      (a-gate (id gate_type input-list)
              (let* ((tipo (eval-type gate_type))
                     (entradas (eval-input-list input-list env)))
                (let* ((valor (cond
                                [(eq? tipo 'and) (and (car entradas) (cadr entradas))]
                                [(eq? tipo 'or)  (or  (car entradas) (cadr entradas))]
                                [(eq? tipo 'xor) (xor (car entradas) (cadr entradas))]
                                [(eq? tipo 'not) (not (car entradas))]))
                       (nuevo-env (extend-env (list id) (list valor) env)))
                  (list nuevo-env)))))))

#|
Función: eval-type

Convierte un tipo de compuerta desde su forma de estructura
a un símbolo identificador ('and, 'or, 'xor, 'not)

Se utiliza dentro de eval-gate para determinar qué operación utilizar

|#

(define eval-type
  (lambda (t)
    (cases type t
      (and-type () 'and)
      (or-type ()  'or)
      (xor-type () 'xor)
      (not-type () 'not))))


#|
Funcion: eval-input-list

Evalúa la lista de entradas (inputs de inpyt_list) de una compuerta.
En caso de ser una referencia a otra compuerta o un identificador
va y lo busca en el ambiente.
Puede ser una lista de valores booleanos explícitos (true/false)
o referencias a otras compuertas ya evaluadas.

Convierte todos los valores a booleanos con la función num->bool.

|#

(define eval-input-list
  (lambda (il env)
    (cases input_list il
      (empty-input-list () '())
      (bool-input-list (b rest)
                       (cons (eval-bool b) (eval-input-list rest env)))
      (gateref-input-list (id rest)
                          (let* ((val (apply-env env id))
                                 (bool-val (num->bool val))
                                 (rest-eval (eval-input-list rest env)))
                            (cons bool-val rest-eval))))))

#|
Función auxiliar: num->bool

Convierte números a booleanos en el lenguaje.
1 representa verdadero (#t), -1 representa falso (#f).
También retorna el valor tal cual si ya es booleano.

Ejemplos:
(num->bool 1)     ; => #t
(num->bool -1)    ; => #f
(num->bool #t)    ; => #t

|#


(define (num->bool n)
  (cond
    [(number? n)
     (cond
       [(= n 1) #t]
       [(= n -1) #f])]
    [(boolean? n) n]))


#|
Funcion: eval-bool

Evalúa un valor booleano explícito en la entrada del circuito.
Se usa dentro de eval-input-list cuando las entradas se definen
como true o false directamente.

|#

(define eval-bool
  (lambda (b)
    (cases bool b
      (true-bool () #t)
      (false-bool () #f))))



;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;(define true-value?
 ; (lambda (v)
  ;  (cond
   ;   ((= v 1) #t)
    ;  ((= v -1) #f)
     ; (else #t))))




;;==========================================================
;; Implementación de connect-circuits y merge-circuits
;;==========================================================


;; Función connect-circuits: conecta dos circuitos en serie através de append-gatelist-lists
(define (connect-circuits circuit1 circuit2)
  (let ((gate-list1 (cases circuit circuit1
                      (a-circuit (gl) gl)))
        (gate-list2 (cases circuit circuit2
                      (a-circuit (gl) gl))))
    (a-circuit (append-gate-lists gate-list1 gate-list2))))

;; Función auxiliar para concatenar dos gate-lists conectando el final de la primera con el principio de la segunda
(define (append-gate-lists gl1 gl2)
  (cases gate_list gl1
    (empty-gate-list () gl2)
    (a-gate-list (g rest)
                 (a-gate-list g (append-gate-lists rest gl2)))))

;; Función merge-circuits: combina dos circuitos en paralelo con una compuerta
(define (merge-circuits circuit1 circuit2 gate-type)
  (let* ((circuit1-gates (cases circuit circuit1
                           (a-circuit (gl) gl)))
         (circuit2-gates (cases circuit circuit2
                           (a-circuit (gl) gl)))
         (last-gate1 (get-last-gate circuit1-gates))
         (last-gate2 (get-last-gate circuit2-gates))
         (new-gate-id (string->symbol (string-append "MERGED_" (symbol->string gate-type)))))
    
    ;; Crear una nueva compuerta que combine las salidas
    (a-circuit
     (append-gate-lists 
      circuit1-gates
      (append-gate-lists 
       circuit2-gates
       (a-gate-list
        (a-gate new-gate-id 
                (case gate-type
                  ((and-type) (and-type))
                  ((or-type) (or-type))
                  ((xor) (xor-type))
                  ((not) (not-type)))
                (gateref-input-list 
                 (cases gate last-gate1
                   (a-gate (id type il) id))
                 (cases gate last-gate2
                   (a-gate (id type il) id)))
                (empty-gate-list))))))))

;; Función auxiliar para obtener la última compuerta de una gate-list
#|

Ejemplos de uso:

(define gl1
  (gate_list
    (gate G1 (type or) (input_list 1 -1))
    (gate_list
      (gate G2 (type and) (input_list G1 1))
      (empty-gate-list))))

(get-last-gate gl1)
;; -> G2


(define gl2
  (gate_list
    (gate G7 (type not) (input_list -1))
    (empty-gate-list)))

(get-last-gate gl2)
;; -> G7

|#


#|

Ejemplos de uso:

(define gl1
  (gate_list
    (gate G1 (type or) (input_list 1 -1))
    (gate_list
      (gate G2 (type and) (input_list G1 1))
      (empty-gate-list))))

(get-last-gate gl1)
;;G2 -> (gate G2 (type and) (input_list G1 1))


(define gl2
  (gate_list
    (gate G7 (type not) (input_list -1))
    (empty-gate-list)))

(get-last-gate gl2)
;; G7 -> (gate G7 (type not) (input_list -1))



|#
(define (get-last-gate gl)
  (cases gate_list gl
    (empty-gate-list () (eopl:error 'get-last-gate "Empty gate list"))
    (a-gate-list (g rest)
                 (if (cases gate_list rest
                       (empty-gate-list () #t)
                       (a-gate-list (g r) #f))
                     g
                     (get-last-gate rest)))))
#|
let
a = 1
b = 1
in
let
c1 = circuit(gate_list(gate G1 (type not) (input_list a)))
c2 = circuit(gate_list(gate G2 (type and) (input_list a b)))
connected = connect-circuits(c1, c2)
in
eval-circuit(connected)
|#


;; eval-list-prim
(define eval-list-prim
  (lambda (prim args env)
    (cases primitive-list prim
      (vacio?-prim-list () (null? (car args)))
      (lista?-prim-list () (list? (car args)))
      (cabeza-prim-list () (vector-ref (vector-ref args 0) 0));(caar args))
      (cola-prim-list () (cdr (car args)))
      (append-prim-list () (append-aux args))
      ; set-list(<lista> <posicion> <valor-nuevo>)
      ;(ref-list-prim () )
      (set-list-prim ()
                     (let ((lst (car args))
                           (pos (cadr args))
                           (new-value (caddr args)))
                       (list-set lst pos new-value)))
      
      )))

; Función auxiliar para recorrer la lista de listas y hacerles append
(define append-aux
  (lambda (args)
    (if (null? args)
        '()
        (append (car args) (append-aux (cdr args))))))

;; eval-expr-bool
(define eval-expr-bool
  (lambda (pred args)
    (cases pred-prim pred
      (less-pred-prim () (< (car args) (cadr args)))
      (greater-pred-prim () (> (car args) (cadr args)))
      (less-equal-pred-prim () (<= (car args) (cadr args)))
      (greater-equal-pred-prim () (>= (car args) (cadr args)))
      (equal-pred-prim () (= (car args) (cadr args)))
      (not-equal-pred-prim () (not (= (car args) (cadr args))))
      )))

;; eval-expr-bool-binop
(define eval-expr-bool-binop
  (lambda (binop args)
    (cases oper-bin-bool binop
      (and-op-bool () (and (car args) (cadr args)))
      (or-op-bool () (or (car args) (cadr args)))
      )))

(define eval-expr-bool-uniop
  (lambda (uniop args)
    (cases oper-un-bool uniop
      (not-op-bool () (not args))
      )))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args (list 'closure) env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (vec-types vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals types-vals env)
    (extended-env-record syms (list->vector vals) (list->vector types-vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;(define iota
;  (lambda (end)
;    (iota-aux 0 end)))
;
;(define iota-aux
;  (lambda (ini fin)
;    (if (>= ini fin)
;        ()
;        (cons ini (iota-aux (+ 1 ini) fin)))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))


(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals type-val env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals type-val)
                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (vec-type? vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec vec-type)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec vec-type)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Pruebas

#|
(interpretador)
--> var 
x = 1, 
y = 2 in 
+(x,y);


var x = lista(1,2,3) 
in x;
(1 2 3)

var x = lista() in x;
()


lista?
lista?(lista()) -> #t
lista?(lista(1,2,3)) -> #t


cabeza(lista(1,2,3)) -> 1


append(lista(1,2,3) lista(5,6)) -> (1 2 3 5 6)


var x = lista(1,2,3) in 
begin 
set x = lista(4,3); 
x 
end 
;
(4 3) -> Preguntar sobre esta parte, si es redundante ref-list y set-list ya que eso ya existe


<(5,3)
#f

>(5,3)
#t

>=(5,3)
#t


<=(5,3)
#f

<=(0,0)
#t

 <=(2,0)
#f

==(24,3)
#f

!=(2,0)
#t

and( <(5,3) , >(8,4) )


--> and( <(5,3) , >(8,4) )
#f
--> or( <(5,3) , >(8,4) )
#t


not( <(4,3) ) -> #t


|#



#|
(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let
x = -(y,1)
in
let
x = +(x,2)
in
add1(x)")

(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-ident (var-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (var-exp 'v)
                                                                    (var-exp 'y)))
                                                 (var-exp 'x)
                                                 (lit-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))
|#

(interpretador)