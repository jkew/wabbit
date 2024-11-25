; ########################################
; # AST INPUT
; ########################################
;AssignDeclare(UnscopedVar("a", U), IntegerLiteral(2))
;AssignDeclare(UnscopedVar("x", U), FloatLiteral(2.1))
;AssignDeclare(UnscopedVar("y", U), FloatLiteral(3.4))
;AssignDeclare(UnscopedVar("z", U), Binary(Add, [UnscopedVar("x", U), UnscopedVar("y", U)], U))
;print UnscopedVar("z", U)
;print UnscopedVar("x", U) Sub UnscopedVar("y", U)
;print UnscopedVar("x", U) Mul UnscopedVar("y", U)
;print UnscopedVar("x", U) Div UnscopedVar("y", U)
;func f F (UnscopedVar("a", F)){
;    AssignDeclare(UnscopedVar("c", U), Binary(Add, [UnscopedVar("a", U), UnscopedVar("a", U)], U))
;    return UnscopedVar("c", U)
;}
; ########################################
; # AST BEFORE STAGE: ✅ Types Phase 1: infer types on assignments
; ########################################
;alloc U global a U
;alloc U global x U
;alloc U global y U
;alloc U global z U
;func f F (local_1 a F){
;    # scope 1
;    alloc U local_1 c U
;    local_1 c U = local_1 a U Add local_1 a U
;    return local_1 c U
;    # end scope 1
;}
;func main I (){
;    global a U = 2i
;    global x U = 2.1f
;    global y U = 3.4f
;    global z U = global x U Add global y U
;    print global z U
;    print global x U Sub global y U
;    print global x U Mul global y U
;    print global x U Div global y U
;    return 0i
;}
; ########################################
; # AST BEFORE STAGE: ✅ Types Phase 2: set types on variables
; ########################################
;alloc U global a U
;alloc U global x U
;alloc U global y U
;alloc U global z U
;func f F (local_1 a F){
;    # scope 1
;    alloc U local_1 c U
;    local_1 c U = local_1 a U Add local_1 a U
;    return local_1 c U
;    # end scope 1
;}
;func main I (){
;    global a I = 2i
;    global x F = 2.1f
;    global y F = 3.4f
;    global z U = global x U Add global y U
;    print global z U
;    print global x U Sub global y U
;    print global x U Mul global y U
;    print global x U Div global y U
;    return 0i
;}
; ########################################
; # AST BEFORE STAGE: ✅ Types Phase 3: infer types on assignments
; ########################################
;alloc U global a I
;alloc U global x F
;alloc U global y F
;alloc U global z U
;func f F (local_1 a F){
;    # scope 1
;    alloc U local_1 c U
;    local_1 c U = local_1 a F Add local_1 a F
;    return local_1 c U
;    # end scope 1
;}
;func main I (){
;    global a I = 2i
;    global x F = 2.1f
;    global y F = 3.4f
;    global z U = global x F Add global y F
;    print global z U
;    print global x F Sub global y F
;    print global x F Mul global y F
;    print global x F Div global y F
;    return 0i
;}
; ########################################
; # AST BEFORE STAGE: ✅ Types Phase 4: set types on remaining statements
; ########################################
;alloc U global a I
;alloc U global x F
;alloc U global y F
;alloc U global z U
;func f F (local_1 a F){
;    # scope 1
;    alloc U local_1 c U
;    local_1 c F = local_1 a F Add local_1 a F
;    return local_1 c U
;    # end scope 1
;}
;func main I (){
;    global a I = 2i
;    global x F = 2.1f
;    global y F = 3.4f
;    global z F = global x F Add global y F
;    print global z U
;    print global x F Sub global y F
;    print global x F Mul global y F
;    print global x F Div global y F
;    return 0i
;}
; ########################################
; # AST BEFORE STAGE: ✅ Types Phase 5: set types on variables
; ########################################
;alloc I global a U
;alloc F global x U
;alloc F global y U
;alloc F global z U
;func f F (local_1 a F){
;    # scope 1
;    alloc F local_1 c U
;    local_1 c F = local_1 a F Add local_1 a F
;    return local_1 c U
;    # end scope 1
;}
;func main I (){
;    global a I = 2i
;    global x F = 2.1f
;    global y F = 3.4f
;    global z F = global x F Add global y F
;    print global z U
;    print global x F Sub global y F
;    print global x F Mul global y F
;    print global x F Div global y F
;    return 0i
;}
; ########################################
; # AST BEFORE STAGE: ✅ create llvm ir
; ########################################
;alloc I global a I
;alloc F global x F
;alloc F global y F
;alloc F global z F
;func f F (local_1 a F){
;    # scope 1
;    alloc F local_1 c F
;    local_1 c F = local_1 a F Add local_1 a F
;    return local_1 c F
;    # end scope 1
;}
;func main I (){
;    global a I = 2i
;    global x F = 2.1f
;    global y F = 3.4f
;    global z F = global x F Add global y F
;    print global z F
;    print global x F Sub global y F
;    print global x F Mul global y F
;    print global x F Div global y F
;    return 0i
;}
; ########################################
; # LLVM IR
; ########################################
@.a = global i32 0                       ; Alloc(GlobalVar("a", I), I)           
@.x = global double 0.000000e+00         ; Alloc(GlobalVar("x", F), F)           
@.y = global double 0.000000e+00         ; Alloc(GlobalVar("y", F), F)           
@.z = global double 0.000000e+00         ; Alloc(GlobalVar("z", F), F)           
define i32 @f(i32 %.0_a1) {              ; Fn("f", [ScopedVar(1, "a", F)]        
    %.1_a = alloca i32                                                           
    store i32 %.0_a1, i32* %.1_a                                                 
                                         ; Scope(1)                              
    %.1_c = alloca double                ; Alloc(ScopedVar(1, "c", F), F)        
    %.3 = load double, double* %.1_a     ; ScopedVar(1, "a", F)                  
    %.4 = load double, double* %.1_a     ; ScopedVar(1, "a", F)                  
    %.2 = fadd double %.3, %.4           ; Binary(Add, [ScopedVar(1, "a",        
    store i32 %.2, i32* %.1_c            ; Assign(ScopedVar(1, "c", F), B        
    %.5 = load double, double* %.1_c     ; ScopedVar(1, "c", F)                  
    ret i32 %.5                          ; Return(ScopedVar(1, "c", F))          
                                         ; EndScope(1)                           
}                                                                                
define i32 @main() {                     ; Fn("main", [], [Assign(GlobalV        
    %.6 = alloca i32                                                             
    store i32 2, i32* %.6                ; IntegerLiteral(2)                     
    %.7 = load i32, i32* %.6                                                     
    store i32 %.7, i32* @.a              ; Assign(GlobalVar("a", I), Inte        
    %.8 = alloca double                                                          
    store double 2.1, double* %.8        ; FloatLiteral(2.1)                     
    %.9 = load double, double* %.8                                               
    store i32 %.9, i32* @.x              ; Assign(GlobalVar("x", F), Floa        
    %.10 = alloca double                                                         
    store double 3.4, double* %.10       ; FloatLiteral(3.4)                     
    %.11 = load double, double* %.10                                             
    store i32 %.11, i32* @.y             ; Assign(GlobalVar("y", F), Floa        
    %.13 = load double, double* @.x      ; GlobalVar("x", F)                     
    %.14 = load double, double* @.y      ; GlobalVar("y", F)                     
    %.12 = fadd double %.13, %.14        ; Binary(Add, [GlobalVar("x", F)        
    store i32 %.12, i32* @.z             ; Assign(GlobalVar("z", F), Bina        
    %.15 = load double, double* @.z                                              
    call double (double) @_print_double(double @.z)                                         
    %.17 = load double, double* @.x      ; GlobalVar("x", F)                     
    %.18 = load double, double* @.y      ; GlobalVar("y", F)                     
    %.16 = fsub double %.17, %.18        ; Binary(Sub, [GlobalVar("x", F)        
    call i32 (i32) @_print_int(i32 %.16)                                         
    %.20 = load double, double* @.x      ; GlobalVar("x", F)                     
    %.21 = load double, double* @.y      ; GlobalVar("y", F)                     
    %.19 = fmul double %.20, %.21        ; Binary(Mul, [GlobalVar("x", F)        
    call i32 (i32) @_print_int(i32 %.19)                                         
    %.23 = load double, double* @.x      ; GlobalVar("x", F)                     
    %.24 = load double, double* @.y      ; GlobalVar("y", F)                     
    %.22 = fdiv double %.23, %.24        ; Binary(Div, [GlobalVar("x", F)        
    call i32 (i32) @_print_int(i32 %.22)                                         
    %.25 = alloca i32                                                            
    store i32 0, i32* %.25               ; IntegerLiteral(0)                     
    %.26 = load i32, i32* %.25                                                   
    ret i32 %.26                         ; Return(IntegerLiteral(0))             
}                                                                                
declare i32 @_print_int(i32)                                                     
declare i32 @_print_double(double)                                               
