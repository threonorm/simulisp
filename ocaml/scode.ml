(* We rely on OCaml's runtime to garbage collect for us*)
module Gc : 
  sig
     type ptr
     type dataword = 
        | Nil
        | Local of int*int
        | Symb of string
        | Closure of ptr
        | Cond of ptr
        | List of ptr
        | Num of int
        | Proc of ptr
        | Call of ptr
        | Quote of ptr 
     type procword = 
        | More of ptr
        | Funcall of ptr
        | Car 
        | Cdr 
        | Cons
        | Incr
        | Decr
        | Zero 
        | Atom
        | Progn
        | List
     val fetch_word : ptr -> dataword
     val fetch_cell : ptr -> dataword * dataword
     val fetch_proc_cell : ptr-> dataword*procword
  end
    =   
  struct 
    type ptr = word * word 
    type word = 
        | Data of dataword
        | Proc of procword
    and dataword = 
        | Nil
        | Local of int*int
        | Symb of string
        | Closure of ptr
        | Cond of ptr
        | List of ptr
        | Num of int
        | Proc of ptr
        | Call of ptr
        | Quote of ptr 
    and procword = 
        | More of ptr
        | Funcall of ptr
        | Car 
        | Cdr 
        | Cons 
        | Atom
        | Progn
        | List
    let fetch_word (Data car,_) = car
    let fetch_cell (Data car, Data cdr) = (car, cdr) 
    let fetch_proc_cell (Data car, Proc cdr) = (car,cdr)          
  end

module Eval : 
  sig
      val eval : unit -> unit 
      val expr : ref word
      val value : ref word 
      val env : ref word
      val args : ref word
      val stack : ref word
  end
   =
  struct
      let expr  = ref Obj.magic () 
      let value = ref Obj.magic ()
      let env   = ref Obj.magic () 
      let args  = ref Obj.magic () 
      let stack = ref Obj.magic () 
      let rec walk_on_list a b flag  = function
        | List(ptr) -> let (car,cdr) = Gc.fetch_cell ptr in
                           if flag then 
                              if b=0 then car
                              else walk_on_list a (b-1) flag cdr
                           else 
                              if a=0 then walk_on_list a b true car
                              else walk_on_list (a-1) b flag cdr
                              
        | _ -> assert(false)  

      let eval () = match (!expr) with
        | Nil          -> value := !expr
        | Local(a,b)   -> value := walk_on_list a b (!env)   
        | Symb(s)      -> value := !expr
        | Closure(ptr) ->
        | Cond(ptr)    ->
        | List(ptr)    -> value := !expr
        | Num(vint)    -> value := !expr
        | Proc(ptr)    ->
        | Call(ptr)    ->
        | Quote(ptr)   -> 
             
  end

