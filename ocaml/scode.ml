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

module Eval =
  struct
      let expr = ref (Obj.magic () : word)
      let value = ref (Obj.magic () : word)
      let env = ref (Obj.magic () : word) 
      let args = ref (Obj.magic () : word)
      let stack = ref (Obj.magic () : word)        
      
  end
