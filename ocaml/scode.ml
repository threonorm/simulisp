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
     val fetch_car : ptr -> dataword
     val fetch_cdr : ptr -> dataword
     val alloc_cons : dataword -> dataword -> ptr
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
    let alloc_cons car cdr = (Data(car),Data(cdr))
    let fetch_car = fetch_word
    let fetch_cdr (_, Data cdr) = cdr    
  end

module Eval : 
  sig
      val eval : unit -> unit 
      val expr : ref dataword
      val value : ref dataword 
      val env : ref dataword
      val args : ref dataword
      val stack : ref dataword
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
      
      let rec evaldata () = match (!expr) with
        | Nil          -> value := !expr;
        (*TODO: Dispatch on stack?*) 
        | Local(a,b)   -> value := walk_on_list a b (!env)   
        (*TODO: Dispatch on stack?*) 
        | Symb(s)      -> value := !expr;
        (*TODO: Dispatch on stack?*) 
        | Closure(ptr) -> env   := List(
                                  alloc_cons !args (snd (Gc.fetch_cell !expr))
                                       );
                          expr  := Gc.fetch_car (Gc.fetch_car ptr);
                          eval()   
        | Cond(ptr)    -> (
                          if !value = Nill 
                          then 
                             expr := Gc.fetch_cdr ptr 
                          else 
                             expr := Gc.fetch_car ptr
                          );
                          eval ()
        | List(ptr)    -> value := !expr
        (*TODO: Dispatch on stack?*) 
        | Num(vint)    -> value := !expr
        (*TODO: Dispatch on stack?*) 
        | Proc(ptr)    -> value := Closure(alloc_cons (!exp) (!env)) ;
        (*TODO: Dispatch on stack?*) 
        | Call(ptr)    ->
        | Quote(ptr)   -> failwith("No implementation"); 
  end

