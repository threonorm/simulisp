(* We rely on OCaml's runtime to garbage collect for us*)
module Gc : 
  sig
     type ptr
     type word = 
        | Nil
        | Local of int*int
        | Symb of string
        | Closure of ptr
        | Cond of ptr
        | List of ptr
        | Num of int
        | Proc of ptr
        | Quote of ptr 
        | First of ptr
        | Next of ptr 
        | Sequence of ptr
        | Primitive of prim
     and prim =
        | Car 
        | Cdr 
        | Cons
        | Incr
        | Decr
        | Zero 
        | Atom
       (* | Progn*)
     val fetch_word : ptr -> word
     val fetch_cell : ptr -> word * word
     val fetch_car : ptr -> word
     val fetch_cdr : ptr -> word
     val alloc_cons : word -> word -> ptr
  end

    =   
  struct 
    type ptr = word * word 
    and word = 
        | Nil
        | Local of int*int
        | Symb of string
        | Closure of ptr
        | Cond of ptr
        | List of ptr
        | Num of int
        | Proc of ptr
        | Quote of ptr 
        | First of ptr
        | Next of ptr 
        | Sequence of ptr
        | Primitive of prim
    and prim = 
        | Car 
        | Cdr 
        | Cons
        | Incr
        | Decr
        | Zero 
        | Atom
        (*| Progn*)
    let fetch_word (car,_) = car
    let fetch_cell (car, cdr) = (car, cdr) 
    let alloc_cons car cdr = ((car),(cdr))
    let fetch_car = fetch_word
    let fetch_cdr (_, cdr) = cdr    
  end

module Eval : 
  sig
      val eval  : unit -> unit 
      val expr  : Gc.word ref
      val value : Gc.word ref
      val env   : Gc.word ref
      val args  : Gc.word ref
      val stack : Gc.word ref
  end
   =
  struct
      let expr  = ref (Obj.magic ()) 
      let value = ref (Obj.magic ())
      let env   = ref (Obj.magic ())
      let args  = ref (Obj.magic ())
      let stack = ref (Obj.magic ()) 
      let rec walk_on_list a b flag  = function
        | Gc.List(ptr) -> let (car,cdr) = Gc.fetch_cell ptr in
                           if flag then 
                              if b=0 then car
                              else walk_on_list a (b-1) flag cdr
                           else 
                              if a=0 then walk_on_list a b true car
                              else walk_on_list (a-1) b flag cdr
                              
        | _ -> assert(false)  
      let get_ptr = function
        | Gc.Closure( ptr)
        | Gc.Cond (ptr)
        | Gc.List( ptr)
        | Gc.Proc( ptr)
        | Gc.Quote( ptr) 
        | Gc.First( ptr)
        | Gc.Next( ptr) 
        | Gc.Sequence( ptr) -> ptr
        | _ -> assert(false)
    

  
      let rec eval () = match (!expr) with
        | Gc.Nil          -> value := !expr;
        | Gc.Local(a,b)   -> value := walk_on_list a b false (!env)   
        | Gc.Symb(s)      -> value := !expr;
        | Gc.Closure(ptr) -> env   := Gc.List(
                                  Gc.alloc_cons !args (snd (Gc.fetch_cell ptr))
                                       );
                          expr  := Gc.fetch_car(get_ptr (Gc.fetch_car ptr));
                          eval()   
        | Gc.Cond(ptr)    -> (
                          if !value = Gc.Nil 
                          then 
                             expr := Gc.fetch_cdr ptr 
                          else 
                             expr := Gc.fetch_car ptr
                          );
                          eval ()
        | Gc.List(ptr)    -> value := !expr
        | Gc.Num(vint)    -> value := !expr
        | Gc.Proc(ptr)    -> value := Gc.Closure(Gc.alloc_cons (!expr) (!env)) 
        
        | Gc.First(ptr)   -> let blah = !env 
                          and blup = Gc.fetch_cdr (ptr) in
                          expr := Gc.fetch_car (ptr);
                          eval ();
                          expr := blup ;
                          env  := blah ;
                          args := Gc.List(Gc.alloc_cons !value Gc.Nil); 
                          eval()
        | Gc.Next(ptr)    -> let blah = !env 
                          and blup = Gc.fetch_cdr (ptr) in
                          expr := Gc.fetch_car (ptr);
                          eval ();
                          expr := blup ;
                          env  := blah ;
                          args := Gc.List(Gc.alloc_cons (!value) (!args)); 
                          eval ()
        
        | Gc.Primitive(prim) ->
          begin
            match prim with
              | Gc.Car   -> value := Gc.fetch_car (get_ptr (!value))
              | Gc.Cdr   -> value := Gc.fetch_cdr (get_ptr (!value))
              | Gc.Cons  -> let (car,Gc.List cdr) = Gc.fetch_cell (get_ptr (!value)) in
                         let cadr= Gc.fetch_car cdr in
                         value := Gc.List(Gc.alloc_cons cadr car);
              | Gc.Incr  -> let (Gc.Num a) = !value in
                         value := Gc.Num (a+1)
              | Gc.Decr  -> let (Gc.Num a) = !value in
                         value := Gc.Num (a-1)
              | Gc.Zero  -> let (Gc.Num a) = !value in
                         if a=0 then value := Gc.Symb("T")
                         else value := Gc.Nil
              | Gc.Atom  -> assert(false)
      
          end         
        | Gc.Sequence(ptr) -> value := Gc.Nil ;
                           let cdr = Gc.fetch_cdr ptr in
                           let blah = !env in
                           expr := Gc.fetch_car (ptr);
                           eval (); 
                           expr := cdr;
                           env  := blah ;
                           eval ();
        | Gc.Quote(ptr)   -> failwith("No implementation"); 
  end

