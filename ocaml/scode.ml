(* structured assembly in which to write s-code *)

type prim = Car 
          | Cdr 
          | Cons
          | Incr
          | Decr
          | Isgt60
          | Zero 
          | Atom
          | Div60
          | Mod60
          | Mod24

module Asm = struct

  type asm = Nil
           | Local of int * int
           | Global of string
           | Symb of string
           | Cond of asm * asm
           | List of asm * asm
           | Num of int
           | Proc of asm
           | First of asm * asmargs
           | Sequence of asm * asm
  and asmargs = Next of asm * asmargs
              | CallPrim of prim
              | CallFun of asm
          
end

(* We rely on OCaml's runtime to garbage collect for us*)
module Gc : 
  sig
     type ptr = word * word
     and word = 
        | Nil
        | Local of int*int
        | Global of string
        | Symb of string
        | Closure of ptr
        | Cond of ptr
        | List of ptr
        | Num of int
        | Proc of ptr
        | First of ptr
        | Next of ptr
        | Last of ptr
        | ApplyOneArg of ptr 
        | Sequence of ptr
        | Primitive of prim
        | ReturnTag of (unit -> unit) * ptr

     val fetch_word : ptr -> word
     val fetch_cell : ptr -> word * word
     val fetch_car : ptr -> word
     val fetch_cdr : ptr -> word
     val alloc_cons : word -> word -> ptr

     val get_global : string -> word
     val assemble_and_load : (string * Asm.asm) list -> unit
  end

    =   
  struct 
    type ptr = word * word
    and word = 
        | Nil
        | Local of int*int
        | Global of string
        | Symb of string
        | Closure of ptr
        | Cond of ptr
        | List of ptr
        | Num of int
        | Proc of ptr
        | First of ptr
        | Next of ptr
        | Last of ptr
        | ApplyOneArg of ptr
        | Sequence of ptr
        | Primitive of prim
        | ReturnTag of (unit -> unit) * ptr

    let global_mem = Hashtbl.create 42
    
    let get_global = Hashtbl.find global_mem
    let fetch_word (car,_) = car
    let fetch_cell (car, cdr) = (car, cdr)
    let alloc_cons car cdr = ((car),(cdr))
    let fetch_car = fetch_word
    let fetch_cdr (_, cdr) = cdr 
    let alloc_word word = (word,Nil)

    let assemble_and_load =
      (* mostly repetitive boilerplate code *)
      let rec assemble = function
        | Asm.Nil -> Nil
        | Asm.Local (n,m) -> Local (n,m)
        | Asm.Global s -> Global s
        | Asm.Symb s -> Symb s
        | Asm.Cond (consequent, alternative) ->
            Cond (alloc_cons (assemble consequent) (assemble alternative))
        | Asm.List (car, cdr) -> List (alloc_cons (assemble car) (assemble cdr))
        | Asm.Num n -> Num n
        | Asm.Proc asm -> Proc (alloc_word (assemble asm))
        | Asm.First (arg1, rest) ->
            begin 
            match rest with
                |Asm.Next(_,_) ->                   
                 First (alloc_cons (assemble arg1) (assemble_args rest))
                | _->  (*Case apply one arg*)
                 ApplyOneArg(alloc_cons (assemble arg1) (assemble_args rest))
            end
        | Asm.Sequence (car, cdr) -> Sequence (alloc_cons (assemble car) (assemble cdr))
      and assemble_args = function
        | Asm.Next (arg, rest) -> 
          begin
          match rest with
            |Asm.Next(_,_)-> Next (alloc_cons (assemble arg) (assemble_args rest))
            |_-> Last(alloc_cons (assemble arg) (assemble_args rest))
          end 
        | Asm.CallPrim prim ->   Primitive prim
        | Asm.CallFun asm ->     assemble asm
      in
      List.iter (fun (str, asm) -> Hashtbl.add global_mem str (assemble asm))
  end

module Eval : 
  sig
      val get_val_num : unit -> int option
      val eval  : unit -> unit 
      val expr  : Gc.word ref
      val value : Gc.word ref
      val env   : Gc.word ref
      val args  : Gc.word ref
      val stack : Gc.word ref
      val execute_main : unit -> unit 
  end
   =
  struct
    let expr  = ref Gc.Nil
    let value = ref Gc.Nil
    let env   = ref Gc.Nil
    let args  = ref Gc.Nil
    let stack = ref Gc.Nil

    let get_val_num () = match !value with
          | Gc.Num a -> Some a
          | _ -> None 
    let polyprint = function
              | Gc.Num a -> print_int a ; print_endline("vu")
              |_->()
    let rec walk_on_list a b flag  = function
      | Gc.List(ptr) -> let (car,cdr) = Gc.fetch_cell ptr in
                        if flag then 
                            if b=0 then  car
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
      | Gc.First( ptr)
      | Gc.Next( ptr)
      | Gc.Last( ptr) 
      | Gc.ApplyOneArg(ptr)
      | Gc.Sequence( ptr) 
      | Gc.ReturnTag(_, ptr) -> ptr
      | _ -> assert(false)
    

    let dispatch_on_stack () = match !stack with
      | Gc.ReturnTag(return_state, _) -> return_state ()
      | Gc.Nil -> ()
      | _ -> failwith "bad stack"
    let push_with_return x return_state = 
      stack := Gc.ReturnTag(return_state, Gc.alloc_cons x !stack)
    let push x = 
      stack := Gc.List(Gc.alloc_cons x !stack)
    let pop_to_reg reg =
      reg := Gc.fetch_car (get_ptr !stack);
      stack := Gc.fetch_cdr (get_ptr !stack)

    let return_value v =
      value := v;
      dispatch_on_stack ()
    let self_evaluating () = return_value !expr
  
    let rec eval () = match (!expr) with
      | Gc.Nil          -> self_evaluating ()
      | Gc.Local(a,b)   -> return_value (walk_on_list a b false (!env))
      | Gc.Global s     -> return_value (Gc.get_global s)
      | Gc.Symb(s)      -> self_evaluating ()
      | Gc.Closure(ptr) -> self_evaluating ()
                             
      | Gc.Cond(ptr)    -> (
                        if !value = Gc.Nil 
                        then 
                           expr := Gc.fetch_cdr ptr 
                        else 
                           expr := Gc.fetch_car ptr
                        );
                        eval ()
      | Gc.List(ptr)  -> self_evaluating ()
      | Gc.Num(vint)  -> self_evaluating ()
      | Gc.Proc(ptr)  -> return_value(Gc.Closure(Gc.alloc_cons (!expr) (!env)))
        
      | Gc.First(ptr)       -> save_cdr_and_eval_car return_first
      | Gc.Next(ptr)        -> push !args;
                               save_cdr_and_eval_car return_next
      | Gc.Last(ptr)        -> push !args;
                               save_cdr_and_eval_car return_last
      | Gc.ApplyOneArg(ptr) -> save_cdr_and_eval_car return_apply1

      | Gc.Primitive(prim) -> self_evaluating ()
      | Gc.Sequence(ptr) -> value := Gc.Nil;
                            save_cdr_and_eval_car return_sequence

    and apply () = match !value with
      | Gc.Closure(ptr) ->  env   := Gc.List(
                                Gc.alloc_cons !args (snd (Gc.fetch_cell ptr))
                                     );
                        expr  := Gc.fetch_car(get_ptr (Gc.fetch_car ptr));
                        eval()
      | Gc.Primitive(prim) -> let lastarg= Gc.fetch_car (get_ptr !args) in
        begin
          match prim with
            | Car   -> return_value (Gc.fetch_car (get_ptr lastarg))
            | Cdr   -> return_value (Gc.fetch_cdr (get_ptr lastarg))
            | Cons  -> let (car,Gc.List cdr) = Gc.fetch_cell (get_ptr !args) in
                       let cadr= Gc.fetch_car cdr in
                       return_value (Gc.List(Gc.alloc_cons cadr car))
            | Incr  -> let (Gc.Num a) = lastarg in 
                       return_value (Gc.Num (a+1))
            | Decr  -> let (Gc.Num a) = lastarg in
                       return_value (Gc.Num (a-1))
            | Zero  -> let (Gc.Num a) = lastarg in
                       if a=0 then return_value( Gc.Symb("T") )
                       else return_value Gc.Nil
            | Atom  -> assert(false)
        end           
      | _-> env := Gc.List( 
                Gc.alloc_cons !args Gc.Nil);
            expr := !value;
            eval()

    and save_cdr_and_eval_car return_state =
      push !env;
      push_with_return (Gc.fetch_cdr (get_ptr !expr)) return_state;
      expr := Gc.fetch_car (get_ptr !expr);
      eval ()
    and standard_restore () =
      pop_to_reg expr;
      pop_to_reg env
    and return_first () =
      standard_restore ();
      args := Gc.List(Gc.alloc_cons !value Gc.Nil);
      eval ()
    and return_next () =
      standard_restore ();
      pop_to_reg args;
      args := Gc.List(Gc.alloc_cons !value !args);
      eval ()
    and return_last () =
      standard_restore ();
      pop_to_reg args;
      args := Gc.List(Gc.alloc_cons !value !args);
      push_with_return !args return_apply;
      eval ()
    and return_apply1 () = 
      standard_restore ();
      args := Gc.List(Gc.alloc_cons !value Gc.Nil);
      push_with_return !args return_apply;
      eval ()
    and return_sequence () =
      standard_restore ();
      eval ()
    and return_apply () =
      pop_to_reg args;
      apply ()

    let execute_main () =
      expr:= Gc.get_global("main");
      eval()
  end

open Asm

let add = [
  "main",
  First(Num(21),Next(Num(21),CallFun(Global("add")))) ;
  "add",
  Sequence(First(Local(0,0),CallPrim(Zero)),
           Cond(Local(0,1),
                First(First(Local(0,1),CallPrim(Incr)),
                      Next(First(Local(0,0),CallPrim(Decr)),
                           CallFun(Global("add"))))))
]

(* Testing primitive recursive addition *)

(* 
Gc.assemble_and_load add;;
Eval.execute_main ();;
match Eval.get_val_num () with
    | None -> print_endline("Error")  
    | Some a -> print_int(a);;
*)


let clock = [
  "clock",
  Sequence(First(Local(0,0),CallPrim(Isgt60)),
           Cond(
             First(Num(0),
                   Next(First(Local(0,1),CallPrim(Incr)),
                        CallFun(Global("clock")))),
             First(First(Local(0,0),CallPrim(Incr)),
                   Next(Local(0,1),
                        CallFun(Global("clock"))))))
]






