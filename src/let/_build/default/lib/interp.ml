open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
(*Jeremy Magno I pledge my Honor that I have abided by the Stevens Honor System.*)    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | Cons(e1, e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= list_of_listVal >>= fun ev2 ->
    return (ListVal (ev1::ev2) )
  | Hd(e) ->
    (eval_expr e >>= fun ev -> match ev with
    |ListVal [] -> error "Non-empty list expected!"
    |ListVal(h::_t) -> return h)
  | Tl(e)->
    (eval_expr e  >>= fun ev -> match ev with
    |ListVal [] -> error "Non-empty list expected!"
    |ListVal(_h::t) -> return (ListVal (t)))
  | IsEmpty(e) -> 
    (eval_expr e >>= fun ev ->
     match ev with 
      |ListVal[] -> return (BoolVal true)
      |ListVal _ -> return (BoolVal false)
      |_ -> error "Expected a list!")
  | EmptyList(_e) -> 
    return (ListVal([]))
  | Tuple(es) -> (let rec eval_tuple lst acc = 
    match lst with 
    | [] -> return (List.rev acc)
    | h::t -> 
      eval_expr h >>= fun ev ->
        eval_tuple t (ev::acc) in 
        eval_tuple es [] >>= 
        fun tup_vals -> 
          return (TupleVal tup_vals) )
  |Untuple(ids, e1, e2) ->
    eval_expr e1 >>= fun tupleValue ->
    match tupleValue with
    | TupleVal tv ->
      if List.length ids = List.length tv then
        extend_env_list ids tv >>+ eval_expr e2
      else
        error "extend_env_list: Arguments do not match parameters!"
    | _ ->
      error "Expected a tuple"
and
  eval_exprs : expr list -> ( exp_val list ) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h :: t -> eval_expr h >>= fun i ->
    eval_exprs t >>= fun l ->
    return (i :: l)
    | _-> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


