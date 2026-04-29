type value =
  | ValInt of int
  | ValStr of string
  | ValName of string
  | ValBool of bool
  | ValError
  | ValUnit
  | ValClosure of closure

(*Used and so type value could refer to the types used below*)
and stack = value list
and dict = (string * value) list
and enviro = stack * dict
and closure = string * string list * enviro

let unquote s =
  let len = String.length s in
  String.sub s 1 (len - 2)

let isname s =
  let len = String.length s in
  if len = 0 then false
  else
    let first c =
      Char.(('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_')
    in
    let rest c = first c || Char.('0' <= c && c <= '9') in
    first s.[0]
    &&
    let rec check i = i = len || (rest s.[i] && check (i + 1)) in
    check 1

let findVal (s : string) : value =
  let t = String.trim s in
  let len = String.length t in
  if t = ":error:" then ValError
  else if t = ":unit:" then ValUnit
  else if t = ":true:" then ValBool true
  else if t = ":false:" then ValBool false
  else if len >= 2 && t.[0] = '"' && t.[len - 1] = '"' then ValStr (unquote t)
  else
    try
      let n = int_of_string t in
      ValInt n
    with _ -> if isname t then ValName t else ValError

let valuetostr valu =
  match valu with
  | ValInt n -> string_of_int n
  | ValBool true -> ":true:"
  | ValBool false -> ":false:"
  | ValError -> ":error:"
  | ValUnit -> ":unit:"
  | ValStr str -> str
  | ValName str -> str
  (* i was getting a compiler warning shouldn't get printed*)
  | ValClosure _ -> "noerror"
(*
let upbind (name : string) (value : value) (dic : (string * value) list) :
  //  (string * value) list =
 // (name, value) :: dic
*)

let rec insertval (name : string) (dic : (string * value) list) : value =
  match dic with
  | [] -> ValError
  | (nam, v) :: rest -> if nam = name then v else insertval name rest

let check (v : value) (dic : (string * value) list) : value =
  match v with ValName x -> insertval x dic | _ -> v

let command_exe (cmd : string) (stack : stack) (dic : dict)
    (output : string list) : stack * dict * string list * bool =
  let inpu = String.split_on_char ' ' (String.trim cmd) in
  match inpu with
  | [ "pop" ] -> (
      match stack with
      | _ :: restofstack -> (restofstack, dic, output, false)
      | [] -> (ValError :: stack, dic, output, false))
  | "push" :: rest ->
      let value = String.concat " " rest in
      let parsed = findVal value in
      (parsed :: stack, dic, output, false)
  | [ "add" ] -> (
      match stack with
      | a :: b :: restofstack -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt first, ValInt sec ->
              (ValInt (sec + first) :: restofstack, dic, output, false)
          | _ -> (ValError :: a :: b :: restofstack, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "sub" ] -> (
      match stack with
      | a :: b :: restofstack -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt y, ValInt x ->
              (ValInt (x - y) :: restofstack, dic, output, false)
          | _ -> (ValError :: a :: b :: restofstack, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "mul" ] -> (
      match stack with
      | a :: b :: restofstack -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt y, ValInt x ->
              (ValInt (x * y) :: restofstack, dic, output, false)
          | _ -> (ValError :: a :: b :: restofstack, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "div" ] -> (
      match stack with
      | a :: b :: restofstack -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt y, ValInt x ->
              if y = 0 then
                (ValError :: a :: b :: restofstack, dic, output, false)
              else (ValInt (x / y) :: restofstack, dic, output, false)
          | _ -> (ValError :: a :: b :: restofstack, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "rem" ] -> (
      match stack with
      | a :: b :: restofstack -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt y, ValInt x ->
              if y = 0 then
                (ValError :: a :: b :: restofstack, dic, output, false)
              else (ValInt (x mod y) :: restofstack, dic, output, false)
          | _ -> (ValError :: a :: b :: restofstack, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "neg" ] -> (
      match stack with
      | a :: restofstack -> (
          let a' = check a dic in
          match a' with
          | ValInt i -> (ValInt (i - (i * 2)) :: restofstack, dic, output, false)
          | _ -> (ValError :: a :: restofstack, dic, output, false))
      | _ -> (ValError :: stack, dic, output, false))
  | [ "swap" ] -> (
      match stack with
      | a :: b :: restofstack -> (b :: a :: restofstack, dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "toString" ] -> (
      match stack with
      | a :: rest -> (ValStr (valuetostr a) :: rest, dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "println" ] -> (
      match stack with
      | ValStr s :: rest -> (rest, dic, output @ [ s ], false)
      | a :: rest -> (ValError :: a :: rest, dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "cat" ] -> (
      match stack with
      | a :: b :: rest -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValStr y, ValStr x -> (ValStr (x ^ y) :: rest, dic, output, false)
          | _ -> (ValError :: a :: b :: rest, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "and" ] -> (
      match stack with
      | a :: b :: rest -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValBool y, ValBool x ->
              if y = x then (ValBool y :: rest, dic, output, false)
              else (ValBool false :: rest, dic, output, false)
          | _ -> (ValError :: a :: b :: rest, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "or" ] -> (
      match stack with
      | a :: b :: rest -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValBool y, ValBool x ->
              if y = x then (ValBool y :: rest, dic, output, false)
              else (ValBool true :: rest, dic, output, false)
          | _ -> (ValError :: a :: b :: rest, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "not" ] -> (
      match stack with
      | a :: rest -> (
          match check a dic with
          | ValBool a' ->
              if a' = true then (ValBool false :: rest, dic, output, false)
              else (ValBool true :: rest, dic, output, false)
          | _ -> (ValError :: a :: rest, dic, output, false))
      | _ -> (ValError :: stack, dic, output, false))
  | [ "equal" ] -> (
      match stack with
      | a :: b :: rest -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt y, ValInt x -> (ValBool (x = y) :: rest, dic, output, false)
          | _ -> (ValError :: a :: b :: rest, dic, output, false))
      | _ -> (ValError :: stack, dic, output, false))
  | [ "lessThan" ] -> (
      match stack with
      | a :: b :: rest -> (
          let a' = check a dic and b' = check b dic in
          match (a', b') with
          | ValInt y, ValInt x -> (ValBool (x < y) :: rest, dic, output, false)
          | _ -> (ValError :: a :: b :: rest, dic, output, false))
      | [ x ] -> (ValError :: [ x ], dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "bind" ] -> (
      match stack with
      | ValName x :: ValName y :: rest -> (
          let res = insertval x dic in
          match res with
          | ValError ->
              (ValError :: ValName x :: ValName y :: rest, dic, output, false)
          | _ ->
              let dic' = (y, res) :: dic in
              (ValUnit :: rest, dic', output, false))
      | v :: ValName y :: rest -> (
          let v' = match v with ValName x -> insertval x dic | _ -> v in
          match v' with
          | ValError -> (ValError :: v :: ValName y :: rest, dic, output, false)
          | _ ->
              let dic' = (y, v') :: dic in
              (ValUnit :: rest, dic', output, false))
      | a :: b :: rest -> (ValError :: a :: b :: rest, dic, output, false)
      | _ -> (ValError :: stack, dic, output, false))
  | [ "if" ] -> (
      match stack with
      | x :: y :: z :: rest -> (
          match check z dic with
          | ValBool true -> (x :: rest, dic, output, false)
          | ValBool false -> (y :: rest, dic, output, false)
          | _ -> (ValError :: x :: y :: z :: rest, dic, output, false))
      | _ -> (ValError :: stack, dic, output, false))
  | [] -> (stack, dic, output, false)
  | [ "quit" ] -> (stack, dic, output, true)
  | _ -> (ValError :: stack, dic, output, false)

let interpreter (in_file : string) (out_file : string) : unit =
  let input_lines ic =
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []
  in
  let cmds = In_channel.with_open_text in_file input_lines in

  let run cmds stack dic out =
    (* collect all lines of the function body until funEnd *)
    let rec funbody cmdss cmdlist =
      match cmdss with
      | [] -> failwith "fun without matching funEnd"
      | cmda :: rest ->
          let cmd = String.trim cmda in
          if cmd = "funEnd" then (List.rev cmdlist, rest)
          else funbody rest (cmda :: cmdlist)
    in
    let rec loop cmdss (env : enviro) envstack out :
        stack * dict * string list * value option =
      match cmdss with
      | [] ->
          let stack, dic = env in
          (stack, dic, out, None)
      | cmda :: rest -> (
          let cmd = String.trim cmda in
          let tokens = String.split_on_char ' ' cmd in
          match tokens with
          | [ "let" ] ->
              let _, currentdic = env in
              let innerenv : enviro = ([], currentdic) in
              loop rest innerenv (env :: envstack) out
          | [ "end" ] -> (
              match envstack with
              | (outstack, outdic) :: envrest -> (
                  let innerstack, _ = env in
                  match innerstack with
                  | v :: _ ->
                      let newouterenv : enviro = (v :: outstack, outdic) in
                      loop rest newouterenv envrest out
                  | [] ->
                      let stk, dicc = env in
                      (ValError :: stk, dicc, out, None))
              | [] ->
                  let stk, dicc = env in
                  (ValError :: stk, dicc, out, None))
          | [ "return" ] -> (
              let curstack, curdic = env in
              match curstack with
              | v :: _ -> (curstack, curdic, out, Some v)
              | [] -> (ValError :: curstack, curdic, out, Some ValError))
          | [ "fun"; fname; arg ] ->
              (* get every command till funEnd *)
              let body, restaft = funbody rest [] in
              let stack, dic = env in
              let rec clos : closure =
                (arg, body, (stack, (fname, ValClosure clos) :: dic))
              in
              (* Bind the function name in the current environment*)
              let dic' = (fname, ValClosure clos) :: dic in
              (* Push :unit: on the stack*)
              let stack' = ValUnit :: stack in
              let env' : enviro = (stack', dic') in
              loop restaft env' envstack out
          | [ "call" ] -> (
              let stack, dic = env in
              match stack with
              | arg :: ValName fname :: reststack -> (
                  (* find argument val like for bind*)
                  let argval =
                    match arg with ValName x -> insertval x dic | _ -> arg
                  in
                  match argval with
                  | ValError ->
                      (* invalid arg push error *)
                      let newenv : enviro = (ValError :: stack, dic) in
                      loop rest newenv envstack out
                  | _ -> (
                      (* find function closure in dictionary*)
                      match insertval fname dic with
                      | ValClosure (argname, body, closureenv) ->
                          let _, closdic = closureenv in
                          (* function env*)
                          let funenv : enviro =
                            ([ argval ], (argname, argval) :: closdic)
                          in
                          (* run body commands *)
                          let funstack, _fundic, out', returnopt =
                            loop body funenv [] out
                          in
                          (* result is either explicit return value or top of fun stack *)
                          let v =
                            match returnopt with
                            | Some returnval -> returnval
                            | None -> (
                                match funstack with
                                | v :: _ -> v
                                | [] -> ValError)
                          in
                          let callenv : enviro = (v :: reststack, dic) in
                          loop rest callenv envstack out'
                      | _ ->
                          (* name wasn't a closure *)
                          let newenv : enviro = (ValError :: stack, dic) in
                          loop rest newenv envstack out))
              | _ ->
                  (* not enough values *)
                  let newenv : enviro = (ValError :: stack, dic) in
                  loop rest newenv envstack out)
          | _ ->
              let curstack, curdic = env in
              let stack', dic', out', quit =
                command_exe cmd curstack curdic out
              in
              if quit then (stack', dic', out', None)
              else
                let newenv : enviro = (stack', dic') in
                loop rest newenv envstack out')
    in
    loop cmds (stack, dic) [] out
  in

  (* ignore the last one too*)
  let _, _, results, _ = run cmds [] [] [] in
  let output_lines oc =
    List.iter (fun line -> output_string oc (line ^ "\n")) results
  in
  Out_channel.with_open_text out_file output_lines
