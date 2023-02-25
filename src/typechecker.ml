open Ast
module VMap = Map.Make (String)

let ctx = VMap.empty
let fun_ctx = VMap.empty

let print_typ t =
  match t with
  | TInt -> Format.sprintf "TInt\n"
  | TVoid -> Format.sprintf "TVoid\n"

let rec typ_of_ex ctx fun_ctx : Ast.expr -> Ast.typ = function
  | Const _ -> TInt
  | Op _ -> TInt
  | Var s -> VMap.find s ctx
  | Apply (s, exprs) ->
      let t_l = VMap.find s fun_ctx in
      let typs = List.map (typ_of_ex ctx fun_ctx) exprs in
      let rec h t1 t2 =
        match (t1, t2) with
        | [], [ t2 ] -> t2
        | t1 :: tl1, t2 :: tl2 when t1 = t2 -> h tl1 tl2
        | _ ->
            failwith "Not the right number of arguments or the type is invalid!"
      in
      h typs t_l

let check_types (t1 : Ast.typ option) (t2 : Ast.typ) : Ast.typ =
  match t1 with
  | Some t1 -> if t1 = t2 then t1 else failwith "Types should be equal"
  | None -> t2

let rec check (ctx : typ VMap.t) (fun_ctx : typ list VMap.t) :
    Ast.stmt list -> Ast.stmt list = function
  | [] -> []
  | (Return _ as ret) :: tl -> ret :: check ctx fun_ctx tl
  | Let (n, t_op, e) :: tl ->
      let t = typ_of_ex ctx fun_ctx e in
      let t = check_types t_op t in
      let ctx = VMap.add n t ctx in
      Let (n, Some t, e) :: check ctx fun_ctx tl
  | Func (n, params, t_op, stmts) :: tl ->
      let rec stmts_typ (stmts : Ast.stmt list) (acc : Ast.stmt list * typ) =
        match stmts with
        | [] -> (fst acc, TVoid)
        | [ (Return e as ret) ] ->
            ( (check ctx fun_ctx [ ret ] |> List.hd) :: fst acc,
              typ_of_ex ctx fun_ctx e )
        | stmt :: tl ->
            stmts_typ tl
              ((check ctx fun_ctx [ stmt ] |> List.hd) :: fst acc, TVoid)
      in
      let stmts, t = stmts_typ stmts ([], TVoid) in
      let stmts = List.rev stmts in
      let t = check_types (Some t_op) t in
      let rec mount_t (params : (string * Ast.typ) list) =
        match params with [] -> [ t ] | (_, t) :: tl -> t :: mount_t tl
      in
      let ctx =
        List.fold_left (fun c (nn, typ) -> VMap.add nn typ c) ctx params
      in
      let t_mount = mount_t params in
      let fun_ctx = VMap.add n t_mount fun_ctx in
      Func (n, params, t, stmts) :: check ctx fun_ctx tl
