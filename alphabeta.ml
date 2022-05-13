(*
Ce module gère l'algorithme minimax avec Elagage AlphaBeta.
IMPPORTANT: Ce module est issue du livre Emmanuel Chailloux, Pascal Manoury, and Bruno Pagano.
Developpement d’applications avec Objective Caml . O’Reilly France, 1998. http ://caml.inria.fr/pub/docs/oreilly-book/.
(Référence imposée dans le sujet du projet)
*)
module FAlphabetaO
(Rep : Types.REPRESENTATION) (Eval : Types.EVAL with type game = Rep.game) =

struct
  type game = Rep.game
  type move = Rep.move
  exception AlphaMovement of int
  exception BetaMovement of int
  let maxmin_iter node minmax_cur beta alpha cp =
    let alpha_resu =
      max alpha (minmax_cur (Rep.play true cp node) beta alpha)
    in if alpha_resu >= beta then raise (BetaMovement alpha_resu)
    else alpha_resu
  
  let minmax_iter node maxmin_cur alpha beta cp =
    let beta_resu =
      min beta (maxmin_cur (Rep.play false cp node) alpha beta)
    in if beta_resu <= alpha then raise (AlphaMovement beta_resu)
    else beta_resu

  let rec maxmin depth node alpha beta =
    if (depth < 1 && Eval.is_stable true node)
      || Eval.is_leaf true node
    then Eval.evaluate true node
    else
      try let prev = maxmin_iter node (minmax (depth - 1)) beta
      in List.fold_left prev alpha (Rep.legal_moves true node)
      with BetaMovement a -> a

  and minmax depth node beta alpha =
    if (depth < 1 && Eval.is_stable false node)
      || Eval.is_leaf false node
    then Eval.evaluate false node
    else
      try let prev = minmax_iter node (maxmin (depth - 1)) alpha
      in List.fold_left prev beta (Rep.legal_moves false node)
      with AlphaMovement b -> b

  let rec search a l1 l2 = match (l1,l2) with
      (h1 :: q1, h2 :: q2) -> if a = h1 then h2 else search a q1 q2
    | ([], [] ) -> failwith ("AB: "^(string_of_int a)^" not found")
    | (_ , _) -> failwith "AB: length differs"

(* val alphabeta : int -> bool -> Rep.game -> Rep.move *)
let alphabeta depth player level =
  let alpha = ref Eval.lessI and beta = ref Eval.moreI in
  let l = ref [] in
  let cpl = Rep.legal_moves player level in
  let eval =
    try
    for i = 0 to (List.length cpl) - 1 do
      if player then
        let b = Rep.play player (List.nth cpl i) level in
        let a = minmax (depth-1) b !beta !alpha
        in l := a :: !l ;
        alpha := max !alpha a ;
        (if !alpha >= !beta then raise (BetaMovement !alpha))
      else
        let a = Rep.play player (List.nth cpl i) level in
        let b = maxmin (depth-1) a !alpha !beta
        in l := b :: !l ;
        beta := min !beta b ;
        (if !beta <= !alpha then raise (AlphaMovement !beta))
    done ;
    if player then !alpha else !beta
  with
    BetaMovement a -> a
  | AlphaMovement b -> b
  in
  l := List.rev !l ;
  search eval !l cpl
  end ;;           


module FAlphabeta = (FAlphabetaO : Types.FALPHABETA);;