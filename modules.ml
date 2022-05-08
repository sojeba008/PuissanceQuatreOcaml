module FSquelette
  (Rep : Types.REPRESENTATION)
  (Disp : Types.DISPLAY with type game = Rep.game and type move = Rep.move)
  (Eval : Types.EVAL with type game = Rep.game)
  (Alpha : Types.ALPHABETA with type game = Rep.game and type move = Rep.move) =

struct
  let depth = ref 4
  exception Won
  exception Lost
  exception Draw
  let won = Disp.won
  let lost = Disp.lost
  let draw = Disp.draw
  let again = Disp.question_continue
  let play_game = ref (Rep.game_start() )
  let exit = Disp.exit
  let home = Disp.home

  let playH player () =
    Disp.notifier_a_qui_le_tour 0;
    let choice = Disp.choice player !play_game in
    let old_game = !play_game
    in play_game := Rep.play player choice !play_game ;
    Disp.position player choice old_game !play_game ;
    match Eval.state_of player !play_game with
      Eval.P -> raise Lost
    | Eval.G -> raise Won
    | Eval.D -> raise Draw
    | _ -> ()

  let playM player () =
    Disp.notifier_a_qui_le_tour 1;
    let choice = Alpha.alphabeta !depth player !play_game in
    let old_game = !play_game
    in play_game := Rep.play player choice !play_game ;
    Disp.position player choice old_game !play_game ;
    match Eval.state_of player !play_game with
      Eval.G -> raise Won
    | Eval.P -> raise Lost
    | Eval.D -> raise Draw
    | _ -> ()

  let init () =
    
    let c = Disp.question_choix_difficulte() in
    Random.init ;
    let a = Random.bool() in
    let b = (a==false) in
    play_game := Rep.game_start () ;
    Disp.init () ;
    depth := c;
    match (a,b) with
    true,false -> playH true, playM false
  | false, true -> playM true, playH false;;

end ;;


module FMain (P : Types.SQUELETTE) =
struct
  let play_game movements = while true do (fst movements) () ;
    (snd movements) () done

  let main () = let finished = ref false
  in P.home ();
  while not !finished do
    ( try play_game (P.init () )
      with P.Won -> P.won ()
      | P.Lost -> P.lost ()
      | P.Draw -> P.draw () );
      finished := not (P.again () )
  done ;
  P.exit ()
end ;;