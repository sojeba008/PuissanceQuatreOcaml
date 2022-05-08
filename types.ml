module type REPRESENTATION =
sig
  type game
  type move
  val game_start : unit -> game
  val legal_moves: bool -> game -> move list
  val play: bool -> move -> game -> game
end ;;


module type EVAL =
sig
  type game
  val evaluate: bool -> game -> int
  val moreI : int
  val lessI: int
  val is_leaf: bool -> game -> bool
  val is_stable: bool -> game -> bool
  type state = G | P | D | C
  val state_of : bool -> game -> state
end ;;

module type DISPLAY = 
sig
  type game
  type move
  val home: unit -> unit
  val exit: unit -> unit
  val won: unit -> unit
  val lost: unit -> unit
  val draw: unit -> unit
  val init: unit -> unit
  val position : bool -> move -> game -> game -> unit
  val choice : bool -> game -> move
  val question_choix_difficulte : unit -> int
  val question_continue : unit -> bool
  val notifier_a_qui_le_tour : int -> unit 
end ;;

module type ALPHABETA = 
sig
  type game
  type move
  val alphabeta : int -> bool -> game -> move
end ;;

module type FALPHABETA = functor (Rep : REPRESENTATION)
  -> functor (Eval : EVAL with type game = Rep.game)
  -> ALPHABETA with type game = Rep.game
                and type move = Rep.move ;;


module type SQUELETTE = 
sig
  val home: unit -> unit
  val init: unit -> ((unit -> unit) * (unit -> unit))
  val again: unit -> bool
  val exit: unit -> unit
  val won: unit -> unit
  val lost: unit -> unit
  val draw: unit -> unit
  exception Won
  exception Lost
  exception Draw
end;;