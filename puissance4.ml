open Graphics;;
(* let _=read_line();; *)
module PuissanceQuatre_representation = struct
  type cell = J1 | J2 | Empty
  type game = cell array array
  type move = int
  let col = 7 and row = 6
  let game_start () = Array.make_matrix row col Empty
  let legal_moves b m =
    let l = ref [] in
    for c = 0 to col-1 do if m.(row-1).(c) = Empty then l := (c+1) :: !l done;
    !l

  let augment mat c =
    let l = ref row
    in while !l > 0 && mat.(!l-1).(c-1) = Empty do decr l done ; !l + 1
  
  let player_gen cp m e =
    let mj = Array.map Array.copy m
    in mj.((augment mj cp)-1).(cp-1) <- e ; mj
  
  let play b cp m = if b then player_gen cp m J1 else player_gen cp m J2
end ;;



module PuissanceQuatre_graph = 
struct
  open PuissanceQuatre_representation
  type game = PuissanceQuatre_representation.game
  type move = PuissanceQuatre_representation.move
  let r = 36 (* color of piece *)
  let ec = 20 (* distance between pieces *)
  let dec = 30 (* center of first piece *)

  let htexte = 25 (* where to place text *)


  let wb = 25 (* width of the buttons *)
  let hb = 16 (* height of the buttons *)
  (* val t2e : int -> int *)
  let n_colonnes = 7 
  let n_lignes = 6
  let windows_h = 650
  let windows_w = 670
  let rayon_cercle = 36 (*Rayon d'une case*)
  let marge_horizontale = 20 (* Marge entre les cases *)
  let diametre_cercle = 2*rayon_cercle (*Diamère d'une case*)
  let largeur_totale_cercle = diametre_cercle + marge_horizontale (*Largeur total prise par une case (marge comprise)*)





  let couleur_noir = Graphics.black (* trace *)
  (* let j1_couleur = Graphics.red  (* Couleur du joueur 1 *)
  let j2_couleur = Graphics.yellow Couleur du joueur 2 (IA) *)
  let couleur_arriere_plan = Graphics.rgb 169 169 169 (* Game Background color *)


  let largeur_plateau = n_colonnes * largeur_totale_cercle + marge_horizontale (* Largeur du plateau *)
  let hauteur_plateau_avec_marge = n_lignes * largeur_totale_cercle + marge_horizontale + 30  (* Hauteur du plateau avec marge*)
  let hauteur_plateau = n_lignes * largeur_totale_cercle + marge_horizontale (*Hauteur du plateau avec marge*)
  let retour_btn_pos_x = 20
  let retour_btn_pos_y = (windows_w-100)
  let retour_btn_pos_w = 100
  let retour_btn_pos_h = 40

  let recommencer_btn_pos_x = (windows_h-110)
  let recommencer_btn_pos_y = (windows_w-100)
  let recommencer_btn_pos_w = 100
  let recommencer_btn_pos_h = 40


  let commencer_btn_pos_x = 280
  let commencer_btn_pos_y = 400
  let commencer_btn_pos_w = 80
  let commencer_btn_pos_h = 30

  let facile_btn_pos_x = 220
  let facile_btn_pos_y = 400
  let facile_btn_pos_w = 65
  let facile_btn_pos_h = 30

  let normal_btn_pos_x = 290
  let normal_btn_pos_y = 400
  let normal_btn_pos_w = 65
  let normal_btn_pos_h = 30

  let vv = 16

  let difficle_btn_pos_x = 360
  let difficle_btn_pos_y = 400
  let difficle_btn_pos_w = 65
  let difficle_btn_pos_h = 30

  let rejouer_oui_btn_pos_x = ((windows_w/2)-50)
  let rejouer_oui_btn_pos_y = (windows_h-70-vv) 
  let rejouer_oui_btn_pos_w = 35 
  let rejouer_oui_btn_pos_h = 30 
  
  let rejouer_non_btn_pos_x = ((windows_w/2)-5)
  let rejouer_non_btn_pos_y = (windows_h-70-vv)
  let rejouer_non_btn_pos_w = 35
  let rejouer_non_btn_pos_h = 30


  (* The Colors *)
  let cN = Graphics.black (* trace *)
  let cA = Graphics.red (* Human player *)
  let cB = Graphics.yellow (* Machine player *)
  let cF = Graphics.rgb 169 169 169 (* Game Background color *)


  let ajouter_bouton x y w h texte marge_gauche_texte marge_haut_texte = 
    set_color black;
    fill_rect  x y w h;
    set_color white;
    moveto (x+marge_gauche_texte) (y+marge_haut_texte);
    draw_string texte


  (* val draw_table : unit -> unit : Trace an empty table *)
  let draw_table () =
    Graphics.clear_graph();
    set_line_width 20;
    moveto 10 10;
    Graphics.set_color couleur_arriere_plan;
    Graphics.fill_rect 0 0 (largeur_plateau+20) hauteur_plateau;
    (* Graphics.set_color couleur_noir; *)
    Graphics.moveto 0 hauteur_plateau;
    Graphics.lineto largeur_plateau hauteur_plateau;
    for l = 0 to n_lignes-1 do
      for c = 0 to n_colonnes-1 do
        Graphics.set_color Graphics.white;
        let x = 60 + (c)*largeur_totale_cercle in 
        let y = 55 + (l)*largeur_totale_cercle in 
        Graphics.draw_circle x y (rayon_cercle);
        Graphics.fill_circle x y (rayon_cercle);
      done
    done;
    set_color white;
    fill_rect  0 (windows_w-108) (windows_h+20) 50;
    ajouter_bouton retour_btn_pos_x retour_btn_pos_y retour_btn_pos_w retour_btn_pos_h "Retour" 27 15;
    ajouter_bouton (windows_h-110) (windows_w-100) 100 40 "Recommencer" 13 15


    let notifier_a_qui_le_tour tour=
      set_color white;
      fill_rect  ((windows_w/2)-50) (windows_h-70) 150 30;
      set_color black;
      moveto ((windows_w/2)-50) (windows_h-70);
      if tour == 0 then draw_string "A VOTRE TOUR";
      if tour == 1 then draw_string "TOUR DE L'IA"



  (* val draw_piece : int -> int -> Graphics.color -> unit *)
  (* ’draw_piece l c co’ draws a piece of color co at coordinates l c *)
  let draw_piece l c col =
    Graphics.set_color col;
    let x = 60 + (c-1)*largeur_totale_cercle in 
    let y = 55 + (l-1)*largeur_totale_cercle in 
    Graphics.fill_circle x y (r-5)
  (* val augment : Rep.item array array -> int -> Rep.move *)
  (* ’augment m c’ redoes the line or drops the piece for c in m *)
  let augment mat c =
    let l = ref row in
    while !l > 0 && mat.(!l-1).(c-1) = Empty do
      decr l
    done;
    !l
  (* val conv : Graphics.status -> int *)
  (* convert the region where player has clicked in controlling the game *)
  let conv st =
    let x = (st.Graphics.mouse_x) / largeur_totale_cercle + 1 in
    print_int x;
    print_string " ";
    x
    (* val wait_click : unit -> Graphics.status *)
    (* wait for a mouse click *)
    
    let wait_click () = Graphics.wait_next_event [Graphics.Button_down]
    (* val choiceH : Rep.game -> Rep.move *)
    (* give opportunity to the human player to choose a move *)
    (* the function offers possible moves *)
    let rec choice player game =
      let c = ref 0 in
      let valid_h = ref false in 
      while not ( List.mem !c (legal_moves player game) ) && !valid_h==false do
        let st = wait_click() in
        c := conv ( st);
        if st.Graphics.mouse_y < 560 then
          valid_h:=true;
        let r_x = retour_btn_pos_x in
        let r_y = retour_btn_pos_y in
        let r_w = retour_btn_pos_w in
        let r_h = retour_btn_pos_h in
        if st.Graphics.mouse_x> r_x && st.Graphics.mouse_x<r_x+r_w && st.Graphics.mouse_y> r_y && st.Graphics.mouse_y<r_y+r_h then
          close_graph();
      done;
      !c
  
    let quitter_ou_recommecer st = 
      true
    (* val home : unit -> unit : home screen *)
  let home () =

    open_graph (Printf.sprintf " %ix%i" windows_w windows_h);
    Graphics.set_color cN;
    Graphics.moveto 2 2;

    synchronize ();
    moveto 10 10;
    Graphics.clear_graph();
    set_color cN;
    moveto 15 15;
    draw_string "Par SOSSOU Jean-Baptiste";

    moveto 290 500;
    draw_string "Puissance 4";
    moveto 260 450;
    draw_string "Voulez-vous commencer ?";
    ajouter_bouton commencer_btn_pos_x commencer_btn_pos_y commencer_btn_pos_w commencer_btn_pos_h "Commencer" 15 10;
    ignore (wait_click () );
    Graphics.clear_graph()
  

  (*PErmet de fermet la fenetre*)
  let exit () = Graphics.close_graph()
  


  (*Fonction générique pour poser une question à l'écran à l'utilisateur, par exemple s'il souhaite recommencer ou pas. Lorsqu'il clique sur Oui, on retourne True, et False si non*)
  let question s =
    let rec ecouter_click () =
      let e = wait_click () in
        let x_oui = rejouer_oui_btn_pos_x in
        let y_oui = rejouer_oui_btn_pos_y in
        let w = rejouer_oui_btn_pos_w in
        let h = rejouer_oui_btn_pos_h in
        let x_non = rejouer_non_btn_pos_x in
        print_string " ";
        if e.Graphics.mouse_x> x_oui && e.Graphics.mouse_x<x_oui+w && e.Graphics.mouse_y> y_oui && e.Graphics.mouse_y<y_oui+h then
          true
        else
          if e.Graphics.mouse_x> x_non && e.Graphics.mouse_x<x_non+w && e.Graphics.mouse_y> y_oui && e.Graphics.mouse_y<y_oui+h then
            false
        else
        ecouter_click () in
        moveto  ((windows_w/2)-30) (windows_h-30-vv);
        
        Graphics.set_color white;
        fill_rect  ((windows_w/2)-50) (windows_h-70) 150 30;

        Graphics.set_color cN;
        draw_string s;


        ajouter_bouton rejouer_oui_btn_pos_x rejouer_oui_btn_pos_y rejouer_oui_btn_pos_w rejouer_oui_btn_pos_h "Oui" 9 8;
        ajouter_bouton rejouer_non_btn_pos_x rejouer_non_btn_pos_y rejouer_non_btn_pos_w rejouer_non_btn_pos_h "Non" 9 8;
        ecouter_click()

  (* val won : unit -> unit *)
  (* val lost : unit -> unit *)
  (* val draw : unit -> unit *)
  (* Three functions for these three cases *)
  (*Message affiché en cas de victoire*)
  let won () =
    moveto  ((windows_w/2)-35) (windows_h-30);
    Graphics.set_color cN;
    draw_string "VICTOIRE !";
    ignore (wait_click () ) 

  
  (*Message affiché en cas de défaite*)
  let lost () =
    moveto  ((windows_w/2)-65) (windows_h-30);
    Graphics.set_color cN;
    draw_string "VOUS AVEZ PERDU !";
    ignore (wait_click () ) 


  (*Message affiché en cas de match nul*)
  let draw () =
    moveto  ((windows_w/2)-60) (windows_h-30);
    Graphics.set_color cN;
    draw_string "MATCH NUL !";
    ignore (wait_click () ) 
    (* () *)


  (* 
  On demande à l'utilisateur de choisir le niveau de difficulté de l'Intelligence Artificielle. Les valeurs 4,6,8 retournés respectivement pour les niveaux Facile, Normal et Difficle
  La valeur retournée est en fait la profondeur maximale qu'on va imposer à notre algorithme de AlphaBeta. Ainsi, plus cette valeur est grande, plus l'IA est intelligente et donc le niveau de difficulté est plus gtrand.
  *)
  let choix_difficulte s =
    Graphics.clear_graph();
    let rec ecouter_click () =
      let e = wait_click () in
      let x= e.Graphics.mouse_x  in
      let y= e.Graphics.mouse_y  in 
      if (x>230 && x<295 && y>400 && y<430) then
        begin
          Graphics.clear_graph();
          4;
        end
      else
        if x>300 && x<365 && y>400 && y<430 then 
          begin
            Graphics.clear_graph();
            6
          end
      else
        if x>370 && x<435 && y>400 && y<430 then 
          begin
            Graphics.clear_graph();
            8
          end
        
      else
        ecouter_click () in
        set_color black;
        moveto 15 15;
        draw_string "Par SOSSOU Jean-Baptiste";
    
        moveto 290 500;
        draw_string "Puissance 4";
        moveto 230 450;
        draw_string "Veullez choisir le niveau l'IA";
    
        ajouter_bouton facile_btn_pos_x facile_btn_pos_y facile_btn_pos_w facile_btn_pos_h "Facile" 15 10;
        ajouter_bouton normal_btn_pos_x normal_btn_pos_y normal_btn_pos_w normal_btn_pos_h "Normal" 15 10;
        ajouter_bouton difficle_btn_pos_x difficle_btn_pos_y difficle_btn_pos_w difficle_btn_pos_h "Difficile" 5 10;
        (* won () ;
        question "Rejouez ?"; *)

        ecouter_click()

  let question_choix_difficulte () =
    let b = choix_difficulte "Veullez choisir le niveau l'IA" in
    (* (); *)
    b
    
  (* On demande à l'utilisateur s'il souhaite rejouer. LA fonction retourne True si l'utilisateur clique sur Oui et False si non *)
  let question_continue () =
    let b = question "Rejouez ?" in
    b
  

  (* Cette fonction est appelée à chaque début de partie pour la position *)
  let init = draw_table
    let position b c aj nj =
      if b then
        draw_piece (augment nj c) c cA
      else
        draw_piece (augment nj c) c cB

  (* Dessiner la piece de l'humain sur le plateau l'orsqu'il joue *)
  let dessiner_piece_humain cp j = draw_piece (augment j cp) cp cA


  (* Dessiner la piece de l'IA sur le plateau l'orsqu'elle joue *)
  let dessiner_piece_ia cp j = draw_piece (augment j cp) cp cB
end ;;




(*
Ce module permet gérer tout ce qui est relatif à l'état du jeu.
Il permet entre autre d'évaluer l'état du plateua, c'est-à-dire associer une note(ou score) au plateau afin de déterminer si un choix est plus favorable que l'autre. 
*)
module PuissanceQuatre_eval = struct
  open PuissanceQuatre_representation 
  type game = PuissanceQuatre_representation.game
  let value =
    Array.of_list [0; 2; 10; 50]
  exception Four of int
  exception Draw_Value
  exception Arg_invalid
  let lessI = -10000  (*Valeur prise et considérée comme moins linfini dans le cadre de l'évaluation du plateau. On procède ainsi, étant donné qu'il n'existe pas l'attribut infinity pour le type Integer comme pour le type Float*)
  let moreI = 10000 (*Valeur prise et considérée comme plus linfini dans le cadre de l'évaluation du plateau. On procède ainsi, étant donné qu'il n'existe pas l'attribut infinity pour le type Integer comme pour le type Float*)
  let eval_four m l_dep c_dep delta_l delta_c =
    let n = ref 0 and e = ref Empty
    and x = ref c_dep and y = ref l_dep
    in try
      for i = 1 to 4 do
        if !y<0 || !y>=row || !x<0 || !x>=col then raise Arg_invalid ;
        ( match m.(!y).(!x) with
          J1 -> if !e = J2 then raise Draw_Value ;
          incr n ;
          if !n = 4 then raise (Four moreI) ;
          e := J1
        | J2 -> if !e = J1 then raise Draw_Value ;
          incr n ;
          if !n = 4 then raise (Four lessI);
          e := J2;
        | Empty -> () ) ;
          x := !x + delta_c ;
        y := !y + delta_l
      done ;
      value.(!n) * (if !e=J1 then 1 else -1)
    with
      Draw_Value | Arg_invalid -> 0
  
  let eval_bloc m e cmin cmax lmin lmax dx dy =
    for c=cmin to cmax do for l=lmin to lmax do
      e := !e + eval_four m l c dx dy
    done done

  (*
  Cette fonction permet justement d'évaluer un état du plateau (ou un noeud de l'arbre de recherche) afin de determiner si un choix est plus profitable que l'autre.
  Elle prend en paramètre la matrice representant le plateau et les pieces posées et retourne une valeur entre -10000 0 2 10 50 10000.
  *)
  let evaluate b m =
    try let evaluation = ref 0
    in (* Evaluation des lignes *)
    eval_bloc m evaluation 0 (row-1) 0 (col-4) 0 1 ;
    (* Evaluation des colonnes *)
    eval_bloc m evaluation 0 (col-1) 0 (row-4) 1 0 ;
    (* Evaluation des diagonales venant de la première ligne (vers la droite)*)
    eval_bloc m evaluation 0 (col-4) 0 (row-4) 1 1 ;
    (* Evaluation des diagonales venant de la première ligne (vers la gauche) *)
    eval_bloc m evaluation 1 (row-4) 0 (col-4) 1 1 ;
    (* Evaluation des diagonales venant de la dernière ligne (vers la droite)*)
    eval_bloc m evaluation 3 (col-1) 0 (row-4) 1 (-1) ;
    (* Evaluation des diagonales venant de la dernière ligne (vers la gauche) *)
    eval_bloc m evaluation 1 (row-4) 3 (col-1) 1 (-1) ;
    !evaluation
    with Four v -> v

  (*
  Cette focntion vérifie si l'état du plateau est un noeud terminal ou une feuille de l'arbre de recherche. 
  Dans le cas échéant on est soit en présence d'une victoire(Won oû l'évaluation est égal à 10000), d'une défaite(Loose oû l'évaluation est égal à -10000) ou d'un match nul(Draw oû il n'y a plus de choix possibles)
  
  *)
  let is_leaf b m = let v = evaluate b m
  in v=moreI || v=lessI || legal_moves b m = []
  
  let is_stable b j = true
  
  type state = G | P | D | C  (*La liste des états possible du plateau ou des noeuds l'arbre de recherce : G(Victoire), P(Defaite), D(Defaite), C(Continue)*)
  

  (*Cette fonction permet de savoir si un joueur donné a soit gagné(G) soit perdu la partie (P). Lorsqu'il n'y a pas de gagnant, on retourne C *)
  let state_of player m =
    let v = evaluate player m
    in if v = moreI then if player then G else P
    else if v = lessI then if player then P else G
    else if legal_moves player m = [] then D else C
end ;;

module PuissanceQuatre_squelette = Modules.FSquelette (PuissanceQuatre_representation) (PuissanceQuatre_graph) (PuissanceQuatre_eval) (Alphabeta.FAlphabeta (PuissanceQuatre_representation) (PuissanceQuatre_eval)) ;;

module PuissanceQuatre_main = Modules.FMain(PuissanceQuatre_squelette) ;;

PuissanceQuatre_main.main() ;;
