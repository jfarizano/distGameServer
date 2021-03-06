-module(tateti).
-include("header.hrl").
-export([make_play/3, valid_move/2, modify_board/3, check/3]).

make_play(Game, {I, J}, Name) -> 
  ValidMove = valid_move(Game, {I, J}),
  Pos = J + ( (I - 1) * 3),
  if 
    Game#game.turn == Name ->
      if ValidMove ->
          {NTurn, NBoard} = modify_board(Game, Pos, Name),
          NMoves = Game#game.moves + 1,
          NGame = Game#game{board = NBoard, moves = NMoves, turn = NTurn},
          case check(NBoard, element(Pos, NBoard), NMoves) of
            continue -> NGame;
            Result -> {Result, NGame}
          end;
          true -> {error, "Movimiento inválido"}
      end;
    (Game#game.p1 /= Name) and (Game#game.p2 /= Name) ->
      {error, "No estás participando de este juego"};
    true -> {error, "No es tu turno"}
  end.

valid_move(Game, {I, J}) ->
  % A pesar de que ambas condiciones (pos dentro del tablero y casilla vacia)
  % se puedan verificar en un paso con un and, si la posición no es válida
  % element devuelve un error.
  if
    (I > 0) and (I < 4) and (J > 0) and (J < 4) -> 
      (element(J + ( (I - 1) * 3), Game#game.board) == empty);
    true -> false
  end.
           
modify_board(Game, Pos, Name) ->
  if
    Game#game.p1 == Name -> {Game#game.p2, setelement(Pos, Game#game.board, x)};
    true -> {Game#game.p1, setelement(Pos, Game#game.board, o)}
  end.

check(Board, Token, Moves) -> 
  case Board of
    {Token, _, _,
     Token, _, _,
     Token, _, _} -> win;

    {_, Token, _,
     _, Token, _,
     _, Token, _} -> win;

    {_, _, Token,
     _, _, Token,
     _, _, Token} -> win;

    {Token, Token, Token,
     _, _, _,
     _, _, _} -> win;

    {_, _, _,
     Token, Token, Token,
     _, _, _} -> win;

    {_, _, _,
     _, _, _,  
     Token, Token, Token} -> win;

    {Token, _, _,
     _, Token, _,
     _, _, Token} -> win;

     {_, _, Token,
      _, Token, _,
      Token, _, _} -> win;     

    _ -> 
      if Moves == 9 -> tie;
         true -> continue
      end
  end.