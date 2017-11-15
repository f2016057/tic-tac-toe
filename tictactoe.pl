/* This is a simple prolog program for simulations of tic-tac-toe game.
*
* This particular program is to be used only for the Logic in Computer Science 
* assignment. Not for external use, could be dangerous.
*
* This is a group project wherein participants are as follows :-
*	1. Atul Rai 		    2016A7PS0724G
*	2. Hrishikesh Dahiya 	2016A7PS0057G
*	3. Mounil Memaya 	    2016A7PS0077G
*	4. Satyajeet Jena   	2016A7PS0054G
*   5. Ayush Gupta          2016A7PS0024G
*   6. Bhavish Singh        2016A7PS0726G (Campus Roadie)
* Copyright for these participants.
* 
*
* Here goes your much awaited (the)code
*
* Below predicates define the winning conditions
*/


/*% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.*/
minimax(Pos, BestNextPos, Val) :-                     
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-                   
    utility(Pos, Val).


best([Pos], Pos, Val) :-
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-
    minimax(Pos1, _, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).



betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   
    min_to_move(Pos0),                      
    Val0 > Val1, !                         
    ;
    max_to_move(Pos0),                     
    Val0 < Val1, !.                           

betterOf(_, _, Pos1, Val1, Pos1, Val1).        






/*% move(+Pos, -NextPos)
% True if there is a legal (according to rules) move from Pos to NextPos.*/
move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    winPos(X1, NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    drawPos(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard).

/*% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board with an empty case replaced by Player mark.*/
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).


/*% min_to_move(+Pos)
% True if the next player to play is the MIN player.*/
min_to_move([o, _, _]).

/*% max_to_move(+Pos)
% True if the next player to play is the MAX player.*/
max_to_move([x, _, _]).

/*% utility(+Pos, -Val) :-
% True if Val the the result of the evaluation function at Pos.
% We will only evaluate for final position.
% So we will only have MAX win, MIN win or draw.
% We will use  1 when MAX win
%             -1 when MIN win
%              0 otherwise.*/
utility([o, win, _], 1).     
utility([x, win, _], -1).     
utility([_, draw, _], 0).

/*% winPos(+Player, +Board)
% True if Player win in Board.*/
winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    equal(X1, X2, X3, P) ;   
    equal(X4, X5, X6, P) ;    
    equal(X7, X8, X9, P) ;   
    equal(X1, X4, X7, P) ;  
    equal(X2, X5, X8, P) ;
    equal(X3, X6, X9, P) ;  
    equal(X1, X5, X9, P) ;    
    equal(X3, X5, X7, P).     

/*% drawPos(+Player, +Board)
% True if the game is a draw.*/
drawPos(_,Board) :-
    \+ member(0, Board).


/*% equal(+W, +X, +Y, +Z).
% True if W = X = Y = Z.*/
equal(X, X, X, X).





/*% bestMove(+Pos, -NextPos)
% Compute the best Next Position from Position Pos
% with minimax or alpha-beta algorithm.*/
bestMove(Pos, NextPos) :-
    minimax(Pos, NextPos, _).


/*% play
% Start the game.*/
play :-
    nl,
    write('===================='), nl,
	  write('= Prolog TicTacToe ='), nl,
	  write('===================='), nl, nl,
	  write('Rem : x starts the game'), nl,
	  playAskColor.
	
	
	
/*% playAskColor
% Ask the color for the human player and start the game with it.*/
playAskColor :-
	  nl, write('Color for human player ? (x or o)'), nl,
	  read(Player), nl,
	  (
	    Player \= o, Player \= x, !,    
	    write('Error : not a valid color !'), nl,
	    playAskColor                     
	    ;
	    EmptyBoard = [0, 0, 0, 0, 0, 0, 0, 0, 0],
	    show(EmptyBoard), nl,
	
	    
	    play([x, play, EmptyBoard], Player)
	  ).


/*% play(+Position, +HumanPlayer)
% If next player to play in position is equal to HumanPlayer -> Human must play
% Ask to human what to do.*/
play([Player, play, Board], Player) :- !,
    nl, write('Next move ?'), nl,
    read(Pos), nl,                                
    (
      humanMove([Player, play, Board], [NextPlayer, State, NextBoard], Pos), !,
      show(NextBoard),
      (
        State = win, !,                            
        nl, write('End of game : '),
        write(Player), write(' win !'), nl, nl
        ;
        State = draw, !,                 
        nl, write('End of game : '),
        write(' draw !'), nl, nl
        ;
        play([NextPlayer, play, NextBoard], Player) 
      )
      ;
      write('-> Bad Move !'), nl,              
      play([Player, play, Board], Player)        
    ).



/*% play(+Position, +HumanPlayer)
% If it is not human who must play -> Computer must play
% Compute the best move for computer with minimax or alpha-beta.*/
play([Player, play, Board], HumanPlayer) :-
    nl, write('Computer play : '), nl, nl,
    % Compute the best move
    bestMove([Player, play, Board], [NextPlayer, State, BestSuccBoard]),
    show(BestSuccBoard),
    (
      State = win, !,                              
      nl, write('End of game : '),
      write(Player), write(' win !'), nl, nl
      ;
      State = draw, !,                           
      nl, write('End of game : '), write(' draw !'), nl, nl
      ;
      
      play([NextPlayer, play, BestSuccBoard], HumanPlayer)
    ).



/*% nextPlayer(X1, X2)
% True if X2 is the next player to play after X1.*/
nextPlayer(o, x).
nextPlayer(x, o).

/*% When human play*/
humanMove([X1, play, Board], [X2, State, NextBoard], Pos) :-
    nextPlayer(X1, X2),
    set1(Pos, X1, Board, NextBoard),
    (
      winPos(X1, NextBoard), !, State = win ;
      drawPos(X1,NextBoard), !, State = draw ;
      State = play
    ).



/*% set1(+Elem, +Pos, +List, -ResList).
% Set Elem at Position Pos in List => Result in ResList.
% Rem : counting starts at 1.*/
set1(1, E, [X|Ls], [E|Ls]) :- !, X = 0.

set1(P, E, [X|Ls], [X|L2s]) :-
    number(P),
    P1 is P - 1,
    set1(P1, E, Ls, L2s).


/*% show(+Board)
% Show the board to current output.*/
show([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    write('   '), show2(X1),
    write(' | '), show2(X2),
    write(' | '), show2(X3), nl,
    write('  -----------'), nl,
    write('   '), show2(X4),
    write(' | '), show2(X5),
    write(' | '), show2(X6), nl,
    write('  -----------'), nl,
    write('   '), show2(X7),
    write(' | '), show2(X8),
    write(' | '), show2(X9), nl.



/*% show2(+Term)
% Write the term to current outupt
% Replace 0 by ' '.*/
show2(X) :-
    X = 0, !,
    write(' ').

show2(X) :-
    write(X).
