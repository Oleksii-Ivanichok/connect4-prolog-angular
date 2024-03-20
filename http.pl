:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- dynamic top/2.
:- dynamic size/2.
:- dynamic piece/3.
:- dynamic win/1.
:- dynamic lose/1.
:- dynamic eval/3.
:- dynamic score/2.
:- dynamic depth/1.
:- dynamic strategy/1.

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% start_server(8080).

:- http_handler('/start-game', handle_startGame, []).


handle_startGame(Request) :-
    http_read_json_dict(Request, Dict),
    get_dict(height, Dict, Height),
    get_dict(width, Dict, Width),
    get_dict(depth, Dict, Depth),
    newGame(Height, Width, Depth),
    ResponseDict = _{game: 'started'},
	reply_json_dict(ResponseDict).

:- http_handler('/selected', handle_selectedColumn, []).

handle_selectedColumn(Request) :-
	http_read_json_dict(Request, Dict),
	get_dict(column, Dict, Column),
	game(Column, Response),
    reply_json_dict(Response).

test(Column, Response) :-
	Response = _{column: Column},
% TO START the Game ?-newGame(6,7).

score(r,0).
score(y,0).

%%%%%%%%  INIT GameBoard %%%%%%%%

init_top(0):-!.
init_top(C):-assert(top(C,0)),NewC is C-1, init_top(NewC) .

init(C,R):-
	retractall(size(_,_)),
	retractall(depth(_)),
	retractall(score(_,_)),
	retractall(strategy(_)),
	assert(score(r,0)),
	assert(score(y,0)),
	assert(size(C,R)),
	retractall(top(_,_)),
	init_top(C),
	retractall(piece(_,_,_)),
	resetStatus().

%%%%  WIN/LOSE Checking %%%%%%%%%%%%

resetStatus():-
	retractall(win(_)),
	retractall(lose(_)).

checkStatus(X,Y,Color):-
	check(X,Y),
	(   Color == r -> assert(win(r)),assert(lose(y));
	    Color == y -> assert(win(y)),assert(lose(r))
	).
checkStatus(_,_,_).

draw():-
	size(C,R),
	aggregate_all(count,top(_,R),Cnt),
	Cnt == C.

traverse(C,R,IncC,IncR,Res):-
       NewC is C + IncC,
       NewR is R + IncR,
       piece(C,R,C1),
       piece(NewC,NewR,C2),
       C1 == C2,
       traverse(NewC,NewR,IncC,IncR,Res1),
       Res is Res1 + 1,!.
traverse(_,_,_,_,Res):-
	Res is 1.
%Horzintal Check
check(X,Y):-
	traverse(X,Y,1,0,R1),
	traverse(X,Y,-1,0,R2),
	R is R1 + R2 - 1  ,
	R >= 4,!.
%Vertical Check
check(X,Y):-
	traverse(X,Y,0,1,R1),
	traverse(X,Y,0,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.
%Main Diagonal Check
check(X,Y):-
	traverse(X,Y,1,1,R1),
	traverse(X,Y,-1,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.
%Secondary Diagonal Check
check(X,Y):-
	traverse(X,Y,1,-1,R1),
	traverse(X,Y,-1,1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.


%%%%%%%%%  IO Rules %%%%%%%%%%%%%

print():-
	size(_,Y),
	print(1,Y).

print(C,R):-
	size(X,_),
	C == X,
	R == 1,
	piece(C,R,_),!.
print(C,R):-
	size(X,_),
	C == X,
	R == 1,
	not(piece(C,R,_)),!.

print(C,R):-
	size(X,_),
	NewC is C+1,
	piece(C,R,_),
	C < X ,
	print(NewC,R),!.
print(C,R):-
	size(X,_),
	NewC is C+1 ,
	not(piece(C,R,_)),
	C < X   ,
	print(NewC,R),!.

print(_,R):-
	NewR is R-1,
	R >= 0,
	print(1,NewR).

scan(Color, Column):-
    C is Column,
	insert(C,Color),
	top(C,R),
	print(),
	(checkStatus(C,R,Color);!
	),!.
scan(Color, Column):-
	scan(Color, Column).

printAIlist([]).
printAIlist([H|T]):-
	getResult(H,S,_),
	(
	    abs(S) < 1000000000;
	    S =< -1000000000,
	    S >= 1000000000
	),
	printAIlist(T).

%%%%%%%%%%%  Tool Rules  %%%%%%%%%%%%%

insert(C,Color):-
	size(_,R),
	top(C,H),
	H < R,
	TmpH is H + 1,
        retractall(top(C,_)),
        assert(top(C,TmpH)),
        assert(piece(C,TmpH,Color)),
	score(Color,V),
	eval(C,TmpH,E),
	V1 is V + E,
	retractall(score(Color,_)),
	assert(score(Color,V1));
	true.


remove(C):-
	top(C,H),
	piece(C,H,Color),
	retractall(piece(C,H,_)),
	TmpH is H - 1,
        retractall(top(C,_)),
        assert(top(C,TmpH)),
	score(Color,V),
	eval(C,H,E),
	V1 is V - E,
	retractall(score(Color,_)),
	assert(score(Color,V1));
	true.

%%%%%%%%%%%%  Start Game Rules %%%%%%%%%%%%%%

newGame(C,R, Depth):-
	init(C,R),
	createEvalTable(),
	S is 1,
	assert(strategy(S)),
	(   S == 1 ->
			D is Depth,
	        assert(depth(D));
	    true
	).

	game(Column, Response):-
		scan(r, Column),
		win(r),!;
		strategy(S),
		(   S == 1 ->
				   depth(D),
				   doBest(y,D,_,Move),
				   Response = _{column: Move},
				   insert(Move,y),
				   print(),
				   top(Move,H),
				   (
					 check(Move,H);
						 not(check(Move,H))
				   )
		).
	

%%%%%%%%%%%%%   AI Rules   %%%%%%%%%%%%%%

getResult([H,T|_],H,T).

doBest(Color, Depth, BestScore, BestMove):-
	size(C,_),
	setof(X,between(1,C,X),MoveList),
	minimax(MoveList,Depth, Color, Result),
	sort(Result,SortedResult),
	(depth(D),Depth == D,printAIlist(Result);
	true),
	(   Color == r ->
			  nth0(0,SortedResult,Move),
			  getResult(Move,BestScore,BestMove);
	    Color == y ->
	                  length(SortedResult,Len),
			  nth1(Len,SortedResult,Move),
	                  getResult(Move,BestScore,BestMove)
	).

minimax([],_,_,[]).
minimax([H|T],Depth, Color, L):-
	Color == r,
	minimax(T,Depth,Color,L1),
	top(H,Height),
	size(_,R),
	(
	    Height == R -> L = L1,!;

	    insert(H,Color),
	    top(H,Hight),
	    checkStatus(H,Hight,Color),
	    (    win(r) ->
	            V0 is -1000000000  * (Depth + 1),
		    append([[V0,H]],L1,L);
		 win(y)	->
		    V0 is 1000000000   * (Depth + 1),
		    append([[V0,H]],L1,L);
                 draw() ->
		    append([[0,H]],L1,L);
	         not(win(r);win(y);draw())  ->
	          (
		    Depth == 0 ->
		        score(r,Vr),score(y,Vy),
		        V0 is Vy - Vr,
		        append([[V0,H]],L1,L);

		    Depth > 0   ->
		       NewDep is Depth - 1,
		       doBest(y, NewDep, BestScore,_),
		       append([[BestScore,H]],L1,L)
		 )
	    ),
	    resetStatus(),
	    remove(H)
	).



minimax([H|T],Depth, Color, L):-
	Color == y,
	minimax(T,Depth,Color,L1),
	top(H,Height),
	size(_,R),
	(
	    Height == R -> L = L1,!;

	    insert(H,Color),
	    top(H,Hight),
	    checkStatus(H,Hight,Color),
	    (    win(r) ->
	            V0 is -1000000000  * (Depth + 1),
		    append([[V0,H]],L1,L);
		 win(y) ->
	            V0 is 1000000000   * (Depth + 1),
		    append([[V0,H]],L1,L);
		 draw() ->
	            append([[0,H]],L1,L);

	        not(win(r);win(y);draw())  ->
	         (  Depth == 0 ->
		     score(r,Vr),score(y,Vy),
		     V0 is Vy - Vr,
		     append([[V0,H]],L1,L);

		   Depth > 0   ->
		    NewDep is Depth - 1,
		    doBest(r, NewDep, BestScore,_),
		    append([[BestScore,H]],L1,L)
		 )
	    ),
	    resetStatus(),
	    remove(H)
	).

%%%%%%%%%%%%  Evaluation Rules %%%%%%%%

createEvalTable():-
	size(_,R),
	retractall(eval(_,_,_)),
	initEvalRow(R),
	calcEvalRow(R).


initEvalCol(_,0):-!.
initEvalCol(CurRow,CurCol):-
	NewCol is CurCol - 1,
	initEvalCol(CurRow,NewCol),
	assert(eval(CurRow,CurCol,0)).

initEvalRow(0):-!.
initEvalRow(CurRow):-
	NewRow is CurRow - 1,
	initEvalRow(NewRow),
	size(C,_),
	initEvalCol(CurRow,C).


calcEvalCol(_,0):-!.
calcEvalCol(CurRow,CurCol):-
	NewCol is CurCol - 1,
	calcEvalCol(CurRow,NewCol),
	calcHor(CurRow,CurCol),
	calcVer(CurRow,CurCol),
	calcUpDia(CurRow,CurCol),
	calcDownDia(CurRow,CurCol).

calcEvalRow(0):-!.
calcEvalRow(CurRow):-
	NewRow is CurRow - 1,
	calcEvalRow(NewRow),
	size(C,_),
	calcEvalCol(CurRow,C).

calcHor(X,Y):-
	(Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	eval(X,Y,E),eval(X,Y1,E1),eval(X,Y2,E2),eval(X,Y3,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X,Y1,E1)),assert(eval(X,Y1,Ne1)),
	Ne2 is E2+1,retractall(eval(X,Y2,E2)),assert(eval(X,Y2,Ne2)),
	Ne3 is E3+1,retractall(eval(X,Y3,E3)),assert(eval(X,Y3,Ne3)));
	true.


calcVer(X,Y):-
	(   X1 is X+1, X2 is X+2, X3 is X+3,
	eval(X,Y,E),eval(X1,Y,E1),eval(X2,Y,E2),eval(X3,Y,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X1,Y,E1)),assert(eval(X1,Y,Ne1)),
	Ne2 is E2+1,retractall(eval(X2,Y,E2)),assert(eval(X2,Y,Ne2)),
	Ne3 is E3+1,retractall(eval(X3,Y,E3)),assert(eval(X3,Y,Ne3)));
	true.


calcUpDia(X,Y):-
	(   X1 is X+1, X2 is X+2, X3 is X+3,
	Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	eval(X,Y,E),eval(X1,Y1,E1),eval(X2,Y2,E2),eval(X3,Y3,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X1,Y1,E1)),assert(eval(X1,Y1,Ne1)),
	Ne2 is E2+1,retractall(eval(X2,Y2,E2)),assert(eval(X2,Y2,Ne2)),
	Ne3 is E3+1,retractall(eval(X3,Y3,E3)),assert(eval(X3,Y3,Ne3)));
	true.

calcDownDia(X,Y):-(
	X1 is X-1, X2 is X-2, X3 is X-3,
	Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	eval(X,Y,E),eval(X1,Y1,E1),eval(X2,Y2,E2),eval(X3,Y3,E3),
	Ne  is E+1 ,retractall(eval(X,Y,E))  ,assert(eval(X,Y,Ne)),
	Ne1 is E1+1,retractall(eval(X1,Y1,E1)),assert(eval(X1,Y1,Ne1)),
	Ne2 is E2+1,retractall(eval(X2,Y2,E2)),assert(eval(X2,Y2,Ne2)),
	Ne3 is E3+1,retractall(eval(X3,Y3,E3)),assert(eval(X3,Y3,Ne3)));
        true.