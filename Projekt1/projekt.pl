display_lab([],_).
display_lab([H]) :- print_row(H),!.
display_lab([H|T]) :- print_row(H), nl,  display_lab(T).

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

%modyfikować listę i wypisać labirynt


replace(_, _, [],_,_,_).
replace(O, R, [H|T], [N|T2],X,Y) :- replace1(O, R, H, N,X,Y), Y1 is Y+1, replace(O, R, T, T2,X,Y1).

replace1(_, _, [], [],_,_).
replace1(O, R, [O|T], [R|T2],X,Y) :- X1 is X+1, replace1(O, R, T, T2,X1,Y).
replace1(O, R, [H|T], [H|T2],X,Y) :- X1 is X+1, H \= O, replace1(O, R, T, T2,X1,Y).



display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).


%display_lab([[o, x, x, x, x],
%[o, o, o, o, x],
%[x, o, x, o, x],
%[x, o, x, o, x],
%[x, o, o, o, x],
%[x, x, x, x, x]]).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W), Y1 is Y+1, replace(O, R, T, T2,X,Y1,W).

replace1(_, _, [], [],_,_,_).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2]) :- X1 is X+1, replace1(O, R, T, T2,X1,Y,[W|W2]).
replace1(O, R, [H|T], [H|T2],X,Y,[W|W2]) :- X1 is X+1, H \= O, replace1(O, R, T, T2,X1,Y,[W|W2]).


replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).



replace(o,.,[[o, x, x, x, x],
 [o, o, o, o, x],
 [x, o, x, o, x],
 [x, o, x, o, x],
 [x, o, o, o, x],
 [o, o, x, x, x]],R,1,1, [[1,1],[2,1],[2,2],[3,2],[4,2],[5,2],[6,1],[6,2]]),

display_lab(R),




%%%%%%%%%%%%%%%%%%DISPLAY%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.
display_lab(Maze,Path):- sort(Path,SortedPath), replace(o,.,Maze,R,1,1,SortedPath), display_lab(R),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W,Z), X1 is X+1, replace(O, R, T, T2,X1,Y,Z).

replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).


%%%%%%Replace 1d list%%%%%%%%
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.
display_lab(Maze,Path):- sort(Path,SortedPath), replace(o,.,Maze,R,1,1,SortedPath), display_lab(R),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W,Z), X1 is X+1, replace(O, R, T, T2,X1,Y,Z).

replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).

nth0_2(Row, Column, List, Element):-Row2 is Row-1, Column2 is Column-1, 
    nth0_2(Row2, Column2, List, Element,1).
nth0_2(Row, Column, List, Element,_) :-
    nth0(Row, List, SubList),
    nth0(Column, SubList, Element).

size2d([L|_],R):-length(L,R).
neighbourUp(X,Y, Maze,[X1,Y]):- X1 is X-1, X1 > 0 , nth0_2(X1,Y, Maze,El), El == o.
neighbourDown(X,Y,Maze, [X1,Y]):- X1 is X+1, length(Maze,Len), X1 =< Len, nth0_2(X1,Y, Maze,El), El == o.
neighbourLeft(X,Y,Maze,[X,Y1]):- Y1 is Y-1, Y1 > 0, nth0_2(X,Y1, Maze,El), El == o.
neighbourRight(X,Y,Maze, [X,Y1]):- Y1 is Y+1, size2d(Maze,Len), Y1 =< Len, nth0_2(X,Y1, Maze,El), El == o.

getAllNeighbours(X,Y,Maze,List):- 
    neighbourUp(X,Y,Maze,List); 
    neighbourDown(X,Y,Maze,List);
    neighbourLeft(X,Y,Maze,List);
    neighbourRight(X,Y,Maze,List).
path(Maze,S,F,Path):- path(Maze,S,F,Path,[]).
path(_,C,C,P,Acc):- append(P,[C],Acc).
path(_,_,_,D,D).

%path(Maze,[X,Y],[XN,YN],Path):-getAllNeighbours(X,Y,Maze,P), Path = [P],
%    Visited = [[X,Y]].



path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,S,S,_).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),
    append(Visited,[Nextpoint],NewVisited),
    append(Pat,[Nextpoint],NewPat),
    path(Maze,Nextpoint,End,Path,NewVisited,NewPat).
path(Maze,Start,End,Path,Visited,[P|Pat]):-\+move(Maze,Start,Nextpoint,Visited), 
	path(Maze,Start,End,Path,Visited,Pat]





    path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,Path,ReversedPath,_):-reverse(ReversedPath,Path).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint|Pat]).
path(Maze,Start,End,Path,Visited,[P|Pat]):-(\+move(Maze,Start,_,Visited)), 
	path(Maze,P,End,Path,Visited,Pat).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.
display_lab(Maze,Path):- sort(Path,SortedPath), replace(o,.,Maze,R,1,1,SortedPath), display_lab(R),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W,Z), X1 is X+1, replace(O, R, T, T2,X1,Y,Z).

replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).

nth0_2(Row, Column, List, Element):-Row2 is Row-1, Column2 is Column-1, 
    nth0_2(Row2, Column2, List, Element,1).
nth0_2(Row, Column, List, Element,_) :-
    nth0(Row, List, SubList),
    nth0(Column, SubList, Element).

size2d([L|_],R):-length(L,R).
neighbourUp(X,Y, Maze,[X1,Y]):- X1 is X-1, X1 > 0 , nth0_2(X1,Y, Maze,El), El == o.
neighbourDown(X,Y,Maze, [X1,Y]):- X1 is X+1, length(Maze,Len), X1 =< Len, nth0_2(X1,Y, Maze,El), El == o.
neighbourLeft(X,Y,Maze,[X,Y1]):- Y1 is Y-1, Y1 > 0, nth0_2(X,Y1, Maze,El), El == o.
neighbourRight(X,Y,Maze, [X,Y1]):- Y1 is Y+1, size2d(Maze,Len), Y1 =< Len, nth0_2(X,Y1, Maze,El), El == o.

getAllNeighbours(X,Y,Maze,List):- 
    neighbourUp(X,Y,Maze,List); 
    neighbourDown(X,Y,Maze,List);
    neighbourLeft(X,Y,Maze,List);
    neighbourRight(X,Y,Maze,List).

move(Maze,[X,Y],Ns,Visited):- getAllNeighbours(X,Y,Maze,Ns), \+member(Ns,Visited),!.

path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,Path,_,):-reverse(ReversedPath,Path).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint|Pat]).
path(Maze,Start,End,Path,Visited,[P|Pat]):-(\+move(Maze,Start,_,Visited)), 
	path(Maze,P,End,Path,Visited,Pat).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.
display_lab(Maze,Path):- sort(Path,SortedPath), replace(o,.,Maze,R,1,1,SortedPath), display_lab(R),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W,Z), X1 is X+1, replace(O, R, T, T2,X1,Y,Z).

replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).

nth0_2(Row, Column, List, Element):-Row2 is Row-1, Column2 is Column-1, 
    nth0_2(Row2, Column2, List, Element,1).
nth0_2(Row, Column, List, Element,_) :-
    nth0(Row, List, SubList),
    nth0(Column, SubList, Element).

size2d([L|_],R):-length(L,R).
neighbourUp(X,Y, Maze,[X1,Y]):- X1 is X-1, X1 > 0 , nth0_2(X1,Y, Maze,El), El == o.
neighbourDown(X,Y,Maze, [X1,Y]):- X1 is X+1, length(Maze,Len), X1 =< Len, nth0_2(X1,Y, Maze,El), El == o.
neighbourLeft(X,Y,Maze,[X,Y1]):- Y1 is Y-1, Y1 > 0, nth0_2(X,Y1, Maze,El), El == o.
neighbourRight(X,Y,Maze, [X,Y1]):- Y1 is Y+1, size2d(Maze,Len), Y1 =< Len, nth0_2(X,Y1, Maze,El), El == o.

getAllNeighbours(X,Y,Maze,List):- 
    neighbourUp(X,Y,Maze,List); 
    neighbourDown(X,Y,Maze,List);
    neighbourLeft(X,Y,Maze,List);
    neighbourRight(X,Y,Maze,List).

move(Maze,[X,Y],Ns,Visited):- getAllNeighbours(X,Y,Maze,Ns), \+member(Ns,Visited),!.

path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,Path,_,ReversedPath):-reverse(ReversedPath,Path),!.
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint|Pat]).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),\+member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint,Start|Pat]).
path(Maze,Start,End,Path,Visited,[P|Pat]):-(\+move(Maze,Start,_,Visited)), 
	path(Maze,P,End,Path,Visited,Pat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.
display_lab(Maze,Path):- sort(Path,SortedPath), replace(o,.,Maze,R,1,1,SortedPath), display_lab(R),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W,Z), X1 is X+1, replace(O, R, T, T2,X1,Y,Z).

replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).

nth0_2(Row, Column, List, Element):-Row2 is Row-1, Column2 is Column-1, 
    nth0_2(Row2, Column2, List, Element,1).
nth0_2(Row, Column, List, Element,_) :-
    nth0(Row, List, SubList),
    nth0(Column, SubList, Element).

size2d([L|_],R):-length(L,R).
neighbourUp(X,Y, Maze,[X1,Y]):- X1 is X-1, X1 > 0 , nth0_2(X1,Y, Maze,El), El == o.
neighbourDown(X,Y,Maze, [X1,Y]):- X1 is X+1, length(Maze,Len), X1 =< Len, nth0_2(X1,Y, Maze,El), El == o.
neighbourLeft(X,Y,Maze,[X,Y1]):- Y1 is Y-1, Y1 > 0, nth0_2(X,Y1, Maze,El), El == o.
neighbourRight(X,Y,Maze, [X,Y1]):- Y1 is Y+1, size2d(Maze,Len), Y1 =< Len, nth0_2(X,Y1, Maze,El), El == o.

getAllNeighbours(X,Y,Maze,List):- 
    neighbourUp(X,Y,Maze,List); 
    neighbourDown(X,Y,Maze,List);
    neighbourLeft(X,Y,Maze,List);
    neighbourRight(X,Y,Maze,List).

move(Maze,[X,Y],Ns,Visited):- getAllNeighbours(X,Y,Maze,Ns), \+member(Ns,Visited),!.

path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,Path,_,ReversedPath):-reverse(ReversedPath,Path),!.
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint|Pat]).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),\+member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint,Start|Pat]).
path(Maze,Start,End,Path,Visited,[P|Pat]):-(\+move(Maze,Start,_,Visited)), 
	path(Maze,P,End,Path,Visited,Pat).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%display_lab([]).
display_lab([H]) :- print_row(H).
display_lab([H|T]) :- print_row(H), nl,  display_lab(T),!.
display_lab(Maze,Path):- sort(Path,SortedPath), replace(o,.,Maze,R,1,1,SortedPath), display_lab(R),!.

print_row([H|T]):- print_row(H), tab(1), print_row(T),!.
print_row([]).
print_row(o):- tab(1).
print_row(C):- write(C).

replace(_, _, [],_,_,_,_).
replace(O, R, [H|T], [N|T2],X,Y,W) :- replace1(O, R, H, N,X,Y,W,Z), X1 is X+1, replace(O, R, T, T2,X1,Y,Z).

replace1(_, _, [], [],_,_,E,E).
replace1(O, R, [O|T], [R|T2],X,Y,[W|W2],Q) :- Y1 is Y+1, [A,B|_]=W,A==X,B==Y, replace1(O, R, T, T2,X,Y1,W2,Q).
replace1(O, R, [H|T], [H|T2],X,Y,W2,Q) :- Y1 is Y+1, replace1(O, R, T, T2,X,Y1,W2,Q).

nth0_2(Row, Column, List, Element):-Row2 is Row-1, Column2 is Column-1, 
    nth0_2(Row2, Column2, List, Element,1).
nth0_2(Row, Column, List, Element,_) :-
    nth0(Row, List, SubList),
    nth0(Column, SubList, Element).

size2d([L|_],R):-length(L,R).
neighbourUp(X,Y, Maze,[X1,Y]):- X1 is X-1, X1 > 0 , nth0_2(X1,Y, Maze,El), El == o.
neighbourDown(X,Y,Maze, [X1,Y]):- X1 is X+1, length(Maze,Len), X1 =< Len, nth0_2(X1,Y, Maze,El), El == o.
neighbourLeft(X,Y,Maze,[X,Y1]):- Y1 is Y-1, Y1 > 0, nth0_2(X,Y1, Maze,El), El == o.
neighbourRight(X,Y,Maze, [X,Y1]):- Y1 is Y+1, size2d(Maze,Len), Y1 =< Len, nth0_2(X,Y1, Maze,El), El == o.

getAllNeighbours(X,Y,Maze,List):- 
    neighbourUp(X,Y,Maze,List); 
    neighbourDown(X,Y,Maze,List);
    neighbourLeft(X,Y,Maze,List);
    neighbourRight(X,Y,Maze,List).

move(Maze,[X,Y],Ns,Visited):- getAllNeighbours(X,Y,Maze,Ns), \+member(Ns,Visited),!.

path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,Path,_,ReversedPath):-reverse(ReversedPath,Path),!.
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint|Pat]).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),\+member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint,Start|Pat]).
path(Maze,Start,End,Path,Visited,[P|Pat]):-(\+move(Maze,Start,_,Visited)), 
	path(Maze,P,End,Path,Visited,Pat).
