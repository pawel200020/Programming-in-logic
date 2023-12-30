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

move(Maze,[X,Y],Ns,Visited):- getAllNeighbours(X,Y,Maze,Ns), \+member(Ns,Visited).

path(Maze,Start,End,Path):-path(Maze,Start,End,Path,[Start],[Start]).
path(_,C,C,Path,_,[C|ReversedPath]):-reverse([C|ReversedPath],Path).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint|Pat]).
path(Maze,Start,End,Path,Visited,Pat):-move(Maze,Start,Nextpoint,Visited),\+member(Start,Pat),
    path(Maze,Nextpoint,End,Path,[Nextpoint|Visited],[Nextpoint,Start|Pat]).
path(Maze,Start,End,Path,Visited,[P|Pat]):-(\+move(Maze,Start,_,Visited)), 
	path(Maze,P,End,Path,Visited,Pat).