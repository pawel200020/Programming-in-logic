canvas(W, H, S) :-canvas(W,H,S,[]).
canvas(_,0,S,S):-!.
canvas(W,H,S,ARR):- canvasRow(W,Row), H1 is H-1, canvas(W, H1,S, [Row|ARR]).

canvasRow(W,R):-canvasRow(W,R,[]). 
canvasRow(0,R,R):-!.
canvasRow(W,R,ARR):- W1 is W-1,canvasRow(W1,R,[' '|ARR]).

point([],_,_,[]).
point([H|S1],[X,0],Z,[H2|S2]):- Y1 is -1, replaceRow(X,Z,H,H2), point(S1, [X, Y1], Z, S2). 
point([H|S1],[X,Y],Z,[H|S2]):- Y1 is Y-1, point(S1, [X, Y1], Z, S2),!. 
%X - kolumny
%Y - wiersz
%liczymy od 0
replaceRow(_, _, [], []).
replaceRow(0, R, [_|T], [R|T2]) :- replaceRow(-1, R, T, T2).
replaceRow(Y, R, [H|T], [H|T2]) :- Y1 is Y-1, replaceRow(Y1, R, T, T2),!.

writeHorizontalLine(_,_, _, [], []).
writeHorizontalLine(I,0,R, [_|T], [R|T2]) :- I2 is I-1, writeHorizontalLine(I2,-1, R, T, T2).
writeHorizontalLine(I,Y,R, [_|T], [R|T2]) :- I >= 0, Y<0, I2 is I-1,Y2 is Y-1, writeHorizontalLine(I2,Y2, R, T, T2).
writeHorizontalLine(I, Y, R, [H|T], [H|T2]) :- Y1 is Y-1, writeHorizontalLine(I,Y1, R, T, T2),!.


line(S1,[X1,Y1],[X2,Y2],Z,S2):- line(S1,[X1,Y1],[X2,Y2],Z,0,S2),!.
line([],[_,_],[_,_],_,_,[]).
line([H|S1],[X,Yc],[X,Yc],Z,Yc,[H2|S2]):- replaceRow(X,Z,H,H2), Yc2 is Yc+1,line(S1,[X,Yc],[X,Yc],Z,Yc2,S2).
line([H|S1],[X,Yc],[X,Y2],Z,Yc,[H2|S2]):-  replaceRow(X,Z,H,H2), Yc2 is Yc+1,line(S1,[X,Yc2],[X,Y2],Z,Yc2,S2).
line([H|S1],[X,Y1],[X,Yc],Z,Yc,[H2|S2]):- replaceRow(X,Z,H,H2), Yc2 is Yc+1,line(S1,[X,Y1],[X,Yc2],Z,Yc2,S2).
line([H|S1],[X1,Yc],[X2,Yc],Z,Yc,[H2|S2]):- X1 > X2, Xr is X1-X2, writeHorizontalLine(Xr, X2, Z,H,H2), Yc1 is Yc+1, line(S1,[X1,Yc],[X2,Yc],Z,Yc1,S2).
line([H|S1],[X1,Yc],[X2,Yc],Z,Yc,[H2|S2]):- X1 < X2, Xr is X2-X1, writeHorizontalLine(Xr, X1, Z,H,H2), Yc1 is Yc+1, line(S1,[X1,Yc],[X2,Yc],Z,Yc1,S2).
line([H|S1],[X1,Yc],[X2,Y2],Z,Yc,[H2|S2]):- X1-X2 < 0, replaceRow(X1,Z,H,H2), Xn is X1+1, Yc1 is Yc+1, line(S1,[Xn,Yc1],[X2,Y2],Z,Yc1,S2).
line([H|S1],[X1,Yc],[X2,Y2],Z,Yc,[H2|S2]):- X1-X2 > 0, replaceRow(X1,Z,H,H2), Xn is X1-1, Yc1 is Yc+1, line(S1,[Xn,Yc1],[X2,Y2],Z,Yc1,S2).
line([H|S1],[X2,Y2],[X1,Yc],Z,Yc,[H2|S2]):- X1-X2 < 0, replaceRow(X1,Z,H,H2), Xn is X1+1, Yc1 is Yc+1, line(S1,[Xn,Yc1],[X2,Y2],Z,Yc1,S2).
line([H|S1],[X2,Y2],[X1,Yc],Z,Yc,[H2|S2]):- X1-X2 > 0, replaceRow(X1,Z,H,H2), Xn is X1-1, Yc1 is Yc+1, line(S1,[Xn,Yc1],[X2,Y2],Z,Yc1,S2).
line([H|S1],[X1,Y1],[X2,Y2],Z,Yc,[H|S2]):- Yc2 is Yc+1, line(S1,[X1,Y1],[X2,Y2],Z,Yc2,S2).


poly(S1,[[X,Y]|R],Z,S2):-poly(S1,[[X,Y]|R],Z,S2,[X,Y]),!.
poly(_,[],_,_,_):-!.
poly(S1,[[X,Y]],Z,S3,[X1,Y1]):-poly(S1,[],Z,S3,[]),line(S1,[X,Y],[X1,Y1],Z,S3).
poly(S1,[[X1,Y1],[X2,Y2]|R],Z,S3,[X0,Y0]):-poly(S1,[[X2,Y2]|R],Z,S2,[X0,Y0]),line(S2,[X1,Y1],[X2,Y2],Z,S3).