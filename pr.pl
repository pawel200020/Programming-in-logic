canvas(W, H, S) :-canvas(W,H,S,[]).
canvas(_,0,S,S):-!.
canvas(W,H,S,ARR):- canvasRow(W,Row), H1 is H-1, canvas(W, H1,S, [Row|ARR]).

canvasRow(W,R):-canvasRow(W,R,[]). 
canvasRow(0,R,R):-!.
canvasRow(W,R,ARR):- W1 is W-1,canvasRow(W1,R,[' '|ARR]).