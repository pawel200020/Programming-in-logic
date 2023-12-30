:-use_module(library(clpfd)).


mm([S,E,N,D,M,O,R,Y]) :-
Vars = [S,E,N,D,M,O,R,Y],
Vars ins 0..9,
S #\= 0, M #\= 0,
all_different(Vars),
S*1000 + E*100 + N*10 + D +
M*1000 + O*100 + R*10 + E
#=
M*10000 + O*1000 + N*100 + E*10 + Y,
labeling([], Vars).

z1a([T,E,S,T,J,S,U,P,R]) :-
Vars = [T,E,S,T,J,S,U,P,R],
Vars ins 0..9,
J #\= 0, S #\= 0,
all_different(Vars),
T*1000 + E*100 + S*10 + T +
J*1000 + E*100 + S*10 + T
#=
S*10000 + U*1000 + P*100 + E*10 + R,
labeling([], Vars).

z1b([D,O,M,I,A,S,T]) :-
Vars = [D,O,M,I,A,S,T],
Vars ins 0..9,
D #\= 0, M #\= 0,
 D*100 + O*10 + M +
 D*100 + O*10 + M
#=
M*100000+I*10000 + A*1000 + S*100 + T*10 + O,
labeling([], Vars).

z1C([X,Y]) :-
Vars = [X,Y],
Vars ins 0..9,
X #\= 0, Y #\= 0,
all_different(Vars),
 4*(Y*1000 +Y*100 + Y*10 +Y) + X
#=
X*10000 + Y*1000 + Y*100 + Y*10 + Y,
labeling([], Vars).

z1d([A, C, D, G, L, M, T, W]):-
Vars = [A, C, D, G, L, M, T, W],
   Vars ins 0..9,
    A #\= 0, C #\= 0,
    M #\= 0, D #\= 0, G #\= 0, 
all_different(Vars),
 (A*10+M) * M #= (A*10+M),
    (C*10+W) - (D*10+T) #= G,
    (C*10+A) - (D*10+L) #= (10*M + D), 
    (A*10+M) - (C*10+W) #= (C*10 + A),
    M + (D*10+T) #= (D*10+L),
    (10*M + D) * G #= (10*A + M),
    labeling([], Vars).

