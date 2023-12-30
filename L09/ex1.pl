functor(t,F,N)
[a,b,c]=..L
L=..f(a,b)

subst(T,A,V,V):- atomic(T), T=A.
subst(T,A,_V,T):- atomic(T), T\=A.
subst(T,A,V,R):- compound(T), T=..[F|Tail], subst(F,A,V,FR), subs_list(Tail,A,V,FTail), R=..[FR|FTail].

subs_list([],_,_[]).
subs_List([H|Tail],T,V,[HR|RTail]):-subst(H,T,V, HR),subs_list(Tail,T,V,RTail).