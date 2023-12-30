krawedz(a,b).
krawedz(b,c).
krawedz(b,d).
krawedz(c,e).
krawedz(c,f).
krawedz(d,f).
krawedz(f,e).
sciezka_p(X,Y,s(0)):- krawedz(X,Y).
sciezka_p(X,Y,s(D1)):- krawedz(X,Z), sciezka_p(Z,Y,D1).

//sciezka_p(b,X,s(s(0))).