prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

segment(A, B) :- 
    prefix(X, A), 
    suffix(B, X).