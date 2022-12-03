/* YOUR CODE HERE (Problem 1, delete the following line) */
reverseL([], []).
reverseL([H], [H]).
reverseL([H | T], RevL) :- reverseL(T, RevT), append(RevT, [H], RevL).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 2, delete the following line) */
take_all(_, [], []).
take_all(X, [X | T1], T2) :- take_all(X, T1, T2).
take_all(X, [H | T1], [H | T2]) :- X \== H, take_all(X, T1, T2).

remove_duplicates([], []).
remove_duplicates([H | T1], [H | T2]) :- take_all(H, T1, Res), remove_duplicates(Res, T2).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* Your CODE HERE (Problem 3, delete the following line) */
count_elem([], X, 0).
count_elem([H | T], H, C):- count_elem(T, H, Y), C is 1 + Y.
count_elem([H | T], X, C):- H \== X, count_elem(T, X, C).

assoc_list([], []).
assoc_list([H | T], AL) :- 
    count_elem(T, H, Occ),
    take_all(H, T, Res_DL), 
    append([H-OI], ALN, AL), 
    OI is Occ + 1, 
    assoc_list(Res_DL, ALN).

?- assoc_list([1], [1-1]).
?- assoc_list([1,1,2,2,2,3,1], [1-3, 2-3, 3-1]).
?- assoc_list([1,1,4,2,2,2,3,1,1,3,1], X).

/* YOUR CODE HERE (Problem 4, delete the following line) */
is_member(H1, [H1 | T1]).
is_member(X,  [H2 | T2]) :- is_member(X, T2).

intersectionL(_, [], []).
intersectionL([], _, []).
intersectionL([H1 | T1], L2, [H1 | T3]) :- is_member(H1, L2), intersectionL(T1, L2, T3).
intersectionL([H1 | T1], L2, L3) :- not(is_member(H1, L2)), intersectionL(T1, L2, L3).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
/* qs from lecture slides */
partition_qs([],Y,[],[]).
partition_qs([X|Xs],Y,[X|Ls],Rs) :- X =< Y, partition_qs(Xs,Y,Ls,Rs).
partition_qs([X|Xs],Y,Ls,[X|Rs]) :- X > Y, partition_qs(Xs,Y,Ls,Rs).

quicksort([],[]).
quicksort([H|T],SL) :- 
    partition_qs(T,H,Ls,Rs), quicksort(Ls,SLs), quicksort(Rs,SRs), append(SLs,[H|SRs],SL).

first_three([A, B, C], S) :- S is A + B + C.
first_three([A, B, C |_], S) :- S is A + B + C.

maxL3(L, MS3) :- 
    length(L, X),
    X >= 3,
    quicksort(L, SL),
    reverseL(SL, RSL),
    first_three(RSL, MS3).

?- not(maxL3([1], X)).
?- maxL3([1,2,3,4], 9).
?- maxL3([10,3,2,3,10], X).

/* YOUR CODE HERE (Problem 6, delete the following line) */
prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

partition([], [], []).
partition([H], [H], []).
partition(L, P, S) :- 
    length(L, N),
    LP is div(N, 2),
    LS is N - div(N, 2),
    length(P, LP),
    length(S, LS), 
    append(P, S, L).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 7, delete the following line) */
merge(X, [], X).
merge([], Y, Y).
merge([H1 | T1], [H2 | T2], [H1 | T3]) :- H1 =< H2, merge(T1, [H2 | T2], T3).
merge([H1 | T1], [H2 | T2], [H2 | T3]) :- H2 < H1, merge([H1 | T1], T2, T3).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 8, delete the following line) */
mergesort([], []).
mergesort([H], [H]).
mergesort(L, SL) :-
    partition(L, P, S),
    mergesort(P, P_SRT),
    mergesort(S, S_SRT),
    merge(P_SRT, S_SRT, SL).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).