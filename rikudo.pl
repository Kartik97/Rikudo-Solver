createList([],[],[]).
createList([(A,B,C)|T],[(A,B)|T1],[C|T2]):-createList(T,T1,T2).

isMember(X,[X|T]).
isMember(X,[Y|T]):-isMember(X,T).

removeDup([],L,[]).
removeDup([X|T],L,T1):-isMember(X,L),removeDup(T,L,T1).
removeDup([X|T],L,[X|T1]):-not isMember(X,L),removeDup(T,L,T1).

rikudo(Size,):-

smm:- L = [S,E,N,D,M,O,R,Y],Digits=[0,1,2,3,4,5,6,7,8,9],assign_digits(L,Digits).

select(Z,[Z|R],R).
select(Z,[Y|Zs],[Y|Ys]):-select(Z,Zs,Ys).

assign_digits([],List).
assign_digits([D|Ds],List):-select(D,List,NewList),assign_digits(Ds,NewList).