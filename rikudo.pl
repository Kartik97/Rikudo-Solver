createList([],[],[]).
createList([(A,B,C)|T],[(A,B)|T1],[C|T2]):-createList(T,T1,T2).

isMember(X,[X|T]).
isMember(X,[Y|T]):-isMember(X,T).

removeDup([],L,[]).
removeDup([X|T],L,T1):-isMember(X,L),removeDup(T,L,T1).
removeDup([X|T],L,[X|T1]):-not isMember(X,L),removeDup(T,L,T1).