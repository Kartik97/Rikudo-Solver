putNumbers(AN):- L=[(1,2),(3,4),(5,6),(7,8)],D=[1,2,3,4],assign_digits(L,D,AN).

select((A,B),[C|R],R,(A,B,C)).
select((A,B),[Y|Zs],[Y|Ys],AN):-select((A,B),Zs,Ys,AN).

assign_digits([],List,[]).
assign_digits([D|Ds],List,[Ele1|List2]):-select(D,List,NewList,Ele1),assign_digits(Ds,NewList,List2).

isMember(X,[X|T]).
isMember(X,[Y|T]):-isMember(X,T).

check1((A,B,C),L,X,X1):-X1 is X+1,A1 is A-2,B1 is B,C1 is C+1,isMember((A1,B1,C1),L).
check1((A,B,C),L,X,X1):-X1 is X+1,A1 is A-2,B1 is B,C1 is C-1,isMember((A1,B1,C1),L).
check1((A,B,C),L,X,X1):-X1 is X.

check2((A,B,C),L,X,X1):-X1 is X+1,A1 is A+2,B1 is B,C1 is C+1,isMember((A1,B1,C1),L).
check2((A,B,C),L,X,X1):-X1 is X+1,A1 is A+2,B1 is B,C1 is C-1,isMember((A1,B1,C1),L).
check2((A,B,C),L,X,X1):-X1 is X.

check3((A,B,C),L,X,X1):-X1 is X+1,A1 is A-1,B1 is B+1,C1 is C+1,isMember((A1,B1,C1),L).
check3((A,B,C),L,X,X1):-X1 is X+1,A1 is A-1,B1 is B+1,C1 is C-1,isMember((A1,B1,C1),L).
check3((A,B,C),L,X,X1):-X1 is X.

check4((A,B,C),L,X,X1):-X1 is X+1,A1 is A+1,B1 is B+1,C1 is C+1,isMember((A1,B1,C1),L).
check4((A,B,C),L,X,X1):-X1 is X+1,A1 is A+1,B1 is B+1,C1 is C-1,isMember((A1,B1,C1),L).
check4((A,B,C),L,X,X1):-X1 is X.

check5((A,B,C),L,X,X1):-X1 is X+1,A1 is A-1,B1 is B-1,C1 is C+1,isMember((A1,B1,C1),L).
check5((A,B,C),L,X,X1):-X1 is X+1,A1 is A-1,B1 is B-1,C1 is C-1,isMember((A1,B1,C1),L).
check5((A,B,C),L,X,X1):-X1 is X.

check6((A,B,C),L,X,X1):-X1 is X+1,A1 is A+1,B1 is B-1,C1 is C+1,isMember((A1,B1,C1),L).
check6((A,B,C),L,X,X1):-X1 is X+1,A1 is A+1,B1 is B-1,C1 is C-1,isMember((A1,B1,C1),L).
check6((A,B,C),L,X,X1):-X1 is X.

checkCons(S,[],L).
checkCons(S,[(A,B,C)|T],L):-C=\=1,C=\=S,check1((A,B,C),L,0,C1),check2((A,B,C),L,C1,C2),check3((A,B,C),L,C2,C3),check4((A,B,C),L,C3,C4)
						,check5((A,B,C),L,C4,C5),check6((A,B,C),L,C5,C6),C6=:=2,checkCons(S,T,L).
checkCons(S,[(A,B,C)|T],L):-check1((A,B,C),L,0,C1),check2((A,B,C),L,C1,C2),check3((A,B,C),L,C2,C3),check4((A,B,C),L,C3,C4)
						,check5((A,B,C),L,C4,C5),check6((A,B,C),L,C5,C6),C6=:=1,checkCons(S,T,L).