createListPre([],[],[]).
createListPre([(A,B,C)|T],[(A,B)|T1],[C|T2]):-createListPre(T,T1,T2).   /*  R1=pair       R2=nos list */

isMember(X,[X|T]).
isMember(X,[Y|T]):-isMember(X,T).

removeDup([],L,[]).                                           /*  Remove occurrences of ele of first list in second   */
removeDup([X|T],L,T1):-isMember(X,L),removeDup(T,L,T1).
removeDup([X|T],L,[X|T1]):-not isMember(X,L),removeDup(T,L,T1).

defSize(37,-3,3,-3,-6).     /*        1=size, 2=lowerHeight 3=upperHeight 4=lowerWidth 5=MiddleWidth  */
defSize(61,-4,4,-4,-8).
defSize(91,-5,5,-5,-10).

concat([],[],[]).
concat([],M,M).
concat([H|T],M,[H|R]):-concat(T,M,R).

createListH(0,0,H,[(0,H)]):-H=\=0.
createListH(0,0,H,[]):-H=:=0.
createListH(L,R,H,[]):-L>R.
createListH(L,R,H,[(L,H),(R,H)|T]):-L<R, L1 is L+2,R1 is R-2,createListH(L1,R1,H,T).

createList(M,M,H,F1):-L is M,R is M*(-1),createListH(L,R,H,F1).                           /* pass H>0   (upperheight)*/
createList(B,M,H,Res):-B>M , B1 is B-1 , L1 is B , R1 is B*(-1) , H1 is H*(-1), Hpassed is H-1,  /* 1=lowerWidth 2=MidddleWidth */
							createListH(L1,R1,H,List1),createListH(L1,R1,H1,List2),createList(B1,M,Hpassed,List3),
							concat(List1,List2,Res1),concat(Res1,List3,Res).

createNumbers(1,[1]).
createNumbers(S,[S|T]):-S>0,N is S-1,createNumbers(N,T).

createPuzzle(S,ON,OL):-AS is S-1,defSize(S,A,B,C,D),createNumbers(AS,ON),createList(C,D,B,OL).

putNumbers(AN):- L=[(1,2),(3,4),(5,6),(7,8)],D=[1,2,3,4],assign_digits(L,D,AN).

select((A,B),[C|R],R,(A,B,C)).
select((A,B),[Y|Zs],[Y|Ys],AN):-select((A,B),Zs,Ys,AN).

assign_digits([],List,[]).
assign_digits([D|Ds],List,[Ele1|List2]):-select(D,List,NewList,Ele1),assign_digits(Ds,NewList,List2).


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


rikudo(Size,Pre,Links,Res):-S is Size-1,createPuzzle(Size,OutNum,OutPairs),createListPre(Pre,Pair,Num),
							removeDup(OutPairs,Pair,PairList),removeDup(OutNum,Num,NumList),
							assign_digits(PairList,NumList,Assigned),concat(Assigned,Pre,Assigned_Full),
							checkCons(S,Assigned_Full,Assigned_Full).