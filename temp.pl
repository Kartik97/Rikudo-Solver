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

createPuzzlePieces(S,O):-defSize(S,A,B,C,D),createList(C,D,B,O).

