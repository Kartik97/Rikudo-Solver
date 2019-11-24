defSize(19,-2,2,-2,-4).
defSize(37,-3,3,-3,-6).     /*        1=size, 2=lowerHeight 3=upperHeight 4=lowerWidth 5=MiddleWidth  */
defSize(61,-4,4,-4,-8).
defSize(91,-5,5,-5,-10).

findMin([],(F,D,M),(F,D,M)).
findMin([(A,B,C)|T],(F,D,M),Min):-C<M,findMin(T,(A,B,C),Min).
findMin([(A,B,C)|T],(F,D,M),Min):-C>M,findMin(T,(F,D,M),Min).

abs(X,X):-X>=0,!.
abs(X,X1):-X<0,X1 is X*(-1).

check1((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check1((A,B,C),H,In,In).

check2((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check2((A,B,C),H,In,In).

check3((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check3((A,B,C),H,In,In).

check4((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check4((A,B,C),H,In,In).

check5((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check5((A,B,C),H,In,In).

check6((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check6((A,B,C),H,In,In).

createNeigh(S,X,O):-defSize(S,A,B,C,D),check1(X,B,[],O1),check2(X,B,O1,O2),check3(X,B,O2,O3),check4(X,B,O3,O4),
					check5(X,B,O4,O5),check6(X,B,O5,O).

isMember(X,[X|T]).
isMember(X,[Y|T]):-isMember(X,T).

selectCurrent((A,B),[C|R],R,(A,B,C)).
selectCurrent((A,B),[Y|Zs],[Y|Ys],AN):-selectCurrent((A,B),Zs,Ys,AN).

selectCurrentNeigh([X|T],X).
selectCurrentNeigh([X|T],Y):-selectCurrentNeigh(T,Y).

find((A,B,C),[(A,B,C)|T]).                /*  Finds in Prefilled list         */
find((A,B,C),[(X,Y,Z)|T]):-find((A,B,C),T).

isNeigh((A,B),(X,Y)):-SA is (A-X),SB is (B-Y),abs(SA,Sa),abs(SB,Sb),Sum is Sa+Sb,Sum=:=2.

isLinked((X,Y),[(X,Y,C,D)|T],(C,D)).           /* Finds links  */
isLinked((X,Y),[(A,B,C,D)|T],R):-isLinked((X,Y),T,R).

/*
findPath(Size,(A,B),Avail,Links,C,Avail):-Ex is Size-1,C=:=Ex.
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,find((Xval,Yval,NextC),Avail),isNeigh((A,B),(Xval,Yval)),findPath(Size,(Xval,Yval),Avail,NextC,Res).
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,not find((Xval,Yval,NextC),Avail),createNeigh(Size,(A,B,C),Neigh),selectCurrentNeigh(Neigh,(X,Y)),
								not isMember((X,Y,_),Avail),findPath(Size,(X,Y),[(X,Y,NextC)|Avail],NextC,Res).
*/
findPath(Size,(A,B),Avail,Links,C,Avail):-Ex is Size-1,C=:=Ex.
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,find((Xval,Yval,NextC),Avail),isNeigh((A,B),(Xval,Yval)),findPath(Size,(Xval,Yval),Avail,NextC,Res).
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,not find((Xval,Yval,NextC),Avail),createNeigh(Size,(A,B,C),Neigh),selectCurrentNeigh(Neigh,(X,Y)),
								not isMember((X,Y,_),Avail),findPath(Size,(X,Y),[(X,Y,NextC)|Avail],NextC,Res).

rikudo(Size,Pre,Links,Res):-findMin(Pre,(95,95,95),(A,B,C)),findPath(Size,(A,B),Pre,Links,C,Res).

/*
19
[(-2,2,16),(0,2,15),(-1,1,13),(-2,0,12),(-1,-1,11),(1,-1,5),(-2,-2,9),(2,2,1)]

37
[(3,3,36),(6,0,1),(5,1,5),(-4,0,14),(-3,1,11),(-4,2,32),(-1,-1,16),(-4,-2,28),(2,-2,20),(3,-3,24)]
*/