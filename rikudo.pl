defSize(19,-2,2,-2,-4).
defSize(37,-3,3,-3,-6).     /*        1=size, 2=lowerHeight 3=upperHeight 4=lowerWidth 5=MiddleWidth  */
defSize(61,-4,4,-4,-8).
defSize(91,-5,5,-5,-10).

findMin([],(F,D,M),(F,D,M)).
findMin([(A,B,C)|T],(F,D,M),Min):-C<M,findMin(T,(A,B,C),Min).
findMin([(A,B,C)|T],(F,D,M),Min):-C>M,findMin(T,(F,D,M),Min).

abs(X,X):-X>=0,!.
abs(X,X1):-X<0,X1 is X*(-1).

/*
slides------bottom left
*/

check1((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check1((A,B,C),H,In,In).

check2((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check2((A,B,C),H,In,In).

check3((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check3((A,B,C),H,In,In).

check4((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check4((A,B,C),H,In,In).

check5((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check5((A,B,C),H,In,In).

check6((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check6((A,B,C),H,In,In).

createNeigh(S,X,O):-defSize(S,A,B,C,D),check1(X,B,[],O1),check2(X,B,O1,O2),check3(X,B,O2,O3),check4(X,B,O3,O4),
					check5(X,B,O4,O5),check6(X,B,O5,O).

isMember(X,[X|T]).
isMember(X,[Y|T]):-isMember(X,T).

selectCurrentNeigh([X|T],X).
selectCurrentNeigh([X|T],Y):-selectCurrentNeigh(T,Y).

find((A,B,C),[(A,B,C)|T]).                /*  Finds C in Prefilled list         */
find((A,B,C),[(X,Y,Z)|T]):-find((A,B,C),T).

isNeigh((X,Y),[(X,Y)|T]).
isNeigh((X,Y),[_|T]):-isNeigh((X,Y),T).

isLinked((X,Y),[],(X,Y)).
isLinked((X,Y),[(X,Y,C,D)|T],(C,D)).
isLinked((X,Y),[(A,B,X,Y)|T],(A,B)).           /* Finds links  */
isLinked((X,Y),[(A,B,C,D)|T],R):-isLinked((X,Y),T,R).

findPath(Size,(A,B),Avail,C,Avail):-Ex is Size-1,C>=Ex,!.
findPath(Size,(A,B),Avail,C,Res):-NextC is C+1,createNeigh(Size,(A,B,C),Neigh),
								(find((Xval,Yval,NextC),Avail) ->
								(isNeigh((Xval,Yval),Neigh),findPath(Size,(Xval,Yval),Avail,NextC,Res));
								selectCurrentNeigh(Neigh,(X,Y)),
								not isMember((X,Y,_),Avail),findPath(Size,(X,Y),[(X,Y,NextC)|Avail],NextC,Res)
								).

findPathBack(Size,(A,B),Avail,1,Avail).
findPathBack(Size,(A,B),Avail,C,Res):-NextC is C+1,createNeigh(Size,(A,B,C),Neigh),
								(find((Xval,Yval,NextC),Avail) ->
								(isNeigh((Xval,Yval),Neigh),findPathBack(Size,(Xval,Yval),Avail,NextC,Res));
								selectCurrentNeigh(Neigh,(X,Y)),
								not isMember((X,Y,_),Avail),findPathBack(Size,(X,Y),[(X,Y,NextC)|Avail],NextC,Res)
								).
/*
findPath(Size,(A,B),Avail,C,Avail):-Ex is Size-1,C>=Ex,!.
findPath(Size,(A,B),Avail,C,Res):-NextC is C+1,find((Xval,Yval,NextC),Avail),createNeigh(Size,(A,B,C),Neigh),
								isNeigh((Xval,Yval),Neigh),findPath(Size,(Xval,Yval),Avail,NextC,Res).
findPath(Size,(A,B),Avail,C,Res):-NextC is C+1,not find((Xval,Yval,NextC),Avail),createNeigh(Size,(A,B,C),Neigh),
								selectCurrentNeigh(Neigh,(X,Y)),
								not isMember((X,Y,_),Avail),findPath(Size,(X,Y),[(X,Y,NextC)|Avail],NextC,Res).

findPathBack(Size,(A,B),Avail,1,Avail).
findPathBack(Size,(A,B),Avail,C,Res):-NextC is C-1,find((Xval,Yval,NextC),Avail),createNeigh(Size,(A,B,C),Neigh),
									isNeigh((Xval,Yval),Neigh),findPathBack(Size,(Xval,Yval),Avail,NextC,Res).
findPathBack(Size,(A,B),Avail,C,Res):-NextC is C-1,not find((Xval,Yval,NextC),Avail),createNeigh(Size,(A,B,C),Neigh),
									selectCurrentNeigh(Neigh,(X,Y)),
									not isMember((X,Y,_),Avail),findPathBack(Size,(X,Y),[(X,Y,NextC)|Avail],NextC,Res).
*/
/*
findPath(Size,(A,B),Avail,Links,C,Avail):-Ex is Size-1,C>=Ex,!.
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,find((Xval,Yval,NextC),Avail),isNeigh((A,B),(Xval,Yval)),
										isLinked((A,B),Links,(A,B)),
										findPath(Size,(Xval,Yval),Avail,Links,NextC,Res).
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,find((Xval,Yval,NextC),Avail),isNeigh((A,B),(Xval,Yval)),
										isLinked((A,B),Links,(Xval,Yval)),
										findPath(Size,(Xval,Yval),Avail,Links,NextC,Res).
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,not find((Xval,Yval,NextC),Avail),isLinked((A,B),Links,(X,Y)),
										not isMember((X,Y,_),Avail),findPath(Size,(X,Y),[(X,Y,NextC)|Avail],Links,NextC,Res).
findPath(Size,(A,B),Avail,Links,C,Res):-NextC is C+1,not find((Xval,Yval,NextC),Avail),
										createNeigh(Size,(A,B,C),Neigh),selectCurrentNeigh(Neigh,(X,Y)),
										not isMember((X,Y,_),Avail),findPath(Size,(X,Y),[(X,Y,NextC)|Avail],Links,NextC,Res).

rikudo(Size,Pre,Links,Res):-findMin(Pre,(95,95,95),(A,B,C)),findPath(Size,(A,B),Pre,Links,C,Res).
*/

findVal((X,Y),[(X,Y,C)|T],C).
findVal((X,Y),[(A,B,C)|T],Val):-findVal((X,Y),T,Val).

checkLinks([],Res).
checkLinks([(A,B,C,D)|T],Res):-findVal((A,B),Res,V1),findVal((C,D),Res,V2),V3 is (V1-V2),abs(V3,V),V=:=1,checkLinks(T,Res).   

rikudo(37,[],Links,Res):-findPath(37,(3,-3),[(-3,-3,1)],1,Res),checkLinks(Links,Res).
rikudo(61,[],Links,Res):-findPath(61,(4,-4),[(-4,-4,1)],1,Res),checkLinks(Links,Res).
rikudo(91,[],Links,Res):-findPath(91,(5,-5),[(-5,-5,1)],1,Res),checkLinks(Links,Res).
rikudo(Size,Pre,Links,Res):-(findMin(Pre,(95,95,95),(A,B,1)),findPath(Size,(A,B),Pre,1,Res),checkLinks(Links,Res));
							(findMin(Pre,(95,95,95),(A,B,C)),findPath(Size,(A,B),Pre,C,Res1),
							findPathBack(Size,(A,B),Res1,C,Res),
							checkLinks(Links,Res)).

/*
[(2,4,59),(-1,3,56),(1,3,60),(-6,2,1),(4,2,42),(-1,1,20),(2,0,26),(6,0,38),(-5,-1,6),(-2,-2,17),(4,-2,34),(-5,-3,9)]
[(3,1,4,0),(5,-1,3,-1),(1,-1,2,-2),(1,-3,0,-4),(3,-3,4,-4)]
*/

/*
[(-2,4,13),(1,3,10),(-4,2,19),(4,2,5),(1,1,1),(-6,0,53),(4,0,26),(6,0,31),(0,-2,60),(-6,-2,48),(-4,-4,45),(4,-4,40)]
[(-2,-2,-1,-3),(-2,-2,-3,-1),(5,-1,7,-1),(-1,1,-2,2),(-5,1,-6,2)]
*/

/*
[(-3,3,9),(3,3,36),(-2,2,11),(1,1,5),(5,1,33),(2,0,24),(6,0,31),(-4,0,14),(3,-1,25),(-3,-1,1),(-2,-2,19),(3,-3,28)]
[(-1,1,0,2),(-1,-1,1,-1),(-1,-1,0,-2),(0,-2,-1,-3),(-3,-3,-4,-2)]
*/