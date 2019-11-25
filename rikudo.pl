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

check2((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check2((A,B,C),H,In,In).

check3((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check3((A,B,C),H,In,In).

check4((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check4((A,B,C),H,In,In).

check5((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
check5((A,B,C),H,In,In).

check6((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),Ch=\=0,!.
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

isLinked((X,Y),[(X,Y,C,D)|T],(C,D)).
isLinked((X,Y),[(A,B,X,Y)|T],(A,B)).           /* Finds links  */
isLinked((X,Y),[(A,B,C,D)|T],R):-isLinked((X,Y),T,R).

removeLink(X,[],[]).
removeLink(X,[X|T],T).
removeLink(X,[Y|T],[Y|R]):-removeLink(X,T,R).

selectLink((A,B),[(A,B,C,D)|T],(C,D),(A,B,C,D)).
selectLink((A,B),[(C,D,A,B)|T],(C,D),(C,D,A,B)).
selectLink((A,B),[_|T],R,X):-selectLink((A,B),T,R,X).

countLink((A,B),[],Cin,Cin):-!.
countLink((A,B),[(A,B,C,D)|T],Cin,Cout):-TempC is Cin+1,!,countLink((A,B),T,TempC,Cout).
countLink((A,B),[(C,D,A,B)|T],Cin,Cout):-TempC is Cin+1,!,countLink((A,B),T,TempC,Cout).
countLink((A,B),[_|T],Cin,Cout):-countLink((A,B),T,Cin,Cout).

findPath(Size,(A,B),Avail,Links,C,Avail,Links):-Ex is Size-1,C>=Ex,!.
findPath(Size,(A,B),Avail,Links,C,Res,Nlinks):-NextC is C+1,createNeigh(Size,(A,B,C),Neigh),
										(find((Xval,Yval,NextC),Avail) ->
											(
												isLinked((A,B),Links,(Xl,Yl)) ->
												(
													isLinked((A,B),Links,(Xval,Yval))->
													(
														selectLink((A,B),Links,(Xval,Yval),X),
														removeLink(X,Links,Rlinks),
														findPath(Size,(Xval,Yval),Avail,Rlinks,NextC,Res,Nlinks)
													);
													(
														countLink((A,B),Links,0,1),C=\=1,
														isNeigh((Xval,Yval),Neigh),
														findPath(Size,(Xval,Yval),Avail,Links,NextC,Res,Nlinks)	
													)
												);
												(
													isNeigh((Xval,Yval),Neigh),
													findPath(Size,(Xval,Yval),Avail,Links,NextC,Res,Nlinks)
												)
											);
											(
												isLinked((A,B),Links,(Xl,Yl)) ->
												(
													not isMember((Xl,Yl,_),Avail),
													countLink((A,B),Links,0,Count),Count=:=1,
													selectLink((A,B),Links,(Xl,Yl),X),
													removeLink(X,Links,Rlinks),
													findPath(Size,(Xl,Yl),[(Xl,Yl,NextC)|Avail],Rlinks,NextC,Res,Nlinks)
												);
												(
													selectCurrentNeigh(Neigh,(X,Y)),
													not isMember((X,Y,_),Avail),
													findPath(Size,(X,Y),[(X,Y,NextC)|Avail],Links,NextC,Res,Nlinks)
												)
											)
										).

findPathBack(Size,(A,B),Avail,Links,1,Avail,Links).
findPathBack(Size,(A,B),Avail,Links,C,Res,Nlinks):-NextC is C-1,createNeigh(Size,(A,B,C),Neigh),
										(find((Xval,Yval,NextC),Avail) ->
											(
												isLinked((A,B),Links,(Xl,Yl)) ->
												(
													isLinked((A,B),Links,(Xval,Yval))->
													(
														selectLink((A,B),Links,(Xval,Yval),X),
														removeLink(X,Links,Rlinks),
														findPathBack(Size,(Xval,Yval),Avail,Rlinks,NextC,Res,Nlinks)
													);
													(
														countLink((A,B),Links,0,1),C=\=1,
														isNeigh((Xval,Yval),Neigh),
														findPathBack(Size,(Xval,Yval),Avail,Links,NextC,Res,Nlinks)	
													)
												);
												(
													isNeigh((Xval,Yval),Neigh),
													findPathBack(Size,(Xval,Yval),Avail,Links,NextC,Res,Nlinks)
												)
											);
											(
												isLinked((A,B),Links,(Xl,Yl)) ->
												(
													not isMember((Xl,Yl,_),Avail),
													countLink((A,B),Links,0,Count),Count=:=1,
													selectLink((A,B),Links,(Xl,Yl),X),
													removeLink(X,Links,Rlinks),
													findPathBack(Size,(Xl,Yl),[(Xl,Yl,NextC)|Avail],Rlinks,NextC,Res,Nlinks)
												);
												(
													selectCurrentNeigh(Neigh,(X,Y)),
													not isMember((X,Y,_),Avail),
													findPathBack(Size,(X,Y),[(X,Y,NextC)|Avail],Links,NextC,Res,Nlinks)
												)
											)
										).

findLinkFree([(A,B,C)|T],Links,(A,B,C)):-countLink((A,B),Links,0,Out),Out=:=0,!.
findLinkFree([(A,B,C)|T],Links,R):-findLinkFree(T,Links,R).

rikudo(37,[],Links,Res):-findPath(37,(3,-3),[(-3,-3,1)],Links,1,Res1,Nlinks),Res=[(0,0,-10)|Res1],!.
rikudo(61,[],Links,Res):-findPath(61,(4,-4),[(-4,-4,1)],Links,1,Res1,Nlinks),Res=[(0,0,-10)|Res1],!.
rikudo(91,[],Links,Res):-findPath(91,(-5,5),[(-5,-5,1)],Links,1,Res1,Nlinks),Res=[(0,0,-10)|Res1],!.
rikudo(Size,Pre,Links,Res):-findMin(Pre,(95,95,95),(A,B,1)) ->
							(
								findPath(Size,(A,B),Pre,Links,1,Res1,Nlinks),
								Res=[(0,0,-10)|Res1],!
							);
							(	
								findLinkFree(Pre,Links,(A1,B1,C1)) ->
								(
									findPath(Size,(A1,B1),Pre,Links,C1,Res1,Nlinks),
									findPathBack(Size,(A1,B1),Res1,Nlinks,C1,Res2,Nlinks2),
									Res=[(0,0,-10)|Res2],!
								);
								(
									findMin(Pre,(95,95,95),(A,B,C)),
									(
										(findPath(Size,(A,B),Pre,Links,C,Res1,Nlinks),
										findPathBack(Size,(A,B),Res1,Nlinks,C,Res2,Nlinks2));
										(findPathBack(Size,(A,B),Pre,Links,C,Res1,Nlinks),
										findPath(Size,(A,B),Res1,Nlinks,C,Res2,Nlinks2))
									),
									Res=[(0,0,-10)|Res2],!
								)
							).