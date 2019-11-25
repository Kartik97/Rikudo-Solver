--Without if version--

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


---Combined Links version---

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


----------------------------------

													selectLink((A,B),Links,(Xl,Yl)),
													Xl=:=Xval,Yl=:=Yval,isNeigh((Xval,Yval),Neigh),
													removeLink((A,B,Xval,Yval),Links,Rlinks),
													findPath(Size,(Xval,Yval),Avail,Rlinks,NextC,Res)