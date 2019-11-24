defSize(37,-3,3,-3,-6).     /*        1=size, 2=lowerHeight 3=upperHeight 4=lowerWidth 5=MiddleWidth  */
defSize(61,-4,4,-4,-8).
defSize(91,-5,5,-5,-10).

findMin([],(F,D,M),(F,D,M)).
findMin([(A,B,C)|T],(F,D,M),Min):-C<M,findMin(T,(A,B,C),Min).
findMin([(A,B,C)|T],(F,D,M),Min):-C>M,findMin(T,(F,D,M),Min).

abs(X,X):-X>=0,!.
abs(X,X1):-X<0,X1 is X*(-1).

check1((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),!.
check1((A,B,C),H,In,In).

check2((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+2,B1 is B,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),!.
check2((A,B,C),H,In,In).

check3((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),!.
check3((A,B,C),H,In,In).

check4((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B+1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),!.
check4((A,B,C),H,In,In).

check5((A,B,C),H,In,[(A1,B1)|In]):-A1 is A-1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),!.
check5((A,B,C),H,In,In).

check6((A,B,C),H,In,[(A1,B1)|In]):-A1 is A+1,B1 is B-1,abs(B1,AbB),abs(A1,AbA),AbB=<H,Ch is AbA+AbB,Ch=<(2*H),!.
check6((A,B,C),H,In,In).

createNeigh(S,X,O):-defSize(S,A,B,C,D),check1(X,B,[],O1),check2(X,B,O1,O2),check3(X,B,O2,O3),check4(X,B,O3,O4),
					check5(X,B,O4,O5),check6(X,B,O5,O).