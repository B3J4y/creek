%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
% < Namen, Matrikelnummern und Mail-Adressen ALLER Gruppenmitglieder >         %
%                                                                              %
% (Pro Gruppe sind MAXIMAL DREI Mitglieder erlaubt!)                           %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hilfspraedikate zur Ermittlung zugehoeriger Koordinatenpaare fuer eine Nummer

delta(0,0). delta(1,0). delta(0,1). delta(1,1).
range(X,Y,XX,YY) :- delta(DX,DY), XX is X+DX, YY is Y+DY.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ihre Loesung                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% creek(+M,+N,+Numbers,-Blacks)
% berechnet die schwarzen Felder ("Blacks") EINER Loesung fuer das durch "M",
% "N" und "Numbers" beschriebene MxN-Gitter.

%creek(M,N,Numbers,Blacks, Grid) :- myGrid(M,N,AllBlacks), bomb(M,N,Numbers,Grid,AllBlacks, Blacks).
creek(M,N,Numbers,Blacks, Grid) :- myGrid(M,N,AllBlacks), iBomb(M,N,Numbers,Grid,AllBlacks, Blacks).

%iBomb(M,N,Numbers, NewNumbers, Grid, NewGrid) 
iBomb(_,_,[], [], Grid, Grid).
%getting 0
iBomb(M,N,[c(f(X, Y),Count)|Numbers], NewNumbers, Grid, NewGrid) :- Count==0,
	%Alle Ecken 
	(((X==Y, X==0, X1 is X+1, Y1 is Y+1, blackOrWhite(X1,Y1, Grid, Res, w))
	;(X==0, Y==N, X1 is X+1, blackOrWhite(X1,Y, Grid, Res, w) )
	;(X==M,Y==0, Y1 is Y+1, blackOrWhite(X,Y1,Grid, Res, w))
	;(X==M,Y==N, blackOrWhite(X,Y, Grid, Res, w)));
	%Alle Raender
	((X==0, Y>0, Y<N, X1 is X+1, Y1 is Y+1, blackOrWhite(X1,Y, Grid, Back1,w), blackOrWhite(X1,Y1,Back1, Res,w))
	;(X==M, Y>0, Y<N, Y1 is Y+1, blackOrWhite(X,Y, Grid, Back1,w), blackOrWhite(X,Y1,Back1,Res,w))
	;(Y==0, X>0, X<M, Y1 is Y+1, X1 is X+1, blackOrWhite(X,Y1, Grid, Back1,w), blackOrWhite(X1,Y1,Back1,Res,w))
	;(Y==N, X>0, X<M, X1 is X+1, blackOrWhite(X,Y,Grid,Back1,w), blackOrWhite(X1,Y,Back1,Res,w)));
	%Der Rest
	(X=\=M, X=\=0, Y=\=N,Y=\=0
	, X1 is X+1, Y1 is Y+1, blackOrWhite(X,Y, Grid, Back1,w), blackOrWhite(X,Y1, Back1, Back2,w),
	blackOrWhite(X1,Y,Back2, Back3,w), blackOrWhite(X1,Y1,Back3,Res,w))
	), iBomb(M,N,Numbers, NewNumbers, Res, NewGrid).
%getting 1
iBomb(M,N,[c(f(X, Y),Count)|Numbers], NewNumbers, Grid, NewGrid) :- Count==1,
	%Alle Ecken 
	((X==Y, X==0, X1 is X+1, Y1 is Y+1, blackOrWhite(X1,Y1, Grid, Res, s))
	;(X==0, Y==N, X1 is X+1, blackOrWhite(X1,Y, Grid, Res, s) )
	;(X==M,Y==0, Y1 is Y+1, blackOrWhite(X,Y1,Grid, Res, s))
	;(X==M,Y==N, blackOrWhite(X,Y, Grid, Res, s))), 
	iBomb(M,N,Numbers, NewNumbers, Res, NewGrid).
%getting 2
iBomb(M,N,[c(f(X, Y),Count)|Numbers], NewNumbers, Grid, NewGrid) :- Count==2,
	((X==0, Y>0, Y<N, X1 is X+1, Y1 is Y+1, blackOrWhite(X1,Y, Grid, Back1, b), blackOrWhite(X1,Y1,Back1, Back2, b))
	;(X==M, Y>0, Y<N, Y1 is Y+1, blackOrWhite(X,Y, Grid, Back1,b), blackOrWhite(X,Y1,Back1,Back2,b))
	;(Y==0, X>0, X<M, Y1 is Y+1, X1 is X+1, blackOrWhite(X,Y1, Grid, Back1,b), blackOrWhite(X1,Y1,Back1,Back2,b))
	;(Y==N, X>0, X<M, X1 is X+1, blackOrWhite(X,Y,Grid,Back1,b), blackOrWhite(X1,Y,Back1,Back2,b))),
	iBomb(M,N,Numbers, NewNumbers, Back2, NewGrid).
%getting 4
iBomb(M,N,[c(f(X, Y),Count)|Numbers], NewNumbers, Grid, NewGrid) :- Count==4,
	X=\=M, X=\=0, Y=\=N,Y=\=0
	,X1 is X+1, Y1 is Y+1, blackOrWhite(X,Y, Grid, Back1,b), blackOrWhite(X,Y1, Back1, Back2,b),
	blackOrWhite(X1,Y,Back2, Back3,b), blackOrWhite(X1,Y1,Back3,Back4,b),
	iBomb(M,N,Numbers, NewNumbers, Back4, NewGrid).
%no bomb
iBomb(M,N,[c(f(X, Y),Count)|Numbers], [c(f(X, Y),Count)|NewNumbers], Grid, NewGrid) :- Count>0, Count<4,
	((Count==1,((X==Y, X=\=0)
	;(X=\=0, Y==N)
	;(X=\=M,Y==0)
	;(X=\=M,Y==N))
	;(X==0, Y=\=N)
	;(X==M,Y=\=0)
	;(X==M,Y=\=N));
	(Count==2,(X>0, X<M, Y>0, Y<N));
	(Count==3)), iBomb(M,N,Numbers, NewNumbers, Grid, NewGrid).

%blackOrWhite(X,Y,Blacks,Back, Bow)
blackOrWhite(X,Y,Grid,NewGrid,Bow):- delete(Grid, p(f(X,Y),_),Grid1),append([p(f(X,Y),Bow)], Grid1, NewGrid).

%grid(+X,+Y, -Grid)
%Macht ein volles Grid, ohne Abstufungen
myGrid(X,Y, []) :- ((X==Y, X=<0);(X=<0); (Y=<0)).
myGrid(X,Y, [p(f(X,Y),t)|Grid]) :- X>0, Y>0, X1 is X-1, Y1 is Y-1,
	myGrid(X1,Y, Grid1), row(X,Y1,Grid2), union(Grid1, Grid2, Grid).

%row(+X,+Y,-Row)
row(X,Y,[]) :- ((X==Y, X=<0);(X=<0); (Y=<0)).
row(X,Y,[p(f(X,Y),t)|Grid]) :-Y>0, Y1 is Y-1,row(X,Y1,Grid).

%union(+Liste, +Liste, - Vereinigung) 
union([A|B], C, D) :- member(A,C), !, union(B,C,D).
union([A|B], C, [A|D]) :- union(B,C,D).
union([],Z,Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Einige Eingaben mit ihren jeweiligen Loesungen                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grid(1,3,4,[c(f(0,2),1),c(f(2,1),0),c(f(2,3),4)]).

grid(2,4,3,[c(f(1,0),1),c(f(1,2),4),c(f(3,0),1),c(f(3,2),2),c(f(4,2),1)]).

grid(3,4,4,[c(f(0,1),0),c(f(0,4),1),c(f(4,0),0),
            c(f(2,1),2),c(f(2,2),3),c(f(2,4),1),c(f(3,3),4)]).

grid(4,5,5,[c(f(0,2),2),c(f(0,3),2),c(f(1,1),2),c(f(1,4),1),
            c(f(2,1),2),c(f(2,2),1),c(f(2,3),1),c(f(3,1),2),
            c(f(3,2),1),c(f(3,3),1),c(f(4,1),2),c(f(4,4),2),c(f(5,2),2)]).

grid(5,5,5,[c(f(0,4),0),c(f(1,1),3),c(f(2,2),0),c(f(2,4),3),
            c(f(3,1),3),c(f(3,3),1),c(f(4,4),1),c(f(5,1),1),c(f(5,5),1)]).

grid(6,6,4,[c(f(1,1),3),c(f(1,3),3),c(f(2,2),2),c(f(2,3),1),
            c(f(3,1),0),c(f(3,3),2),c(f(4,2),1),c(f(5,1),3),c(f(5,3),3)]).

grid(7,6,6,[c(f(0,2),1),c(f(0,3),0),c(f(1,1),2),c(f(1,4),3),
            c(f(2,0),1),c(f(2,5),1),c(f(3,1),3),c(f(3,2),3),c(f(3,4),2),
            c(f(5,1),2),c(f(5,3),2),c(f(5,5),2)]).

% http://www.janko.at/Raetsel/Creek/001.a.htm
grid(8,10,10,[c(f(0,1),2),c(f(0,9),1),c(f(1,6),4),c(f(1,9),3),
              c(f(2,0),1),c(f(2,2),1),c(f(2,3),3),c(f(2,10),1),
              c(f(3,4),2),c(f(3,7),2),c(f(3,8),3),c(f(4,2),4),c(f(4,5),3),
              c(f(5,4),0),c(f(5,10),1),c(f(6,1),2),c(f(6,4),2),c(f(6,7),2),
              c(f(7,3),3),c(f(7,6),3),c(f(8,1),4),c(f(8,4),1),c(f(8,9),2),
              c(f(9,4),3),c(f(9,6),2),c(f(9,8),3),c(f(10,1),1),c(f(10,10),0)]).

% http://www.janko.at/Raetsel/Creek/023.a.htm
grid(9,10,10,[c(f(0,0),0),c(f(1,4),2),c(f(1,5),0),c(f(1,8),2),
              c(f(2,1),4),c(f(2,5),2),c(f(2,7),3),c(f(3,0),1),
              c(f(3,6),3),c(f(3,8),0),c(f(4,2),4),c(f(4,9),2),
              c(f(5,4),4),c(f(5,8),2),c(f(5,10),0),c(f(6,3),2),
              c(f(6,7),2),c(f(7,2),4),c(f(7,10),2),c(f(8,5),4),c(f(8,8),3),
              c(f(9,9),0),c(f(10,0),1),c(f(10,2),1),c(f(10,3),2),c(f(10,4),1)]).


solution(1,[[f(1,1),f(1,3),f(1,4),f(2,3),f(2,4),f(3,3),f(3,4)],
            [       f(1,3),f(1,4),f(2,3),f(2,4),f(3,3),f(3,4)]]).

solution(2,[[f(1,1),f(1,2),f(1,3),f(2,2),f(2,3),f(3,3),f(4,1),f(4,3)]]).

solution(3,[[f(1,4),f(2,2),f(3,2),f(3,3),f(3,4),f(4,3),f(4,4)],
            [f(1,4),f(2,2),f(3,2),f(3,3),f(3,4),f(4,2),f(4,3),f(4,4)]]).

solution(4,[[f(1,1),f(1,2),f(1,3),f(1,4),f(3,1),f(3,2),f(3,4),f(3,5),
             f(5,1),f(5,2),f(5,3),f(5,4),f(5,5)],
            [f(1,1),f(1,2),f(1,3),f(1,4),f(3,1),f(3,2),f(3,4),
             f(5,1),f(5,2),f(5,3),f(5,4),f(5,5)],
            [f(1,1),f(1,2),f(1,3),f(1,4),f(3,1),f(3,2),f(3,4),f(3,5),
             f(4,5),f(5,1),f(5,2),f(5,3),f(5,5)],
            [f(1,1),f(1,2),f(1,3),f(1,4),f(3,1),f(3,2),f(3,4),
             f(4,5),f(5,1),f(5,2),f(5,3),f(5,5)]]).

solution(5,[[f(1,1),f(1,2),f(2,1),f(2,4),f(2,5),f(3,1),f(3,5),
             f(4,1),f(4,2),f(4,3),f(5,1),f(5,5)],
            [f(1,1),f(1,2),f(2,1),f(2,4),f(2,5),f(3,1),f(3,4),
             f(4,1),f(4,2),f(5,1),f(5,5)],
            [f(1,1),f(1,2),f(2,1),f(2,4),f(3,1),f(3,4),f(3,5),
             f(4,1),f(4,2),f(5,1),f(5,5)],
            [f(1,1),f(1,2),f(2,1),f(2,5),f(3,1),f(3,4),f(3,5),
             f(4,1),f(4,2),f(5,1),f(5,5)],
            [f(1,1),f(1,2),f(1,3),f(2,1),f(2,5),f(3,1),f(3,4),f(3,5),
             f(4,1),f(4,2),f(5,1),f(5,5)]]).

solution(6,[[f(1,1),f(1,2),f(1,3),f(1,4),f(2,2),f(2,3),f(4,3),f(4,4),
             f(5,1),f(5,4),f(6,1),f(6,2),f(6,3),f(6,4)]]).

solution(7,[[f(1,1),f(1,2),f(1,5),f(2,4),f(2,5),
             f(3,1),f(3,2),f(4,2),f(4,3),f(4,4),f(4,5),
             f(6,1),f(6,2),f(6,3),f(6,4),f(6,5),f(6,6)],
            [f(1,1),f(1,2),f(1,5),f(1,6),f(2,4),f(2,5),
             f(3,1),f(3,2),f(4,2),f(4,3),f(4,4),f(4,5),
             f(6,1),f(6,2),f(6,3),f(6,4),f(6,5),f(6,6)]]).

solution(8,[[f(1,1),f(1,2),f(1,6),f(1,7),f(1,10),f(2,1),f(2,4),f(2,6),
             f(2,7),f(2,9),f(2,10),f(3,3),f(3,4),f(3,9),f(4,2),f(4,3),
             f(4,5),f(4,6),f(4,7),f(4,8),f(4,9),f(5,2),f(5,3),f(5,6),
             f(6,8),f(6,10),f(7,1),f(7,2),f(7,3),f(7,4),f(7,5),f(7,6),f(7,7),
             f(8,1),f(8,2),f(8,3),f(8,7),f(8,9),f(9,1),f(9,2),f(9,5),f(9,9),
             f(10,1),f(10,4),f(10,5),f(10,6),f(10,7),f(10,8),f(10,9)]]).

solution(9,[[f(1,4),f(2,1),f(2,2),f(2,4),f(2,7),f(2,8),f(2,9),
             f(3,1),f(3,2),f(3,5),f(3,6),f(3,7),f(4,2),f(4,3),f(4,7),f(4,10),
             f(5,2),f(5,3),f(5,4),f(5,5),f(5,9),f(6,4),f(6,5),f(6,7),f(6,8),
             f(7,2),f(7,3),f(7,10),f(8,2),f(8,3),f(8,5),f(8,6),f(8,8),f(8,9),
             f(8,10),f(9,5),f(9,6),f(9,8),f(10,1),f(10,3),f(10,4)]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testpraedikate                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_all/0: Testet alle vordefinierten Eingaben                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_all :- test_all([1,2,3,4,5,6,7,8,9]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_all(+TestCases): Testet alle Eingaben in TestCases                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_all([]).
test_all([TestCase | TestCases]) :-
  write('Testcase '), write(TestCase), write(': '),
  test(TestCase), nl, test_all(TestCases).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test(+TestCase): Testet eine einzelne Eingabe                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(TestCase) :-
  grid(TestCase,M,N,Numbers),
  solution(TestCase,AllBlacks),
  ( test_creek(M,N,Numbers,AllBlacks) -> write(success) ; write(failure) ).

test_creek(M,N,Numbers,AllBlacks) :-
  AllBlacks == [] ->
    not(creek(M,N,Numbers,_))
  ;
    (
      creek(M,N,Numbers,Blacks), !,
      ground(Blacks),
      sort(Blacks,SortedBlacks),
      member(SortedBlacks,AllBlacks)
    ).
