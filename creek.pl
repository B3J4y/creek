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

creek(M,N,Numbers,Blacks):-myGrid(M,N,WholeTGrid), iBomb(M,N,Numbers,Numbs,WholeTGrid, BombedGrid), loopMyGrid(Numbs,BombedGrid, NewGrid), 
	getFirtstWhite(NewGrid,p(f(X,Y),w)),getWhiteSnake(X,Y,NewGrid,WhiteSnake), not(member(p(f(_,_),w),WhiteSnake)),
	%member(p(f(X,Y),b),WhiteSnake.
	bagof(f(X1,Y1),member(p(f(X1,Y1),b),WhiteSnake), Blacks), !. %gridToBlack

%loopMyGrid([],Grid, Grid):-not(bagof(p(f(X,Y),t),member(p(f(X,Y),t),Grid),_)).
loopMyGrid([],Grid, NewGrid):-whiteTs(Grid,[],WhitedGrid), 
	((bagof(p(f(X,Y),T),(member(p(f(X,Y),t),WhitedGrid)),Ts),blackSureFields(Ts,WhitedGrid,NewGrid));
		(not(bagof(p(f(X,Y),T),(member(p(f(X,Y),t),WhitedGrid)),_)), WhitedGrid=NewGrid)).
loopMyGrid(Numbers,Grid, NewGrid) :- length(Numbers,I), I>0,insertAtEnd(f(x, x), Numbers, EndNumbers),
	sureshot(EndNumbers,SureNumbs,Grid,SureGrid), whiteTs(SureGrid,SureNumbs,WhitedGrid),
	weightFields(SureNumbs,WhitedGrid,WeightedGrid), colorMaxElements(WeightedGrid,ColoredMax),
	((bagof(p(f(X,Y),t),(member(p(f(X,Y),t),ColoredMax)),Ts),blackSureFields(Ts,ColoredMax,BlackGrid));
		(not(bagof(p(f(X,Y),t),(member(p(f(X,Y),t),ColoredMax)),_)), BlackGrid=ColoredMax))
	, weightedToT(BlackGrid,NewTGrid),loopMyGrid(SureNumbs,NewTGrid, NewGrid).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Workplace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getFirtstWhite([p(f(X,Y),w)|_], p(f(X,Y),w)).
getFirtstWhite([p(f(_,_),T)|List], Res):- T\=w, getFirtstWhite(List, Res),!.

getWhiteSnake(X,Y,Grid,Grid):-member(p(f(X,Y),T),Grid), T\==w.
getWhiteSnake(X,Y,Grid,Res):-member(p(f(X,Y),w),Grid), delete( Grid, p(f(X,Y),w),Res1), X1 is X-1, X2 is X+1, Y1 is Y-1, Y2 is Y+1,
	upStream(X,Y2,Res1, Res2), rightStream(X2,Y,Res2, Res3), leftStream(X1,Y,Res3, Res4) ,downStream(X,Y1,Res4, Res), !.
	%getWhiteSnake(X1,Y,Grid,Res1),getWhiteSnake(X2,Y,Grid,Res2),getWhiteSnake(X,Y1,Grid,Res3),getWhiteSnake(X,Y2,Grid,Res4),
	
upStream(X,Y,Grid,Grid) :- member(p(f(X,Y),T),Grid), T\==w.
upStream(X,Y,Grid,Grid) :- not(member(p(f(X,Y),_),Grid)).
upStream(X,Y,Grid, Res) :- member(p(f(X,Y),w),Grid), delete( Grid, p(f(X,Y),w),Res1), X1 is X-1, X2 is X+1, Y1 is Y+1,
	upStream(X,Y1,Res1,Res2), rightStream(X2,Y,Res2,Res3), leftStream(X1,Y,Res3,Res).

downStream(X,Y,Grid,Grid) :- member(p(f(X,Y),T),Grid), T\==w.
downStream(X,Y,Grid,Grid) :- not(member(p(f(X,Y),_),Grid)).
downStream(X,Y,Grid, Res) :- member(p(f(X,Y),w),Grid), delete( Grid, p(f(X,Y),w),Res1),X1 is X-1, X2 is X+1, Y1 is Y-1,
	downStream(X,Y1,Res1,Res2), rightStream(X2,Y,Res2,Res3), leftStream(X1,Y,Res3,Res).

rightStream(X,Y,Grid,Grid) :- member(p(f(X,Y),T),Grid), T\==w.
rightStream(X,Y,Grid,Grid) :- not(member(p(f(X,Y),_),Grid)).
rightStream(X,Y,Grid,Res) :-member(p(f(X,Y),w),Grid), delete( Grid, p(f(X,Y),w),Res1), X2 is X+1, Y1 is Y-1, Y2 is Y+1,
	upStream(X,Y2,Res1, Res2), rightStream(X2,Y,Res2,Res3),downStream(X,Y1,Res3, Res).

leftStream(X,Y,Grid,Grid) :- member(p(f(X,Y),T),Grid), T\==w.
leftStream(X,Y,Grid,Grid) :- not(member(p(f(X,Y),_),Grid)).
leftStream(X,Y,Grid,Res) :-member(p(f(X,Y),w),Grid), delete( Grid, p(f(X,Y),w),Res1),X1 is X-1, Y1 is Y-1, Y2 is Y+1,
	upStream(X,Y2,Res1, Res2), leftStream(X1,Y,Res2,Res3),downStream(X,Y1,Res3, Res).
%colorMaxElements(+Grid,-Grid)
%colors the Element with the maximum vaulue
colorMaxElements(Grid,Grid):- not(bagof(p(f(FX,FY),w(WX,WY)),(member(p(f(FX,FY),w(WX,WY)),Grid)), _)).
colorMaxElements(Grid,NewGrid):- bagof(p(f(FX,FY),w(WX,WY)),(member(p(f(FX,FY),w(WX,WY)),Grid)), WeightedList),
	maxInList(WeightedList,Max),
	bagof(p(f(FX2,FY2),w(WX2,WY2)),(member(p(f(FX2,FY2),w(WX2,WY2)),WeightedList), Temp is WX2+WY2, Temp==Max), MaxList),
	member(p(f(FX3,FY3),w(_,_)),MaxList),!,(blackOrWhite(FX3,FY3,Grid, NewGrid,b);blackOrWhite(FX3,FY3,Grid, NewGrid,w)). 
	%writeln(FX3 + " " + FY3 + " " + NewGrid). %(member(p(f(FX3,FY3),w(WX3,WY3)),MaxList),WY3==0);




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% done predicates, ordered alphabetically
%countWoB(+RangedNumbs,+Color,-Count)
%blackOrWhite(+X,+Y,+Grid,-Back, +Bow)
%blackSureFields(List,+Grid,-Liste)
%iBomb(M,N,Numbers, NewNumbers, Grid, NewGrid)
%insertAtEnd(+Element,+List,-NewList)
%listToElementBow(+Numbers,+Grid,+Color,-NewGrid)
%max(+Numb1,+Numb2,-Res)
%maxInList(+Grid, -Maximum)
%myGrid(+X,+Y, -Grid)
%neighbour(+Field,+Grid,-Neighbour)
%numbToFields(+Numb,+NumbFields,+Grid,-NewGrid)
%rangeInNumbs(+Element,+Numbs,-Numb)
%row(+X,+Y,-Row)
%sureshot(+Numbs,-SureNumbs,+Grid,-SureGrid, +Marker)
%union(+Liste, +Liste, - Vereinigung)
%weightedToT(List,-List)
%weightFields(+Numbers,+Grid,-Weightedgrid)
%whiteNeighbour(+Element,Grid)
%whiteTs(+Grid,+Numbs,-NewGrid)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%whiteNeighbour(+Element,Grid)
%searches for white fields in the neighbourhood
blackNeighbour(p(f(X,Y),T),Grid) :-bagof(Neighbour,neighbour(p(f(X,Y),T),Grid,Neighbour),Neighbours), 
	bagof(p(f(X1,Y1),b),member(p(f(X1,Y1),b),Neighbours),Blacks), length(Blacks,B), length(Neighbours,N), N==B.

%blackOrWhite(X,Y,Blacks,Back, Bow)
blackOrWhite(X,Y,Grid,NewGrid,Bow):- delete(Grid, p(f(X,Y),_),Grid1),append([p(f(X,Y),Bow)], Grid1, NewGrid).

%blackSureFields(List,+Grid,-Liste)
%blacks Ts
blackSureFields([],Grid,Grid).
blackSureFields(List,Grid,Grid):- not(bagof(p(f(X,Y),t),(member(p(f(X,Y),t),List),blackNeighbour(p(f(X,Y),t),Grid)),_)).
blackSureFields(List,Grid,NewGrid):- bagof(p(f(X,Y),t),(member(p(f(X,Y),t),List),blackNeighbour(p(f(X,Y),t),Grid)),Blacks),
	listToElementBow(Blacks,Grid,b,NewGrid).

%countWoB(+RangedNumbs,+Color,-Count)
countWoB([],_,0).
countWoB([p(f(_,_),Numb)|RangedNumbs],Color,Count):- Color==Numb, countWoB(RangedNumbs,Color,NewCount), Count is NewCount+1. 
countWoB([p(f(_,_),Numb)|RangedNumbs],Color,Count):- Color\==Numb, countWoB(RangedNumbs,Color,Count).


%iBomb(M,N,Numbers, NewNumbers, Grid, NewGrid)
%searches 100% sure fields on the playground and color them
iBomb(_,_,[], [], Grid, Grid):-!.
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
	(X>0, X<M, Y>0,Y<N
	, X1 is X+1, Y1 is Y+1, blackOrWhite(X,Y, Grid, Back1,w), blackOrWhite(X,Y1, Back1, Back2,w),
	blackOrWhite(X1,Y,Back2, Back3,w), blackOrWhite(X1,Y1,Back3,Res,w))
	), iBomb(M,N,Numbers, NewNumbers, Res, NewGrid).
%getting 1
iBomb(M,N,[c(f(X, Y),Count)|Numbers], NewNumbers, Grid, NewGrid) :- Count==1,
	%Alle Ecken 
	((X==Y, X==0, X1 is X+1, Y1 is Y+1, blackOrWhite(X1,Y1, Grid, Res, b))
	;(X==0, Y==N, X1 is X+1, blackOrWhite(X1,Y, Grid, Res, b) )
	;(X==M,Y==0, Y1 is Y+1, blackOrWhite(X,Y1,Grid, Res, b))
	;(X==M,Y==N, blackOrWhite(X,Y, Grid, Res, b))), 
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
	(Count==1,(
	(X>0, X<M, (Y==N;Y==0))
	;((X==0;X==M),Y>0,Y<N)
	;(X>0, X<M,Y>0,Y<N));
	(Count==2,(X>0, X<M, Y>0, Y<N));
	(Count==3)), iBomb(M,N,Numbers, NewNumbers, Grid, NewGrid).

%insertAtEnd(+Element,+List,-NewList)
insertAtEnd(X,[ ],[X]).
insertAtEnd(X,[H|T],[H|Z]) :- insertAtEnd(X,T,Z). 

%listToElementBoW(+Numbers,+Grid,+Color,-NewGrid)
%Makes out of a list single elements which can be transformed by blackOrWhite()
listToElementBow([],Grid,_,Grid).
listToElementBow([p(f(X, Y),Count)|Numbers],Grid,Color,NewGrid):- Color==b, Count\==w, blackOrWhite(X,Y,Grid,Grid1,Color), 
	listToElementBow(Numbers,Grid1,Color,NewGrid).
listToElementBow([p(f(X, Y),Count)|Numbers],Grid,Color,NewGrid):- Color==w, Count\==b, blackOrWhite(X,Y,Grid,Grid1,Color), 
	listToElementBow(Numbers,Grid1,Color,NewGrid).
listToElementBow([p(f(_, _),Count)|Numbers],Grid,Color,NewGrid):- Color==b, Count==w, 
	listToElementBow(Numbers,Grid,Color,NewGrid).
listToElementBow([p(f(_, _),Count)|Numbers],Grid,Color,NewGrid):- Color==w, Count==b, 
	listToElementBow(Numbers,Grid,Color,NewGrid).
listToElementBow([p(f(_, _),Count)|Numbers],Grid,Color,NewGrid):- Count==Color, Color\==b, Color\==w,
	listToElementBow(Numbers,Grid,Color,NewGrid).

%max(+Numb1,+Numb2,-Res)
%returns the max value
max(I,J,R):-((I>J,R=I);(J>I,R=J);(I==J,R=J)).

%maxInList(+Grid, -Maximum)
%returns the maximum of the list
maxInList([],0).
maxInList([p(f(_,_),w(X,Y))|Grid],Max):- maxInList(Grid,Down), T is X+Y, max(Down,T,Max).

%myGrid(+X,+Y, -Grid)
%Macht ein volles Grid, ohne Abstufungen
myGrid(X,Y, []) :- ((X==Y, X=<0);(X=<0); (Y=<0)).
myGrid(X,Y, [p(f(X,Y),t)|Grid]) :- X>0, Y>0, X1 is X-1, Y1 is Y-1,
	myGrid(X1,Y, Grid1), row(X,Y1,Grid2), union(Grid1, Grid2, Grid).

%neighbour(+Field,+Grid,-Neighbour)
%Find all neighbour Fields
neighbour(p(f(X,Y),_),Grid,p(f(RX,RY),T)) :-X1 is X-1, X2 is X+1, Y1 is Y-1, Y2 is Y+1, 
	((member(p(f(X,Y1),T),Grid), RX=X,RY=Y1)
	;(member(p(f(X,Y2),T),Grid), RX=X,RY=Y2)
	;(member(p(f(X1,Y),T),Grid), RX=X1,RY=Y)
	;(member(p(f(X2,Y),T),Grid), RX=X2,RY=Y)).

%numbToFields(+Numb,+NumbFields,+Grid,-NewGrid)
%get a Field and connect it to its weight
numbToFields(_,[],Grid,Grid).
numbToFields(D,[p(f(X,Y),T)|NumbFields],Grid,NewGrid):-T==t, 
	bagof(Neighbour,(neighbour(p(f(X,Y),T),Grid,Neighbour)),Neighbours), countWoB(Neighbours,w,WhiteCount),
	countWoB(Neighbours,b,BlackCount), length(Neighbours, Fields), Res is 4-Fields+BlackCount-WhiteCount,
	blackOrWhite(X,Y,Grid,Grid1, w(D,Res)), numbToFields(D,NumbFields,Grid1,NewGrid).
numbToFields(D,[p(f(X,Y),w(WX,WY))|NumbFields],Grid,NewGrid):- 
	WR is D+WX,blackOrWhite(X,Y,Grid,Grid1, w(WR,WY)), numbToFields(D,NumbFields,Grid1,NewGrid).
numbToFields(D,[p(f(_,_),T)|NumbFields],Grid,NewGrid):-(T==b;T==w),numbToFields(D,NumbFields,Grid,NewGrid).

%rangeInNumbs(+Element,+Numbs, -Numb)
%Is in the range of my Field a Number?
rangeInNumbs(p(f(X,Y),_),Numbs, c(f(X,Y),T)):- member(c(f(X,Y),T),Numbs).
rangeInNumbs(p(f(X,Y),_),Numbs, c(f(A,B),T)):- not(member(c(f(X,Y),_),Numbs)),X1 is X-1, Y1 is Y-1, ((member(c(f(X1,Y),T),Numbs), X1=A, Y=B);
	(member(c(f(X,Y1),T),Numbs), X=A,Y1=B);(member(c(f(X1,Y1),_),Numbs),X1=A,Y1=B)).

%row(+X,+Y,-Row)
row(X,Y,[]) :- ((X==Y, X=<0);(X=<0); (Y=<0)).
row(X,Y,[p(f(X,Y),t)|Grid]) :-Y>0, Y1 is Y-1,row(X,Y1,Grid).

%sureshot(+Numbs,-SureNumbs,+Grid,-SureGrid, +Marker)
%sureshot takes out every Number which has surely no other possibility to be placed
sureshot([],[],Grid,Grid).
sureshot([f(x, x)|Numbers],Numbers,Grid,Grid).
%Sureshots for 1
sureshot([c(f(X, Y),Count)|Numbers],SureNumbs,Grid,SureGrid) :- Count==1,delete(Numbers, f(x,x), DeletedNumbers), insertAtEnd(f(x, x), DeletedNumbers, EndNumbers),
	bagof(p(f(X1,Y1),Numb),(range(X,Y,X1,Y1), member(p(f(X1,Y1),Numb),Grid)),RangedNumbs), countWoB(RangedNumbs,w,WhiteCount),
	((WhiteCount==3, member(p(f(X2,Y2),t),RangedNumbs), blackOrWhite(X2,Y2,Grid,NewGrid,b),sureshot(EndNumbers,SureNumbs,NewGrid,SureGrid));
		(WhiteCount<3, countWoB(RangedNumbs,b,BlackCount),(
			(BlackCount==1, listToElementBow(RangedNumbs,Grid,w,NewGrid),sureshot(EndNumbers,SureNumbs,NewGrid,SureGrid));
			(BlackCount==0, Grid=NewGrid),insertAtEnd(c(f(X, Y),Count), Numbers, EndNumbs),sureshot(EndNumbs,SureNumbs,NewGrid,SureGrid)
		)
	)).
%Sureshots for 2
sureshot([c(f(X, Y),Count)|Numbers],SureNumbs,Grid,SureGrid) :- Count==2,delete(Numbers, f(x,x), DeletedNumbers), insertAtEnd(f(x, x), DeletedNumbers, EndNumbers),
	bagof(p(f(X1,Y1),Numb),(range(X,Y,X1,Y1), member(p(f(X1,Y1),Numb),Grid)),RangedNumbs), countWoB(RangedNumbs,w,WhiteCount),
	((WhiteCount==2, listToElementBow(RangedNumbs,Grid,b,NewGrid),sureshot(EndNumbers,SureNumbs,NewGrid,SureGrid));
		(WhiteCount<2, countWoB(RangedNumbs,b,BlackCount),(
			(BlackCount==2, listToElementBow(RangedNumbs,Grid,w,NewGrid),sureshot(EndNumbers,SureNumbs,NewGrid,SureGrid));
			(BlackCount<2, Grid=NewGrid),insertAtEnd(c(f(X, Y),Count), Numbers, EndNumbs),sureshot(EndNumbs,SureNumbs,NewGrid,SureGrid)
		)
	)).
%Sureshots for 3
sureshot([c(f(X, Y),Count)|Numbers],SureNumbs,Grid,SureGrid) :- Count==3,delete(Numbers, f(x,x), DeletedNumbers), insertAtEnd(f(x, x), DeletedNumbers, EndNumbers),
	bagof(p(f(X1,Y1),Numb),(range(X,Y,X1,Y1), member(p(f(X1,Y1),Numb),Grid)),RangedNumbs), countWoB(RangedNumbs,w,WhiteCount),
	((WhiteCount==1, listToElementBow(RangedNumbs,Grid,b,NewGrid),sureshot(EndNumbers,SureNumbs,NewGrid,SureGrid));
		(WhiteCount==0, countWoB(RangedNumbs,b,BlackCount),(
			(BlackCount==3,  member(p(f(X2,Y2),t),RangedNumbs), blackOrWhite(X2,Y2,Grid,NewGrid,w)),sureshot(EndNumbers,SureNumbs,NewGrid,SureGrid);
			(BlackCount<3, Grid=NewGrid),insertAtEnd(c(f(X, Y),Count), Numbers, EndNumbs),sureshot(EndNumbs,SureNumbs,NewGrid,SureGrid)
		)
	)).

%union(+Liste, +Liste, - Vereinigung) 
union([A|B], C, D) :- member(A,C), !, union(B,C,D).
union([A|B], C, [A|D]) :- union(B,C,D).
union([],Z,Z).

%weightedToT(List,-List)
%change the weighted Fields back to Ts
weightedToT([],[]).
weightedToT([p(f(X,Y),T)|List],[p(f(X,Y),T)|NewList]) :- atom(T),weightedToT(List,NewList).
weightedToT([p(f(X,Y),w(_,_))|List],[p(f(X,Y),t)|NewList]):-weightedToT(List,NewList).

%weightFields(+Numbers,+Grid,-Weightedgrid)
%All fields which belongs to a Number get a weight
weightFields([],Grid,Grid).
weightFields([c(f(X,Y),D)|Numbers],Grid,WeightedGrid):-bagof(p(f(X1,Y1),T),(range(X,Y,X1,Y1), member(p(f(X1,Y1),T),Grid)),NumbFields),
	numbToFields(D,NumbFields,Grid,NewGrid), weightFields(Numbers,NewGrid,WeightedGrid).

%whiteNeighbour(+Element,Grid)
%searches for white fields in the neighbourhood
whiteNeighbour(p(f(X,Y),_),Grid) :-X1 is X-1, X2 is X+1, Y1 is Y-1, Y2 is Y+1, 
	(member(p(f(X,Y1),w),Grid)
	;member(p(f(X,Y2),w),Grid)
	;member(p(f(X1,Y),w),Grid)
	;member(p(f(X2,Y),w),Grid)).

%whiteTs(+Grid,+Numbs,-NewGrid)
%White all Ts which have whites as a neighbour
whiteTs(Grid,_,Grid):-not(member(p(f(_,_),t),Grid)).
whiteTs(Grid,Numbs,NewGrid):-bagof(p(f(X,Y),t), (member(p(f(X,Y),t),Grid), not(rangeInNumbs(p(f(X,Y),t),Numbs,_)), whiteNeighbour(p(f(X,Y),t),Grid)),WhiteMyTs), 
	length(WhiteMyTs, I), ((I>0,listToElementBow(WhiteMyTs, Grid, w, Grid1), whiteTs(Grid1,Numbs,NewGrid))
	;(I==0, NewGrid=Grid)).
whiteTs(Grid,Numbs,Grid):-not(bagof(p(f(X,Y),t), (member(p(f(X,Y),t),Grid), not(rangeInNumbs(p(f(X,Y),t),Numbs,_)), whiteNeighbour(p(f(X,Y),t),Grid)),_)).

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
