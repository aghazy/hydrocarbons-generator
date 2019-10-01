insert_Last(X,[],[X]).
insert_Last(X,[A|Y],R):-insert_Last(X,Y,L1),R=[A|L1].
%==================================
straight_chain_alkane(1,[carb(h, h, h, h)]).
straight_chain_alkane(N,A):-
N>1,helper1(N,[],R),A=[carb(h, h, h, c)|R].
helper1(2,A,R):-insert_Last(carb(c, h, h, h),A,R).
helper1(N,A,R):-N>2,insert_Last(carb(c, h, h, c),A,K),T is N-1,helper1(T,K,Z),R=Z.
%--------------------------
branch_name(S,N):- HS is S*2+1,
atomic_list_concat([c,S,h,HS],N).
%-------------------------------------------
seq(X,Y,X) :- X=<Y.
seq(X,Y,Z) :- X<Y,
X1 is X + 1,
seq(X1,Y,Z).
%-----------------------------
branched_alkane(N,BA):-
N>3,N1 is N-1 ,helper3(N1,[],K,1,T),BA=[carb(h, h, h, c)|K].
helper3(1,A,R,_,X):- \+ var(X) ,insert_Last(carb(c, h, h, h),A,R).
helper3(N,A,R,L,X):-
N>1,M is N//2,seq(1,M,NU),seq(1,M,ND),ND>=NU,ND=<L,NU=<L,branch_name(NU,CU),branch_name(ND,CD)
,insert_Last(carb(c, h, h, c),A,K),insert_Last(carb(c,CU, h, c),A,K2),insert_Last(carb(c,CU,CD, c),A,K3)
,((T=K,V is N-1,NU = 1,ND=1);(T=K2,V is N-(NU+1),NU=ND,X=1) ; (T=K3 ,Z is NU+ND,V is N-(Z+1)),X=1),L2 is L+1 ,helper3(V,T,R1,L2,X),
length(R1,O),can(R1,O),R=R1.
%---------------------------------------------------------
can([H|[]],S).
can([carb(c,CU,CD, c)|T],S):-
(CU=h,CD=h,can(T,S));(CD=h,seq(1,S,N),branch_name(N,R),R=CU,length(T,LL),N=<LL,can(T,S));
 (seq(1,S,N),branch_name(N,R),R=CD,length(T,LL),N=<LL,can(T,S)).
%---------------------------------------------------------
isomers(N,I):-  N>=4,
              findall(BA,branched_alkane(N,BA) ; straight_chain_alkane(N,BA),A),
              dup(A,I).
%---------------------------------------------------------
dup(L1,L2):- L1 = [] , L2 = [].
dup(L1,L2):- L1 = [H|T], delete(T,H,NT) , H = [H3|T3] , reverse(T3,NewT) , NewT = [Head|Tail] , Li = [H3|Tail], append(Li,[Head],NewList),
                 delete(NT , NewList, NTTT) , dup(NTTT,NTT) , L2 = [H|NTT].



