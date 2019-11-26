len([],0).
len([_|T], Nnew):-len(T, N), Nnew is N+1.

append([], L2, L2).
append([H1|T1], L2, [H1|L]):-append(T1, L2, L).

removeing(_, [], _).
removeing(H, [H|T], T).
removeing(H, [X|T], [X|T1]):-removeing(H, T, T1).

isMember([],(H1, H2, H3),[(H1, H2, H3)], (H1, H2, H3)).
isMember([(H1, H2, H3)|T],(H1, H2, H3),[(H1, H2, H3)|T], (H1, H2, H3)).
isMember([(H1, H2, H3)|T],(H4, H5, H6),[(H1, H2, H3)|T1], U):- (H1, H2) \== (H4, H5), H3\==H6, isMember(T, (H4, H5, H6), T1, U).

giveVal(H, C, Size, D):- C is H + 1, C < Size, C >= 1.

preMember([], (D, E, F), (D, E, F)).
preMember([(A, B, C)|_], (A, B, C), (A, B, C)).
preMember([(A, B, C)|T], (D, E, F), (X, Y, Z)):- (A, B) \== (D, E), C \== F, preMember(T, (D, E, F), (X, Y, Z)).

findingChhotu([], Found, Found).
findingChhotu([(H1, H2, V)|T], (X, Y, Help), Found):- (V < Help, V \== -10, findingChhotu(T, (H1, H2, V), Found));
										  			(V >= Help, findingChhotu(T, (X, Y, Help), Found)).

htbsdka([(A, B, C)|_], (H1, H2, C), (A, B, C)):- (A is H1-2, B is H2);
													(A is H1+2, B is H2);
													(A is H1-1, B is H2+1);
													(A is H1-1, B is H2-1);
													(A is H1+1, B is H2+1);
													(A is H1+1, B is H2-1).
% htbsdka([(A, B, C)|T], (D, E, F), (X, Y, Z)):-(A, B) \== (D, E), C \== F, htbsdka(T, (D, E, F), (X, Y, Z)).


helloLink([(X1, Y1, X2, Y2)|T],  T, (X1, Y1), (X2, Y2)).
helloLink([(X1, Y1, X2, Y2)|T],  T, (X2, Y2), (X1, Y1)).
helloLink([(X1, Y1, X2, Y2)|T],  [(X1, Y1, X2, Y2)|T1], (H1, H2), (A, B)):-helloLink(T, T1, (H1, H2), (A, B)).

nbr(Predefined, L, (H1, H2, H3), L2, Links, Size, Left, Stupid):-
												(
												len(L, D),
												(D < Size-1),
												giveVal(H3, C, Size, Stupid),
												(
													% (htbsdka(Predefined, (H1, H2, C), U2), (A, B, C) = U2, U3 = Links);
													(%or case start
														helloLink(Links, U3, (H1, H2), (A, B));
														(
															(%or case start
																(A is H1-2, B is H2);
																(A is H1-1, B is H2+1);
																(A is H1-1, B is H2-1);
																(A is H1+2, B is H2);
																(A is H1+1, B is H2+1);
																(A is H1+1, B is H2-1)
															),%or case end
															U3 = Links
														)
													)
												),%or case end
												(%or case start
													(A, B) \== (0, 0),
													(Size =:= 7, -2=<A, A=<2, -1=<B, B=<1, (abs(A)+abs(B))=<2);
													(Size =:= 19, -4=<A, A=<4, -2=<B, B=<2, (abs(A)+abs(B))=<4);
													(Size =:= 37, -6=<A, A=<6, -3=<B, B=<3, (abs(A)+abs(B))=<6);
													(Size =:= 61, -8=<A, A=<8, -4=<B, B=<4, (abs(A)+abs(B))=<8);
													(Size =:= 91, -10=<A, A=<10, -5=<B, B=<5, (abs(A)+abs(B))=<10)
												),%or case end
												preMember(Predefined, (A, B, C), U2),
												isMember(L, U2, L1, U1),
												len(L1, E),
												(E=\=D),
												LEft is Left-1,
												nbr(Predefined, L1, U1, L2, U3, Size, LEft, Stupid)
												);
												(
												len(L, D),
												L2 = L,
												D is Size-1
												).
												% ;
												% (
												% Stupid =:= -1
												% ).

rikudo(Size, Prefilled, Links, [(0,0,-10)|L]):-(
												(
													([(H1, H2, H3)|T] = Prefilled),
													findingChhotu(T, (H1, H2, H3), Found)
												);
												len(Prefilled, Nnew),
												Nnew < 1,
												writeln(Found),
												Found = (1, 1, 1)
											   ),
												(_, _, C) = Found,
												(
													(
														C =:= 1,
														nbr(Prefilled, [Found], Found, L, Links, Size, Size, 1)
													)
													% ;
													% (
													% 	C \== 1,
													% 	nbr(Prefilled, [Found], Found, L, Links, Size, Size, -1),
													% 	% write(L),
													% 	Size is Size - C + 1,
													% 	nbr(Prefilled, [Found], Found, L, Links, Size, Size, 1)
													% 	% write(L)
													% )
												).
