ta_slot_assignment([],[],_).
ta_slot_assignment([ta(Name,N)|T],[ta(Name,N1)|T],Name):- 
	N1 is N-1.
ta_slot_assignment([ta(X,N)|T],[ta(X,N)|T1],Name):-
	X\=Name,
	ta_slot_assignment(T,T1,Name).


perm([H|T],L) :- perm(T,P), insert(H,P,L).
perm([],[]).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).

slot_assignment(0,TAs,RemTAs,[]):-
	RemTAs = TAs.

slot_assignment(LabsNum,TAs,A,Assignment):-
	perm(TAs,L),
	L = [ta(S,N)|T1],
	N>0,
	ta_slot_assignment([ta(S,N)],RemTAs,S),
	RemTAs=[H|V],
	A=[H|T2],
	Assignment = [S|T],
	LabsNum1 is LabsNum -1, 
	slot_assignment(LabsNum1,T1,T2,T).	


max_slots_per_day(DaySched, Max):-
     max_slots_per_day_h(DaySched,DaySched, Max).
max_slots_per_day_h([],_, _).
max_slots_per_day_h(R,DaySched, Max) :-
	 R=[H|T],
	 ta_from_tas(H,DaySched,Max),
	 max_slots_per_day_h(T, DaySched, Max).
ta_from_tas([],_,_):-!.
ta_from_tas(TAs,DaySched,Max):-
	 TAs =[H|T],
     count_ta_slots(DaySched, H, Count),
     Count =< Max,
	 ta_from_tas(T,DaySched,Max).
count_ta_slots(DaySched, TA, Count) :-
    count_ta_slots(DaySched, TA, 0, Count).
count_ta_slots([], _, Count,Count):- !.
count_ta_slots([H|T], TA, C, Count) :-
    member(TA, H),
	C1 is C + 1,
    count_ta_slots(T, TA, C1, Count).
count_ta_slots([H|T], TA, C, Count) :-
    \+member(TA, H),
    count_ta_slots(T, TA, C, Count).
	

day_schedule([],TAs,RemTAs,[]):-
	RemTAs=TAs.
day_schedule(DaySlots,TAs,RemTAs,Assignment1):-
	DaySlots=[H|T],
	slot_assignment(H,TAs,A,Assignment),
	Assignment1=[Assignment|T2],
	day_schedule(T,A,RemTAs,T2).
	
	
week_schedule([],TAs,DayMax,[]).
week_schedule([H|T],TAs,DayMax,[Head|Tail]):-
	day_schedule(H,TAs,RemTAs,Head),
	max_slots_per_day(Head,DayMax),
	week_schedule(T,TAs,DayMax,Tail).
	