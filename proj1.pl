%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% File     : proj2.pl
% Author   : Kian Dsouza
% Login id : KIAND
% Date     : 6th September, 2021.

% Purpose  :
% The program implements a fill-in puzzle solver, which takes a Puzzle and a
% WordList, and solves the Puzzle by creating slots which consist of fill-able 
% and pre-filled sequences from the puzzle and matches words from the WordList 
% with these slots, by finding the best slot that can be filled with the best 
% word to be filled with.

% The fill-in puzzle solver program makes use of Prolog's logical variable 
% feature and creates slots, which are maximal horizontal or vertical sequences
% of fill-able and pre-filled squares. The puzzle is then traversed row-wise  
% and column-wise by taking the puzzle's transpose, and all the identified
% slots are stored in a slot list. The best slot from the list is then chosen
% by checking the word list for a slot with the least matching words, which 
% is the best word, and then the best slot and the best word are unified.
% The best slot and the best word are then deleted from the slot list and word
% respectively, and this process is then repeated recursively until the puzzle 
% is solved, if a solution exists, and true is returned otherwise false 
% will be returned

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% loads the correct transpose/2 predicate, which is used to transpose the 
% puzzle to get the slots in each coloumn.
:- ensure_loaded(library(clpfd)).

% puzzle_solution(+Puzzle, +WordList)
% Puzzle is a list of lists, which contains the structure of the puzzle
% WordList is a list of lists, which contains words to be filled in the puzzle
% this defines a predicate that holds when the Puzzle is solved,i.e filled, 
% using all the words from the WordList.
puzzle_solution(Puzzle,WordList):-
    puzzle_slots(Puzzle, SlotList),
    puzzle_match(SlotList, WordList).

% puzzle_slots(+Puzzle, -Slots)
% this defines a predicate that holds when Slots contains a list of all the 
% slots of the Puzzle, first by making slots of every row, then by transposing 
% the puzzle and making slots of every column, and then joining the row and 
% column slots together, and finally deleting the invalid slots of length 1
% to get Slots
puzzle_slots(Puzzle, Slots):-
    slots(Puzzle,Slots1),
    transpose(Puzzle,Puzzle1),
    slots(Puzzle1,Slots2),
    append(Slots1,Slots2,Slots3),
    delete_invalid(Slots3, Slots).

% puzzle_match(+SlotList,+WordList)
% this defines a predicate that holds when the slots in the SlotList are 
% matched and filled with words from the WordList.
% it starts by taking the BestSlot from the SlotList and unifies it with the 
% BestWord in the WordList for that BestSlot, then it deletes the BestSlot and 
% BestWord from the SlotList and WordList respectively, it does this till all
% the slots are matched with words in the WordList
puzzle_match([],[]).    
puzzle_match(SlotList,WordList):-
    best_slot(SlotList, WordList,  BestSlot),
    best_word(BestSlot, WordList, BestWord),
    unify(BestSlot, BestWord),
    delete(BestSlot, SlotList, NewSlotList),
    delete(BestWord, WordList, NewWordList),
    puzzle_match(NewSlotList, NewWordList),!.

% slots(+Puzzle,-Slots)
% this defines a predicate that holds when rows of the Puzzle are used to make 
% up the different Slots of the Puzzle.
% this predicate calls the get_slots/2 predicate for every Row and gets the 
% RowSlots which are joined together to get the Slots of the Puzzle.
slots([],[]).
slots([Row|Rows],Slots):-
    get_slots(Row,RowSlots),
    append(RowSlots,Rest,Slots),
    slots(Rows,Rest).

% get_slots(+Row, -RowSlots)
% this defines a predicate which holds when RowSlots is a list of slots of Row,
% and it uses its accumulator pair get_row_slots/3 predicate to 
% generate this list.
get_slots(Row, RowSlots):-
    get_row_slots(Row,[],RowSlots).

% get_row_slots(+Row,+CurSlot,-RowSlots)
% this defines a predicate that holds when RowSlots is a list of slots of Row,
% CurSlot is an accumulator.
% it checks all the elements of the row, if the element is not a '#' it gets 
% added to the accumulator CurSlot,if it is a '#', CurSlot gets added
% to RowSlots, and if the end of the row is reached without '#', CurSlot is
% added to RowSlots as a list.
get_row_slots([],[],[]).
get_row_slots([],CurSlot,RowSlots):-
    CurSlot \= [],
    RowSlots = [CurSlot].
get_row_slots([Head|Tail],CurSlot,RowSlots):-
    ( Head == '#'
    -> (CurSlot \= []
        -> RowSlots = [CurSlot | OtherRowSlots],
           get_row_slots(Tail,[],OtherRowSlots)
       ;get_row_slots(Tail,[],RowSlots))
    ; append(CurSlot, [Head], NewCurSlot),
      get_row_slots(Tail,NewCurSlot,RowSlots)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%% SEARCHING PROCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% best_slot(+SlotList,+WordList,-BestSlot)
% this defines a predicate that holds when BestSlot is a slot with the fewest
% matching words in the WordList making it the best slot to be filled in.
% it does this by calling the slot_match_count/3 predicate and then its
% accumulator pair best_slot/5, using the number of word matches obtained in
% the WordList,i.e Count of the Slot. 
best_slot([Slot|SlotList],WordList,BestSlot):-
    slot_match_count(Slot,WordList,Count),
    best_slot(SlotList,WordList,Count,Slot,BestSlot).

% best_slot(+SlotList,+WordList,+SlotCount,+Slot,-BestSlot)
% this defines a predicate that holds when BestSlot is a slot with the fewest 
% matching words in the WordList.
% it compares the SlotCount of the Slot, with other Slots in the SlotList, and 
% returns BestSlot with the least count, if the count of the current slot is 
% more than that of Slot, the next Slot in the WordList is checked until the 
% the end of the SlotList is reached.
best_slot([],_,_,Slot,BestSlot):-
    BestSlot = Slot.
best_slot([CurSlot|SlotList],WordList,SlotCount,Slot,BestSlot):-
    slot_match_count(CurSlot,WordList,Count),
    ( Count > SlotCount
    -> best_slot(SlotList,WordList,SlotCount,Slot,BestSlot)
    ; best_slot(SlotList,WordList, Count, CurSlot, BestSlot)
    ).

% best_word(+Slot,+WordList,-BestWord)
% this predicate holds when it finds the BestWord in the WordList for a Slot.
% it calls slot_matches/3 to find the Result of the matching words of a slot
% and creates a choice point by passing one of the BestWord in Result if 
% there exist many, using the member/2 predicate.
best_word(Slot,WordList,BestWord):-
    slot_matches(Slot, WordList, Result),
    member(BestWord, Result).

% slot_matches(+Slot,+WordList,-Result)
% this predicate holds when Result contains all the matches of Slot in 
% the WordList
slot_matches(Slot,WordList,Result):-
    slot_matches(Slot,WordList,[],Result).

% slot_matches(+Slot,+WordList,+WordMatch,-Result)
% this defines a predicate that holds when Result is list of matching words 
% of a Slot in a WordList, with WordMatch being used as an accumulator.
% it iterates through Word in the WordList and checks if that word could be 
% matched with the slot, if it could, it gets added to the WordMatch list.
slot_matches(_,[],Result,Result).
slot_matches(Slot,[Word|WordList],WordMatch,Result):-
    ( match(Slot,Word)
    -> append(WordMatch,[Word],WordMatch1),
    slot_matches(Slot, WordList,WordMatch1,Result)
    ; slot_matches(Slot,WordList,WordMatch,Result)
    ).

% slot_match_count(+Slot, +WordList, -Count)
% this predicate holds when Count is the number of words a Slot matches with 
% in the WordList, it calls the slot_matches/3 predicate to get the Result of 
% word macthes for the slot, and then stores the length of Result in Count
slot_match_count(Slot, WordList, Count):-
    slot_matches(Slot, WordList,Result),
    length(Result, Count).

% match(+Slot,+Word)
% this predicate holds when every of element of slot is the same as the 
% corresponding element of Word if it is a nonvar, and if it is a var, it just 
% checks the next character
match([],[]).
match([El|Slot],[Letter|Word]):-
    ( var(El)
    -> match(Slot,Word)
    ; nonvar(El)
    -> El == Letter,
       match(Slot, Word)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% delete(+El,+List,-NewList)
% this predicate holds when NewList is a list that does not contain El from
% the List.
delete(_,[],[]).
delete(El,[El|Tail],Tail).
delete(El,[Head|Tail],[Head|Rest]):-
    Head \== El,
    delete(El,Tail,Rest).

% delete_invaalid(+List, -NewList)
% this predicate holds when NewList is a list of lists that does not contain 
% lists of length 1 from the original List.
delete_invalid([],[]).
delete_invalid([Head|Tail],[Head|Rest]):-
    length(Head,L),L>1,
    delete_invalid(Tail,Rest).
delete_invalid([Head|Tail],Rest):-
    length(Head,L),L == 1,
    delete_invalid(Tail,Rest).

% unify(+Slot,+Word)
% this predicate holds when it unifies a slot with a word
unify([],[]).
unify(Slot,Word):-
    Slot = Word.





