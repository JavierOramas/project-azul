% Dict Find
% searches for a key:value in a dictionary
find_dict(Key, Value, Dict) :-
    member(Key:Value, Dict).

