:- style_check(-discontiguous).

convertBinToDec(Bin, Dec) :-
    convertBinToDec(Bin, Dec, 0).
convertBinToDec(0, 0, _).
convertBinToDec(Bin, Dec, C) :-
    Bin \= 0,
    Last is Bin mod 2,
    pow(2, C, R),
    Res is R * Last,
    Rest is div(Bin, 10),
    C1 is C + 1,
    convertBinToDec(Rest, Dec1, C1),
    Dec is Res + Dec1.



replaceIthItem(E, [_|T], 0, [E|T]).
replaceIthItem(_, [], _, []).
replaceIthItem(E, [H|T], I, R) :-
    I > 0,
    I1 is I - 1,
    replaceIthItem(E, T, I1, R1),
    R = [H|R1].



splitEvery(_, [], [[]]).
splitEvery(N, [H|T], Res) :-
    N > 0,
    splitEvery(N, T, [H1|T1]),
    length(H1, Len),
    Len < N,
    NewH1 = [H|H1],
    Res = [NewH1|T1].
splitEvery(N, [H|T], Res) :-
    N > 0,
    splitEvery(N, T, [H1|T1]),
    length(H1, Len),
    Len >= N,
    NewList = [H],
    Res = [NewList|[H1|T1]].
splitEvery(N, List, List) :-
    N =< 0.



fillZeros(String, N, R) :-
    addZeros(N, Zeros),
    string_concat(Zeros, String, R).
addZeros(0, "").
addZeros(N, R) :-
    N > 0,
    N1 is N - 1,
    addZeros(N1, R1),
    string_concat("0", R1, R).



logBase2(1, 0).
logBase2(Num, Res) :-
    Num > 1,
    divRoundUp(Num, 2,Num1),
    logBase2(Num1, Res1),
    Res is Res1 + 1.





divRoundUp(X, Y, R) :-
    Rem is X mod Y,
    Rem \= 0,
    Div is div(X, Y),
    R is Div + 1.

divRoundUp(X, Y, R) :-
    Rem is X mod Y,
    Rem = 0,
    R is div(X, Y).

getNumBits(_, fullyAssoc, _, 0).
getNumBits(Sets, setAssoc, Cache, BitsNum) :-
	logBase2(Sets, BitsNum).
getNumBits(_, directMap, Cache, BitsNum) :-
    length(Cache, L),
    logBase2(L, BitsNum).
	



getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
	atom_number(StringAddress,BinAddresss),
	convertAddress(BinAddresss,BitsNum,Tag,Idx,directMap),
	convertBinToDec(Idx,DecIdx),
	convertBinToDec(Tag,DecTag),
	getDataFromCache2(DecIdx,DecTag,Cache,Data,HopsNum).

getDataFromCache2(0,DecTag,[item(tag(Tag),data(Data),1,Order)|_],Data1,HopsNum):-
	
	Data1=Data,
	atom_number(Tag,BinTag),
	convertBinToDec(BinTag,DeTag),
	DecTag=DeTag,
	HopsNum=0.
	
getDataFromCache2(0,DecTag,[item(tag(Tag),data(Data),0,Order)|_],Data1,HopsNum):-

	false.
		


getDataFromCache2(DecIdx,DecTag,[_|T],Data,HopsNum):-
		DecIdx>0,
		DecIdx1 is DecIdx-1,
		getDataFromCache2(DecIdx1,DecTag,T,Data,HopsNum).
			
	
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
	convertAddress(Bin,BitsNum,Tag1,Idx1,directMap,0,"",""),
	
	((atom_number(Tag1,Tag),Tag1\="");(Tag=0,Tag1="")),
	
	atom_number(Idx1,Idx).

	
	
	
convertAddress(0,BitsNum,Tag,Idx,directMap,Acc,IdxAcc,TagAcc):-
	Acc<BitsNum,
	atom_number(String,0),
	string_concat(String,IdxAcc,IdxAcc1),
	Idx=IdxAcc1,
	Tag=TagAcc.
	
	
	
convertAddress(0,BitsNum,Tag,Idx,directMap,Acc,IdxAcc,TagAcc):-
	Acc>=BitsNum,
	atom_number(String,0),
	string_concat(String,TagAcc,TagAcc1),
	Idx=IdxAcc,
	Tag=TagAcc1.


convertAddress(1,BitsNum,Tag,Idx,directMap,Acc,IdxAcc,TagAcc):-
	Acc<BitsNum,
	atom_number(String,1),
	string_concat(String,IdxAcc,IdxAcc1),
	Idx=IdxAcc1,
	Tag=TagAcc.
	
	
	
convertAddress(1,BitsNum,Tag,Idx,directMap,Acc,IdxAcc,TagAcc):-
	Acc>=BitsNum,
	atom_number(String,1),
	string_concat(String,TagAcc,TagAcc1),
	Idx=IdxAcc,
	Tag=TagAcc1.




convertAddress(Bin,BitsNum,Tag,Idx,directMap,Acc,IdxAcc,TagAcc):-
	Acc<BitsNum,LastDigit is Bin mod 10,
	Bin \=0,
	Bin\=1,
	Bin1 is div(Bin,10),
	atom_number(String,LastDigit),
	string_concat(String,IdxAcc,IdxAcc1),
	Acc1 is Acc+1,
	convertAddress(Bin1,BitsNum,Tag,Idx,directMap,Acc1,IdxAcc1,TagAcc).
	
convertAddress(Bin,BitsNum,Tag,Idx,directMap,Acc,IdxAcc,TagAcc):-
	Acc>=BitsNum,LastDigit is Bin mod 10,
	Bin \=0,
	Bin\=1,
	Bin1 is div(Bin,10),
	
	atom_number(String,LastDigit),
	string_concat(String,TagAcc,TagAcc1),
	Acc1 is Acc+1,
	convertAddress(Bin1,BitsNum,Tag,Idx,directMap,Acc1,IdxAcc,TagAcc1).





getDataFromCache(StringAddress,Cache,Data,HopsNum,fullyAssoc,_):-
	getDataFromCache(StringAddress,Cache,Data,HopsNum,0,fullyAssoc,_).

getDataFromCache(StringAddress,[H|_],Data,HopsNum,A,fullyAssoc,_):-
	H=item(tag(X),data(Y),Z,_),X=StringAddress,Z=1,HopsNum=A,Data=Y.


getDataFromCache(StringAddress,[H|T],Data,HopsNum,A,fullyAssoc,_):-
	H=item(tag(X),data(Y),_,_),X\==StringAddress,A1 is A+1,getDataFromCache(StringAddress,T,Data,HopsNum,A1,fullyAssoc,_).

 convertAddress(Tag,_,Tag,_,fullyAssoc).



getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	atom_number(StringAddress,BinAddresss),
	convertAddress(BinAddresss,SetsNum,Tag,Idx,setAssoc),
	convertBinToDec(Idx,DecIdx),
	convertBinToDec(Tag,DecTag),
	length(Cache,Abc),
	divRoundUp(Abc,SetsNum,X),
	splitEvery(X,Cache,L),
	getSetFromCache(L,L2,DecIdx),
	getDataFromCache1(DecTag,L2,Data,HopsNum,setAssoc,0).
	
	getDataFromCache1(DecTag,[],Data1,HopsNum,setAssoc,Acc):- false.
	
	
	
	getDataFromCache1(DecTag,[item(tag(Tag),data(Data),1,Order)|_],Data1,HopsNum,setAssoc,Acc):-
		atom_number(Tag,BinTag),
		convertBinToDec(BinTag,DeTag),
		DecTag=DeTag,
		Data1 = Data,
		HopsNum = Acc.
		
		
		
		
	getDataFromCache1(DecTag,[item(tag(Tag),data(Data),0,Order)|T],Data1,HopsNum,setAssoc,Acc):-
	Acc1 is Acc +1,
	getDataFromCache1(DecTag,T,Data1,HopsNum,setAssoc,Acc1).	
	
	getDataFromCache1(DecTag,[item(tag(Tag),data(Data),1,Order)|T],Data1,HopsNum,setAssoc,Acc):-
		atom_number(Tag,BinTag),
		convertBinToDec(BinTag,DeTag),
		DecTag\=DeTag,
		Acc1 is Acc +1,
		getDataFromCache1(DecTag,T,Data1,HopsNum,setAssoc,Acc1).
		

	
	
	getSetFromCache([H|_],H,0).
	
	getSetFromCache([_|T],L2,DecIdx):-
	DecIdx\=0,
	DecIdx1 is DecIdx-1,
	getSetFromCache(T,L2,DecIdx1).

	

	
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
   getNumBits(SetsNum,setAssoc,_,BitsNum),
   convertAddress(Bin,BitsNum,Tag1,Idx1,setAssoc,0,"",""),
   ((atom_number(Tag1,Tag),Tag1 \="");(Tag=0,Tag1="")),
	((atom_number(Idx1,Idx),Idx1\="");(Idx=0,Idx1="")).
	
	convertAddress(0,BitsNum,Tag,Idx,setAssoc,Acc,IdxAcc,TagAcc):-
	Acc<BitsNum,
	atom_number(String,0),
	string_concat(String,IdxAcc,IdxAcc1),
	Idx=IdxAcc1,
	Tag=TagAcc.
	
	
	
convertAddress(0,BitsNum,Tag,Idx,setAssoc,Acc,IdxAcc,TagAcc):-
	Acc>=BitsNum,
	atom_number(String,0),
	string_concat(String,TagAcc,TagAcc1),
	Idx=IdxAcc,
	Tag=TagAcc1.


convertAddress(1,BitsNum,Tag,Idx,setAssoc,Acc,IdxAcc,TagAcc):-
	Acc<BitsNum,
	atom_number(String,1),
	string_concat(String,IdxAcc,IdxAcc1),
	Idx=IdxAcc1,
	Tag=TagAcc.
	
	
	
convertAddress(1,BitsNum,Tag,Idx,setAssoc,Acc,IdxAcc,TagAcc):-
	Acc>=BitsNum,
	atom_number(String,1),
	string_concat(String,TagAcc,TagAcc1),
	Idx=IdxAcc,
	Tag=TagAcc1.
	
	
	convertAddress(Bin,BitsNum,Tag,Idx,setAssoc,Acc,IdxAcc,TagAcc):-
	Acc<BitsNum,
	LastDigit is Bin mod 10,
	Bin \=0,
	Bin\=1,
	Bin1 is div(Bin,10),
	atom_number(String,LastDigit),
	string_concat(String,IdxAcc,IdxAcc1),
	Acc1 is Acc+1,
	convertAddress(Bin1,BitsNum,Tag,Idx,setAssoc,Acc1,IdxAcc1,TagAcc).
	
convertAddress(Bin,BitsNum,Tag,Idx,setAssoc,Acc,IdxAcc,TagAcc):-
	Acc>=BitsNum,LastDigit is Bin mod 10,
	Bin \=0,
	Bin\=1,
	Bin1 is div(Bin,10),
	atom_number(String,LastDigit),
	string_concat(String,TagAcc,TagAcc1),
	Acc1 is Acc+1,
	convertAddress(Bin1,BitsNum,Tag,Idx,setAssoc,Acc1,IdxAcc,TagAcc1).
	
	








replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-

	TagBits is 6-BitsNum,
	atom_number(StringIdx,Idx),
	atom_number(StringTag,Tag),
	string_length(StringIdx,IdxLength),
	string_length(StringTag,TagLength),
	IdxZerosNeeded is BitsNum- IdxLength,
	TagZerosNeeded is TagBits- TagLength,
	fillZeros(StringTag,TagZerosNeeded,FinalTag),
	fillZeros(StringIdx,IdxZerosNeeded,FinalIdx),
	
	string_concat(FinalTag,FinalIdx,FinalFantasy),
	
	atom_number(FinalFantasy,BinFinal),
	convertBinToDec(BinFinal,DecFinal),
	atom_number(FinalIdx,BinIdx),
	convertBinToDec(BinIdx,DecIdx),
	getDataFromMemory(DecFinal,Mem,ItemData),
	replaceInCache1(FinalTag,DecIdx,OldCache,NewCache,ItemData,directMap).
	
	
	
	
getDataFromMemory(0,[H|_],H).

getDataFromMemory(X,[_|T],Y):-
	X > 0,
	X1 is X-1,
	getDataFromMemory(X1,T,Y).
	
replaceInCache1(FinalTag,0,[item(tag(Tag),data(Data1),_,_)|T],[item(tag(FinalTag),data(ItemData),1,0)|T],ItemData,directMap).

replaceInCache1(FinalTag,X,[H|T],NewCache,ItemData,directMap):-
	X\=0,
	X1 is X-1,
	replaceInCache1(FinalTag,X1,T,NewCache1,ItemData,directMap),
	NewCache=[H|NewCache1].
	



replaceInCache(Tag, _, Mem, OldCache, NewCache, ItemData, fullyAssoc, _) :-
	convertBinToDec(Tag, TagDec),
	getDataFromMemory(TagDec, Mem, ItemData),
	OldCache = [item(tag(SampleTag), _, _, _)|_],
	atom_number(StringTempTag, Tag),
	string_length(SampleTag, TagLength),
	string_length(StringTempTag, TempTagLength),
	AdditionalBits is 6 - TempTagLength,
	fillZeros(StringTempTag, AdditionalBits, StringTag),
	incrementOrder(OldCache, OldCache1),
	findFIFO(OldCache1, Pos),
	NewItem = item(tag(StringTag), data(ItemData), 1, 0),
	replaceIthItem(NewItem, OldCache1, Pos, NewCache).




incrementOrder([], []).
incrementOrder([item(Tag, Data, 1, Order)|T], R) :-
	Order1 is Order + 1,
	incrementOrder(T, R1),
	R = [item(Tag, Data, 1, Order1)|R1].
incrementOrder([item(Tag, Data, 0, Order)|T], R) :-
	incrementOrder(T, R1),
	R = [item(Tag, Data, 0, Order)|R1].



findFIFO(L, Pos) :-
	findZero(L, Pos, 0).
findFIFO(L, Pos) :-
	\+ findZero(L, Pos, 0),
	findMaxOne(L, Pos, 0, _).

findZero([item(_, _, 0, _)|_], Count, Count).
findZero([item(_, _, 1, _)|T], Pos, Count) :-
	Count1 is Count + 1,
	findZero(T, Pos, Count1).

findMaxOne([item(_, _, 1, Order)|[]], Count, Count, Order).
findMaxOne([item(_, _, 1, Order)|T], Pos, Count, Max) :-
	Count1 is Count + 1,
	findMaxOne(T, Pos1, Count1, Max1),
	Order > Max1,
	Max = Order,
	Pos = Count.
findMaxOne([item(_, _, 1, Order)|T], Pos, Count, Max) :-
	Count1 is Count + 1,
	findMaxOne(T, Pos1, Count1, Max1),
	Order =< Max1,
	Max = Max1,
	Pos = Pos1.




replaceInCache(Tag, Idx, Mem, OldCache, NewCache, ItemData, setAssoc, SetsNum) :-
	getNumBits(SetsNum, setAssoc, OldCache, BitsNum),
	atom_number(StringIdx, Idx),
	string_length(StringIdx, IdxLength),
	IdxZerosNeeded is IdxLength - BitsNum,
	fillZeros(StringIdx, IdxZerosNeeded, FinalIdx),

	atom_number(StringTempTag, Tag),
	string_concat(StringTempTag, FinalIdx, AddressString),
	atom_number(AddressString, Address),
	convertBinToDec(Address, AddressDec),
	getDataFromMemory(AddressDec, Mem, ItemData),
	OldCache = [item(tag(SampleTag), _, _, _)|_],

	string_length(SampleTag, TagLength),
	string_length(StringTempTag, TempTagLength),
	AdditionalBits is 6 - BitsNum - TempTagLength,
	fillZeros(StringTempTag, AdditionalBits, StringTag),
	length(OldCache, CacheSize),
	SplitBy is ceiling(CacheSize / SetsNum),
	splitEvery(SplitBy, OldCache, OldCacheSplitted),
	getDataFromMemory(Idx, OldCacheSplitted, TargetSet),
	findFIFO(TargetSet, Pos),
	NewItem = item(tag(StringTag), data(ItemData), 1, -1),
	replaceIthItem(NewItem, TargetSet, Pos, TargetSet1),
	incrementOrder(TargetSet1, TargetSet2),
	replaceIthItem(TargetSet2, OldCacheSplitted, Idx, NewCacheSplitted),
	unsplit(NewCacheSplitted, NewCache).


unsplit([], []).
unsplit([HL|T], R) :-
	is_list(HL),
	unsplit(HL, RH),
	unsplit(T, RT),
	append(RH, RT, R).
unsplit([H|T], R) :-
	\+is_list(H),
	unsplit(T, RT),
	R = [H|RT].

































	
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).

	
    







