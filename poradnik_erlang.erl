

%#####################
% Zad 1. 

% a) zwykła rekurencja 

-module(mod).
-compile(export_all).

sum(1) -> 1;
sum(N) -> N + sum(N-1).


% b) rekurencja ogonowa 

-module(mod).
-compile(export_all).

sum(N) -> sum(N,0).

sum(0, Acc) -> Acc;
sum(N, Acc) -> sum(N-1, N + Acc).


% c)  foldl/3 + seq/2

-module(mod).
-compile(export_all).

func(N) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, lists:seq(1,N)).


% d)  foldr/3 + seq/2

-module(mod).
-compile(export_all).

func(N) -> lists:foldr(fun(X, Sum) -> X + Sum end, 0, lists:seq(1,N)).



%#####################
% Zad 2. 

% a) zwykła rekurencja 

-module(mod).
-compile(export_all).

sum(N, M) when N > M -> exit(self(), kill);
sum(M, M) -> M; 
sum(N, M) -> N + sum(N+1, M).


% b) rekurencja ogonowa 

-module(mod).
-compile(export_all).


sum(N, M) when N > M -> exit(self(), kill);
sum(N, M) -> sum(N, M, N).

sum(M, M, Acc) -> Acc;
sum(N, M, Acc) -> sum(N+1, M, Acc + N+1).


% c) seq/3 + foldl/3

-module(mod).
-compile(export_all).

sum(N, M) when N =< M -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, lists:seq(N,M));
sum(_, _) -> exit(self(), kill).


% d) seq/3 + foldr/3

-module(mod).
-compile(export_all).

sum(N, M) when N =< M -> lists:foldr(fun(X, Sum) -> X + Sum end, 0, lists:seq(N,M));
sum(_, _) -> exit(self(), kill).




%#####################
%  Zad 3. 

% a) 

-module(mod).
-compile(export_all).

create(N) -> create(1, N).

create(N, N) -> [N];
create(A, N) -> [A | create(A+1, N)].

% b) 

-module(mod).
-compile(export_all).

create(1) -> [1];
create(N) -> create(N-1) ++ [N].

% c) 

-module(mod).
-compile(export_all).

create(N) -> lists:seq(1,N).


%#####################
%  Zad 4. 

% a) 

-module(mod).
-compile(export_all).

reverse_create(0) -> [];
reverse_create(N) -> [N | reverse_create(N-1)].


% b) 

-module(mod).
-compile(export_all).

reverse_create(1) -> [1];
reverse_create(N) -> [N] ++ reverse_create(N-1).


% c) reverse/1 + seq/2

-module(mod).
-compile(export_all).

reverse_create(N) -> lists:reverse(lists:seq(1,N)).


%#####################
%  Zad 5. 

% a)

-module(mod).
-compile(export_all).

func(N) -> func(1, N).

func(N, N) ->
    io:format("Number:~p~n",[N]);
func(A, N) ->
    io:format("Number:~p~n",[A]),
    func(A+1, N).

% b) 

-module(mod).
-compile(export_all).

func(1) ->
   io:format("Number:~p~n",[1]);
func(N) ->
   func(N-1),
   io:format("Number:~p~n",[N]).

% c)  foreach/2 + seq/2

-module(mod).
-compile(export_all).

func(N) ->
    L = lists:seq(1,N),
    P = fun(X)-> io:format("Number:~p~n",[X]) end,
    lists:foreach(P, L).


%#####################
% Zad 6. 

% a)

-module(mod).
-compile(export_all).

func(N)  when N > 1 -> func(2, N);
func(_) -> {brak}.

func(A, N) when A =< N ->
    io:format("Number:~p~n",[A]),
    func(A+2, N);
func(_, _) ->
    {koniec}.


% b) 

-module(mod).
-compile(export_all).

func(2) ->
   io:format("Number:~p~n",[2]);
func(N) when N rem 2 == 0 ->
   func(N-2),
   io:format("Number:~p~n",[N]);
func(N) when N > 2 ->
   func(N-1);
func(_) ->
    {brak}.


% c)  foreach/2 + seq/2

-module(mod).
-compile(export_all).


func(N) -> 
    L = [X || X <-lists:seq(1,N), X rem 2 == 0],
    P = fun(X)-> io:format("Number:~p~n",[X]) end,
    case length(L) /= 0 of
        true  ->  lists:foreach(P, L);
        false ->  {brak}
    end.
    


%#####################
% Zad 7. 

% a)

-module(mod).
-compile(export_all).

filter([H|T], N) when H =< N ->
   [H | filter(T, N)];
filter(_, _) ->
   [].

% b) 

-module(mod).
-compile(export_all).

filter([H|T], N) when H =< N ->
   [H] ++ filter(T, N);
filter(_, _) ->
   [].

% c) 

-module(mod).
-compile(export_all).

filter(List, N) -> [X || X <- List, X =< N].


%#####################
% Zad 8. 

% a) 

-module(mod).
-compile(export_all).

reverse([H]) -> [H];
reverse([H|T]) -> reverse(T) ++ [H].


% b) foldl/3

-module(mod).
-compile(export_all).

reverse(L) ->
    P = fun(X, Sum) -> [X|Sum] end,
    lists:foldl(P, [], L).


%#####################
% Zad 9.

% a)

-module(mod).
-compile(export_all).

concatenate([]) -> [];
concatenate([H|T]) -> H ++ concatenate(T).


% b) foldl/3

-module(mod).
-compile(export_all).

reverse(L) ->
    P = fun(X, Sum) -> Sum ++ X end,
    lists:foldl(P, [], L).


%#####################
% Zad 10. 

% a) zwykła rekurencja 

-module(mod).
-compile(export_all).

listLength([]) -> 0;
listLength([_|T]) -> 1 + listLength(T).


% b) rekurencja ogonowa 

-module(mod).
-compile(export_all).

listLength(L) -> listLength(L,0).

listLength([], Acc) -> Acc;
listLength([_|T], Acc) -> listLength(T, Acc+1).


% c) Funs + lists:foldl()

-module(mod).
-compile(export_all).

listLength(L) -> 
    lists:foldl(fun(_, Acc) -> 1 + Acc end,0,L).


% d) Funs + lists:foldr()

-module(mod).
-compile(export_all).

listLength(L) -> 
    lists:foldr(fun(_, Acc) -> 1 + Acc end,0,L).


%#####################
% Zad 11. 

% a) rekurencja

-module(mod).
-compile(export_all).

minL([H|T]) -> minL(T, H).

minL([], Min) -> Min;
minL([H|T], Min) when H < Min -> minL(T, H);
minL([_|T], Min) -> minL(T, Min).


% b) Konstruckja case... of.... end.

-module(mod).
-compile(export_all).

minL([H|T]) -> minL(T, H).

minL([], Min) -> Min;
minL([H|T], Min) ->
    case H < Min of 
      true -> minL(T, H);
      false -> minL(T, Min)
    end.


% c) Konstrukcja if.... end.

-module(mod).
-compile(export_all).

minL([H|T]) -> minL(T, H).

minL([], Min) -> Min;
minL([H|T], Min) ->
   if
    H < Min -> minL(T, H);
    true -> minL(T, Min)
   end.


% d) sort/1 + hd/1

-module(mod).
-compile(export_all).

minL(L) ->
   NewList = lists:sort(L),
   hd(NewList).


%#####################
% Zad 12. 

% a) rekurencja 

-module(mod).
-compile(export_all).


func([H|T]) -> func(T, H, H).

func([], Min, Max) -> {Min, Max};
func([H|T], Min, Max) when H < Min -> func(T, H, Max);
func([H|T], Min, Max) when H > Max -> func(T, Min, H);
func([_|T], Min, Max) -> func(T, Min, Max).


% b) Konstrukcja if.... end.

-module(mod).
-compile(export_all).


func([H|T]) -> func(T, H, H).

func([], Min, Max) -> {Min, Max};
func([H|T], Min, Max) ->
    if 
      H < Min -> func(T, H, Max);
      H > Max -> func(T, Min, H);
      true -> func(T, Min, Max)
    end.


% c) sort/1 + hd/1 + tl/1

-module(mod).
-compile(export_all).

func(L) ->
   NewList = lists:sort(L),
   {hd(NewList), hd(lists:reverse(NewList)}.


%#####################
% Zad 13. 

% a) rekurencyjnie 

-module(mod).
-compile(export_all).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

% b) rekurencja ogonowa 

-module(mod).
-compile(export_all).

factorial(N) -> factorial(N, 1).

factorial(0, Acc) -> Acc;
factorial(N, Acc) -> factorial(N-1, N * Acc).


% c)  seq/2 + foldl/3

-module(mod).
-compile(export_all).

factorial(N)  when N > 0 ->
    L = lists:seq(1,N),
    P = fun(X, Acc) -> X * Acc end,
    lists:foldl(P, 1, L);
factorial(0) ->
    1.


%#####################
% Zad 14. 


-module(mod).
-compile(export_all).


func(1) -> [1];
func(N) -> func(N-1) ++ [lists:foldl(fun(X, Sum)-> X+Sum end, 0, lists:seq(1,N))].




%#####################
% Zad 15. 

-module(mod).
-compile(export_all).

func(L) -> [[X, Y, Z] || X <-L, Y <-L, Z <- L, X*X + Y*Y == Z*Z].



%#####################
% Zad 16. 
% Treść: Zbadaj czy elementy danej listy składającej się cyfr tworzą liczbę palindromiczną. 
% Example:  IN: [1,2,3,2,1]  OUT:  TAK 
%           IN: [1,2,3,3,1]  OUT:  NIE


-module(mod).
-compile(export_all).

func(L) -> func(L, lists:reverse(L)).

func([],[]) -> {tak};
func([H1|T1], [H2|T2]) when H1 == H2 -> func(T1, T2);
func(_, _) -> {nie}.


%#####################
% Zad 17. 

% a)

-module(mod).
-compile(export_all).

func([]) -> [];
func([H|T]) when H rem 2 == 0 -> [H | func(T)];
func([_|T]) -> func(T).


% b) 

-module(mod).
-compile(export_all).

func([]) -> [];
func([H|T]) when H rem 2 == 0 -> [H] ++func(T);
func([_|T]) -> func(T).

% c) 

-module(mod).
-compile(export_all).

func(L) -> [X || X <- L, X rem 2 == 0].


%#####################
% Zad 18. 

% a) 

-module(mod).
-compile(export_all).

func([]) -> [];
func([H|T]) when not is_integer(H) -> [H | func(T)];
func([_|T]) -> func(T).


% b)

-module(mod).
-compile(export_all).

func([]) -> [];
func([H|T]) when not is_integer(H) -> [H] ++ func(T);
func([_|T]) -> func(T).


% c) 
-module(mod).
-compile(export_all).

func(L) -> [X || X <- L, not is_integer(X)].




%#####################
% Zad 19. 

-module(mod).
-compile(export_all).

func(L) -> func(L, L).

func([], L) ->
    func(L, L);
func([H|T], L) ->
    io:format("~p ~n",[H]),
    timer:sleep(250),
    func(T, L).


%#####################
% Zad 20. 

% a) sublist/2 + sublist/3 + nth/2 + length/1

-module(mod).
-compile(export_all).

func(L, Index) ->
   NewValue = 2 * lists:nth(Index, L),
   lists:sublist(L, Index-1) ++ [NewValue] ++ lists:sublist(L, Index+1, length(L)).


% b) sublist/2 + nthtail/2 + nth/2 + length/1

-module(mod).
-compile(export_all).

func(L, Index) ->
   NewValue = 2 * lists:nth(Index, L),
   lists:sublist(L, Index-1) ++ [NewValue] ++ lists:nthtail(Index, L).

%#####################
% Zad 21.

% a) 

-module(mod).
-compile(export_all).

func([],[]) -> [];
func([H1|T1], [H2|T2]) -> [max(H1, H2) | func(T1, T2)].

% b) 

-module(mod).
-compile(export_all).

func([],[]) -> [];
func([H1|T1], [H2|T2]) -> 
   case H1 > H2 of 
      true  -> [H1 | func(T1, T2)];
      false -> [H2 | func(T1, T2)]
   end.

% c) 

-module(mod).
-compile(export_all).

func([],[]) -> [];
func([H1|T1], [H2|T2]) -> 
   if 
      H1 > H2  -> [H1 | func(T1, T2)];
      H1 =< H2 -> [H2 | func(T1, T2)]
   end.


%#####################
% Zad 22. 

-module(mod).
-compile(export_all).

sleep(N) ->
   receive
       after N ->
           ok
   end.


%#####################
% Zad 23. 

% a)  rekurencja

-module(mod).
-compile(export_all).

func(N, L) -> func(N, L, [], []).

func(_N, [], Lmin, Lmax) -> {Lmin, Lmax};
func(N, [H|T], Lmin, Lmax) when H < N -> func(N, T, [H|Lmin], Lmax);
func(N, [H|T], Lmin, Lmax) -> func(N, T, Lmin, [H|Lmax]).


% b) konstrukcja case...of....end.


-module(mod).
-compile(export_all).

func(N, L) -> func(N, L, [], []).

func(_N, [], Lmin, Lmax) -> {Lmin, Lmax};
func(N, [H|T], Lmin, Lmax) ->
    case H < N of 
       true  -> func(N, T, [H|Lmin], Lmax);
       false -> func(N, T, Lmin, [H|Lmax]) 
    end.

% c) konstrukcja if ...end.

-module(mod).
-compile(export_all).

func(N, L) -> func(N, L, [], []).

func(_N, [], Lmin, Lmax) -> {Lmin, Lmax};
func(N, [H|T], Lmin, Lmax) ->
    if 
       H < N  -> func(N, T, [H|Lmin], Lmax);
       H >= N -> func(N, T, Lmin, [H|Lmax]) 
    end.


%#####################
% Zad 24. 

% a) wybór pivota jako środkowego elementu 

-module(mod).
-compile(export_all).

qsort([]) -> [];                                    % tablica zero elementowa juz jest posorotwana
qsort([H]) -> [H];                                  % tablica jedno elementowa już jest posortowana
qsort(L)  when length(L) > 1 ->
   PivotIndex = length(L) div 2,                    % wybor elementu który będzie pivotem                      
   PivotElement = lists:nth(PivotIndex, L),         % pbranie wartości pivotu 
   NewList = lists:delete(PivotElement, L),         % usunięcie wartości Pivota z listy 
   {Lmin, Lmax} = func(PivotElement, NewList),      % posegreguj elementy na liscie 
   qsort(Lmin) ++ [PivotElement] ++ qsort(Lmax).    % wywolaj rekurencyjnie sortowanie na dwóch podtablicach.



%----- funkcja pomocnicza : dzieli tablicę na dwie podtablice-------

func(N, L) -> func(N, L, [], []).

func(_N, [], Lmin, Lmax) -> {Lmin, Lmax};
func(N, [H|T], Lmin, Lmax) when H < N -> func(N, T, [H|Lmin], Lmax);
func(N, [H|T], Lmin, Lmax) -> func(N, T, Lmin, [H|Lmax]).


% b) wybór pivota jako pierwszego elementu 

-module(mod).
-compile(export_all).

qsort([]) -> [];                                    % tablica zero elementowa juz jest posorotwana
qsort([H]) -> [H];                                  % tablica jedno elementowa już jest posortowana
qsort([H|T]) ->
   {Lmin, Lmax} = func(H, T),                       % posegreguj elementy na liscie tworzac dwie podtablice
   qsort(Lmin) ++ [H] ++ qsort(Lmax).               % wywolaj rekurencyjnie sortowanie na dwóch podtablicach.



%----- funkcja pomocnicza : dzieli tablicę na dwie podtablice-------

func(N, L) -> func(N, L, [], []).

func(_N, [], Lmin, Lmax) -> {Lmin, Lmax};
func(N, [H|T], Lmin, Lmax) when H < N -> func(N, T, [H|Lmin], Lmax);
func(N, [H|T], Lmin, Lmax) -> func(N, T, Lmin, [H|Lmax]).



%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%------------------------  Tworzenie procesów / Programowanie współbieżne---------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------


%#####################
% Zad 25. 

% a) Szyka wersja - w sam raz na kolosa :) 

-module(mod).
-compile(export_all).


%---- tworzenie listy procesów -----
func(N) -> [spawn(mod, loop, []) || _<-lists:seq(1,N)].


%------ funkcja procesu -----
loop() ->
    io:format("Pid ~p ~n",[self()]).



% b) Standardowa rekurencja 

-module(mod).
-compile(export_all).


%---- tworzenie listy procesów -----
func(0) -> [];
func(N) ->
    [spawn(mod, loop, []) | func(N-1)].


%------ funkcja procesu -----
loop() ->
    io:format("Pid ~p ~n",[self()]).



% c) rekurencja ogonowa 

-module(mod).
-compile(export_all).


%---- tworzenie listy procesów -----
func(N) -> func(N, []).

func(0, L) -> L;
func(N, L) -> func(N-1, L ++ [spawn(mod, loop, [])]).


%------ funkcja procesu -----
loop() ->
    io:format("Pid ~p ~n",[self()]).




%#####################
% Zad 26. 

% a) 

func([]) ->
    {ok};
func([H|T]) ->
    H!hello,
    func(T).

% b) 

func(L) ->
     P = fun(X)-> X!hello end,
     lists:foreach(P, L),
     {ok}.

% c) 

func(L) ->
    [X!hello || X<-L], 
    {ok}.


%#####################
% Zad 27. 

-module(mod).
-compile(export_all).


% --- tworzy bazę + rejestruje nazwę celem łatwego dostępu do niej z innych funkcji 
start() ->
   register(database, spawn(mod, loop, [[]])).


% --- proces bazy danych ----
% L : jest to lista do której będę dodawane/odejmowane elementy 

loop(L) ->
    receive
      {add, Data} ->
          loop(L++[Data]);
      {delete, Data} ->
            loop(lists:delete(Data, L));
      {show} ->
          io:format("~p ~n",[L]), 
          loop(L)
    end.


%------ Obsługa bazy danych ---------

add(Data) -> database!{add, Data}.
delete(Data) -> database!{delete, Data}.
show() -> database!{show}.



%#####################
% Zad 28  . 

-module(mod).
-compile(export_all).



% -----  proces server  -------

loop() ->
    receive
      {req, From, Msg} ->
          From ! {response, self(), Msg}
    end, 
    loop().


% -------  funkcja klienta  --------
% ServerPid :  pid procesu który odapala funkcję loop() - czyli naszego servera
% Msg : treść wiadomości 

client(ServerPid, Msg) ->
    ServerPid ! {req, self(), Msg},
    receive
      {response, ServerPid, Msg} ->
          io:format("Wiadomosc otrzymana z server: ~p ~n",[Msg])
      after 1000 -> {error}
    end.
    

% ------  funkcja tworząca proces servera  ------

start() -> 
    spawn(mod, loop, []).




% Kod testowy (co wpisac do konsoli) 
% Pid = mod:start().  -> <0.215.0>.
% mod:client(Pid, hello).





%#####################
% Zad 29.

-module(mod).
-compile(export_all).

%##################################################
%-------------- funkcja procesu A -----------------
% N : licznik przesłanych wiadomości. Z każdą nową wiadomością jest decrementowany, aż do zera. 
% PidB : 

loopA(0, _PidB) ->
    io:format("proces A koniec ~n"),
    exit(normal);                    % koniec pracy, ta wiadomośc zostanie przesłana do procesu B i on też się zakończy.
loopA(N, PidB) ->
    io:format("A nr ~p ~n",[N]),
    PidB!{msg, self(), N},
    receive 
      {response, PidB}->
             loopA(N-1, PidB)
    end.

% ------------- funkcja tworząca proces A ------------
% Chodzi o to, żeby nie wrzucać funkcji link() do funkcji procesu. 

makeProcessA(N, PidB) ->
     link(PidB),
     loopA(N, PidB) .
%##################################################


%##################################################
%------------- funkcja procesu B -------------------
loopB() ->
    receive
      {msg, From, N} ->
          io:format("B nr ~p ~n",[N]),
          From ! {response, self()},
          loopB();
      {'EXIT', _From, _Reason} ->
          io:format("proces B koniec~n")
    end.

% ------------- funkcja tworząca proces B ------------
% Chodzi o to, żeby nie wrzucać funkcji process_flag(trap_exit, true) do funkcji procesu. 

makeProcessB() ->
     process_flag(trap_exit, true),
     loopB().

%##################################################

%----------- Funkcja startowa ----------------------

start(N) ->
    PidB = spawn(mod, makeProcessB, []),
    spawn(mod, makeProcessA, [N, PidB]).


% Aby odpalić program wpis: mod:start(N) gdzie N >0 , polecam dla małych N.



%#####################
% Zad 30. 
% Treść: Napisz funkcję która odpali N procesów połączone w pierścień. 
% Tak jak na : http://www.erlang.org/course/ex2.gif
% Następnie prześlij dowolną wiadomość pomiędzy nimi M razy. 
% Kiedy przesyłanie wiadomości się zakończy zabij wszystkie procesy które tworzyły pierścień. 


% a) Do każdego procesu przesyłam Pid nasepnego procesu 

% Opis algorytmu: 
%   1. Każdy proces jako parametr posiada: Unikalny numer, ilość razy jaką ma przekazać wiadomość oraz pid sąsiada do któego wysąłć wiadomosć.
%   2. Tworzymy listę N procesów. Na tym etapie jako NextPid wpisz dowolną wartość - nie ma ona znaczenia
%   3. Stwórz nową  listę procesów tak aby pid pierwszego procesu na liście stał się ostatnim. 
%   4. Mając orginalną listę procesów i listę z punkt 3 możemy każdemu procesowi przesłac PID kolejnego procesu w pierścieniu. 
%   5. Rozpocznij przesyłanie wiadomości 



-module(mod).
-compile(export_all).


% ------ funkcja startowa  ---------
% N : ilość procesów w pierścieniu 
% M : ilość wiadomości do przesłania w pierscieniu. 

start(N, M) ->
    L = [spawn(mod, loop, [X, M, 0]) || X <- lists:seq(1,N)],  % utworzenie listy procesów składajacych się na pierścień. 
    [H|T] = L,
    PidNextList = T ++ [H],
    lists:zipwith(fun(X,Y) -> X!{set, Y} end, L, PidNextList), %  ustawienie parametru NextPid dla każdego procesu.
    H!{req, tresc}.                                            %  wysłanie wiadomości do pierwszego procesu w pierścieniu


% ------ funkcja procesu  ---------
%
%   Nr: unikalny numer procesu - w celu rozrównienia ich przy analizie działania programu
%   M : ilość razy jaką proces może przesłac wiadomość. Kiedy M osiąga wartośc 1 przesyła 
%       wiadomość dalej i kończy działanie.
%   NextPid : pid sąsiada do którego będą wysyłane wiadomości. 

loop(Nr,M, NextPid) ->
    receive
      {set, Pid} ->           % ustawia Pid następnego procesu w pierścieniu. 
          loop(Nr, M, Pid);
      {req, Msg} when M > 1-> 
          io:format("Nr procesu: ~p | Nr wiadomosci: ~p  | Tresc wiadomosci: ~p ~n",[Nr, M, Msg]), 
          NextPid ! {req, Msg},
          loop(Nr, M-1, NextPid);
      {req, Msg} when M == 1->
           io:format("Nr procesu: ~p | Nr wiadomosci: ~p  | Tresc wiadomosci: ~p | KONIEC ~n ",[Nr, M, Msg]),
          NextPid ! {req, Msg}, 
          exit(normal)
    end.



% b) 
%   Opis algorytmu:
%
%   1. Do każdego procesu przesyłamn tablicę wszystkich procesów wchodzących w skład pierścienia.
%   2. Pomiędzy procesami przesyłam dodatkową tablicę której pierwszy element będzie zawierał PID następnego elementu w pierścieniu. 


-module(mod).
-compile(export_all).

% ------ funkcja startowa ------
% N : ilość procesów w pierścieniu 
% M : ilość wiadomości do przesłania w pierscieniu. 

start(N, M) ->
    L = [spawn(mod, loop, [X, M]) || X <- lists:seq(1,N)],  % lista wszystkich procesów wchodzących w skład pierścienia.
    [H|T] = L,
    H ! {T, L},
    {ok}.


% ------ funkcja procesu  ---------
%   ProcessNr: unikalny numer procesu - w celu rozrównienia ich przy analizie działania programu
%   M : ilość razy jaką proces może przesłac wiadomość. Kiedy M osiąga wartośc 1 przesyła 
%       wiadomość dalej i kończy działanie.
loop(ProcessNr, M) ->
    receive
      {[H|T], L} when M > 1 ->
            io:format("Nr procesu: ~p | Nr wiadomosci: ~p ~n",[ProcessNr, M]),
            H!{T, L},
            loop(ProcessNr, M-1);
        {[],[H|T]} when M > 1 ->
            io:format("Nr procesu: ~p | Nr wiadomosci: ~p ~n",[ProcessNr, M]),
            H!{T,[H|T]},
            loop(ProcessNr, M-1);
      {[H|T], L} when M == 1 ->
            io:format("Nr procesu: ~p | Nr wiadomosci: ~p  | KONIEC ~n",[ProcessNr, M]),
            H!{T, L},
            exit(normal);
         {[],[_H|_T]} when M == 1 ->
            io:format("Nr procesu: ~p | Nr wiadomosci: ~p  | KONIEC ~n",[ProcessNr, M]),
            exit(normal)
    end.





%#####################
% Zad 31. 


-module(mod).
-compile(export_all).


start() -> 
   L = [spawn(mod, loop, [0]) || _ <- lists:seq(1,10)],
   [H|T] = L,
   PidList = T ++ [self()],
   lists:zipwith(fun(X, Y) -> X ! {set, Y} end, L, PidList),
   H ! {req, groundhogDay},
   receive 
       {req, Msg} ->
           io:format("----- THE END------- | pid: ~p ~n",[self()])
   end.



loop(NextPid) ->
   receive
       {set, Pid} ->
          io:format("self(): ~p | nextPid() :  ~p ~n",[self(), Pid]),
          loop(Pid);
       {req, Msg} ->
          io:format("pid: ~p   wysyla do pidNext:  ~p ~n",[self(), NextPid]), 
          NextPid ! {req, Msg}
   end.




%#####################
% Zad 32.


-module(mod).
-compile(export_all).


%---------------------- Funkcja procesu bocznego (jak by płatka gwiazdy :P )-----------------
loop() ->
    receive
        {req, From, N} ->
            io:format("Pid ~p otrzymal wiadomosc Nr ~p od procesu: ~p ~n",[self(), N, From]),
            From!{response, self(), N},
            loop();
        {stop, From} ->
            io:format("Pid ~p TERMINATE... ~n",[self()]),
            From!{response, self()},
            exit(normal)
    end.


%--------------- Funkcja procesu centralnego  --------------

loopCentral() ->
     receive
       {response, From, N} ->
           io:format("<---> | Pid ~p otrzymal wiadomosc Nr ~p od Pid: ~p ~n",[self(), N, From]),
           loopCentral()
   end.


%-------------------- Funkcja startowa -------------------------------

start(N, M) ->
   L = [spawn(mod, loop, []) || _ <-lists:seq(1,N)],
   PidCentral = spawn(mod, loopCentral, []),
   sendMsg(L, M, PidCentral).


%--------------- Funkcja pomocniczna sendMsg() ------------------------

sendMsg(L, 0, MainPid) ->
    [X!{stop, MainPid} || X <-L],
    {done};
sendMsg(L,Ile, MainPid) ->
   [X!{req, MainPid, Ile} || X <-L], 
   sendMsg(L, Ile -1, MainPid).




%#####################
% Zad 33.

s1() ->
   receive
   {a} ->
      s2();
   {b} ->
      s3()
   end.


s2() ->
   receive
   {c} ->
      s3();
   {d} ->
      s4()
   end.


s3() ->
   receive
   {e} ->
      s1();
   {f} ->
      s2()
   end.


s4() ->
   receive
   {g} ->
      s3()
   end.




%#####################
% Zad 34.


blokuj() ->
    mutex ! {blokuj, self()},   % wysłanie żądania do mutexu 
    receive                     % do momentu aż nie nadejdzie wiadomośc od mutexu proces będzie blokowany w klauzuli receive ... end.
        {mutex, mozna_wchodz} ->
            wchodze
    end.                        % tutaj następuje wejście do współdzielonego zasobu 



%  odblokuj() : jeżeli proces skończył operacje w sekcji krytycznej to powinien zwolnić mutexa. 
%               w tym celu wywołuje metodę odblokuj(). Proces Mutexa może odblokować tylko proces który do blokował! 


odblokuj() ->
   mutex ! {odblokuj, self()}.


%  Mutex będzie implementowany jako maszyna 2 stanowa. 
%  Stany implementowane będą jako funkcje. 
%  Uwaga: na proces może składać się wiele funkcji, ale każdy proces ma tylko jedną skrzynkę odbiorczą!  

wolny() ->
   receive
       {blokuj, Pid} ->
           Pid!{mutex, mozna_wchodz},
           zajety(Pid);
       {stop} ->  % zatrzymanie procesu mutexa
           true  
   end.

zajety(Pid) ->
   receive
      {odblokuj, Pid} ->
         wolny()
   end.



%#####################
% Zad 35.

-module(mod).
-compile(export_all).


%  ----------------- producent w pętli produkuje liczby  ------------------------
% Produkt: będą to kolejny liczby: 0, 1, 2, 3, ...,N. które są wysyłane do bufora 

producerLoop(Produkt) ->
    blokuj(),                          % producent otrzymuje wyłączy dostęp do bufora 
    server ! {is_full, self()},        % sprawdzenie czy bufor jest pełny 
    receive
       {server, yesFull} ->            % bufor jest pełny dlatego zwolnij mutexa i czekaj na kolejne wywołanie
           odblokuj(),                 % zwolnienie dostępu do bufora. 
           producerLoop(Produkt); 
       {server, noFull} ->             % bufor nie jest pełny, można wyprodukować i przesłać dane do bufora
           ok
        after 2000 ->                  % zakoncz proces producenta bo server padł 
          odblokuj(),                  % zwolnienie dostępu do bufora. 
          exit(normal)       
    end,
    server ! {put, self(), Produkt},   % przesyłanie produktu :) 
    receive                            % oczekiwanie na potwierdzenie przyjęcia danych od servera, jeżeli brak po 2 sek to server padł 
       {server, BuforSize} ->
           io:format(">Producent ~p | dostarczyl do servera liczbe: ~p  | rozmiar bufora: ~p ~n",[self(), Produkt, BuforSize]),
           odblokuj(),                 % zwolnienie dostępu do bufora. 
           timer:sleep(500),           % uśpij proces producenta  - tylko na potrzeby syulacji, żeby widzieć co się dzieje w oknie konsoli!   
           producerLoop(Produkt + 1)             
        after 2000 ->                  % zakoncz proces producenta bo server padł 
           odblokuj(),                 % zwolnienie dostępu do bufora. 
           {error, server_padl}
    end.
    
    

%  ----------------- concumer w nieskonczonej pętli pobiera dane z servera  -----------------
%  ConsumerNr : każdy konsument ma swój unikalny numer - na potrzeby testów 

consumerLoop(ConsumerNr) ->
   blokuj(),                          % Konsument otrzymuje wyłączy dostęp do bufora 
   server ! {is_data, self()},        % Sprawdż czy w w buforze są jakieś dane 
   receive 
      {server, yesEmpty} ->           % bufor jest pusty dlatego zwolnij mutexa i czekaj na kolejne wywołanie
          odblokuj(),                 % zwolnienie dostępu do bufora. 
          consumerLoop(ConsumerNr);                         
      {server, noEmpty} ->            % bufor nie jest pusty, można pobrać element z bufora
          ok
      after 2000 ->                   % zakoncz proces konsumenta bo server padł 
          odblokuj(),                 % zwolnienie dostępu do bufora. 
          exit(normal)    
   end,
   server ! {take, self()},           % wysłanie wiadomości do bufora celem pobrania liczby 
   receive
       {server, Value, BuforSize} ->  % otrzymanie : liczny + aktualny rozmiar bufora 
           io:format("Consumer ~p  pid:  ~p| pobrano wartosc: ~p | rozmiar bufora: ~p ~n",[ConsumerNr, self(), Value, BuforSize]),
           odblokuj(),                % zwolnienie dostępu do bufora. 
           timer:sleep(2500),         % uspij proces biezącego konsumenta aby wymusić pobieranie elememntów przez pozostałe : tylko na potrzeby symulacji! 
           consumerLoop(ConsumerNr)
        after 3500 ->                  % zakoncz proces Konsumenta bo server padł 
           odblokuj(),
           {error, server_padl}
   end.
  



%  ----------------- proces servera : Lista : zawiera dane które pochodzą od producenta i któe wysyłam do konsumenta -----------------
serverLoop(Lista) ->
   receive
      {put, From, Produkt} ->                     % sekcja na potrzeby producent który wrzuca tutaj dane 
          NewList = Lista ++ [Produkt],           % dane pakowane są na koniec listy : ma to działac jak FIFO
          From ! {server, length(NewList)},       % wyslij potwierdzenie przyjecia danych + aktualną liczbę elementów w buforze
          serverLoop(NewList);
      {take, From} ->                             % sekcja na potrzeby konsumenta który pobiera tutaj dane.
          [H|T] = Lista,                          % dane są pobierane z początku listy : ma to działać jak FIFO 
          From ! {server, H, length(T)},
          serverLoop(T);

      {is_data, From} ->                           % sekcja na potrzeby konsumenta który pyta bufor czy sa w nim jakieś dane
          case length(Lista) > 0 of
              true ->  From ! {server, noEmpty},
                       serverLoop(Lista);
              false -> From ! {server, yesEmpty},
                       serverLoop(Lista)
          end;

      {is_full, From} ->                           % sekcja na potrzeby producenta który pyta bufor czy jest już pełny
          case length(Lista) == 5 of               % rozmiar bufora to max 5 elementów 
              true -> From!{server, yesFull},      % bufor jest pełny!
                      serverLoop(Lista);
              false -> From!{server, noFull},      % bufor nie jest pełny! 
                      serverLoop(Lista)
          end;
      {stop} ->                                    % zakoncz proces server 
          exit(kill)                               % wykonuje "abnormal termination" aby pozabijać pozostałe procesy które linkuje z serverem. Czyli procesy konsumenta producenta i mutex.
   end.




%---------------------------------------------------------------------------------------------
%------------------ Funkcje startowe poszczególnych częsci składowych systemu  ---------------
%---------------------------------------------------------------------------------------------


%#########################################
%--------- proces startowy -------------
% Opis: tworzy on wszystkie pozostałe procesy programu. 
% Idę jest taka, aby oddzielić wszystkie procesy 
% wchodzące w skład naszego projektu od procesu shella. 
start(N) ->
    io:format("proces startowy Pid: ~p ~n",[self()]),
    
    PidServer = serverStart(), 
    timer:sleep(1500),

    makeMutex(PidServer),
    timer:sleep(1500),

    makeProducer(PidServer),
    timer:sleep(1500),

    makeConsumers(N, PidServer),
    
    timer:sleep(12000),
    server ! {stop}.

% ------ funkcja tworząca proces startowy ----------- !!!!!!!!!!!!!!!! TĄ FUNKCJĘ TRZEBA WYWOŁAĆ ABY URUCHOMIĆ PROGRAM! !!!!!!!!!!!!!!!
str(N) ->
    spawn(mod, start, [N]).

% Aby odpalić program wpisz:  
% > c(mod).
% > mod:str(N) gdzie N liczba naturalna.     
%######################################### 




%############################################################################
%--------- funkcja tworząca proces servera. Zwraca ona jego pid -------------
%--------- pid ten jest wykorzytywane przez inne funkcje! -------------------

serverStart() ->
    Pid = spawn(mod, serverLoop, [[]]),
    register(server,  Pid),
    io:format("serverLoop: ~p ~n",[Pid]),
    Pid.



%#########################################
%--------- proces producenta -------------
producerStart(ServerPid) ->
    link(ServerPid),
    register(consumer, self()),
    io:format("producerLoop: ~p ~n",[self()]),
    producerLoop(0).

% ------ funkcja tworząca proces producenta -----------

makeProducer(ServerPid) ->
    spawn(mod, producerStart, [ServerPid]).
%######################################### 



%#########################################
%------- proces konsumenta ---------------
% ConsumerNr : unikalny numer - tylko na potrzeby testowe 
% ServerPid  : pid server aby powiązać nowo powstały proces konsumenta z nim. Kiedy server "wybuchnie" spowoduje to śmierć procesu konsumenta

consumerStart(ConsumerNr, ServerPid) ->  % Każdy konsument ma swój numer. 
    link(ServerPid),
    io:format("New Consumer nr: ~p  pid: ~p ~n",[ConsumerNr, self()]),
    consumerLoop(ConsumerNr).


%--------- funkcja tworząca N procesów konsumentów ----
makeConsumers(N, ServerPid) ->
   [spawn(mod, consumerStart, [X, ServerPid]) || X <-lists:seq(1,N)],
   {utworzono_konsumentow}.
%#########################################


%---- Proces MUTEXa wykorzystywane na potrzeby synchronizacji przez naszą implementację wzorca producer/consumer

%#########################################
%------- proces mutexa ---------------
% ServerPid :pid server aby powiązać nowo powstały proces mutexa z nim. Kiedy server "wybuchnie" spowoduje to śmierć procesu mutexa.

startMutex(ServerPid) ->
    link(ServerPid),
    register(mutex, self()),
    io:format("Mutex pid: ~p ~n",[self()]),
    wolny().                   % proces mutexu zaczyna swoje życie jako stan 'wolny'


%------- funkcja tworząca proces mutexa ---------------
makeMutex(ServerPid) ->
    spawn(mod, startMutex, [ServerPid]).
%#########################################




%---blokuj(), odblokuj() : funkcje wykorzystywane przez inne procesy w celu manipulacji dostępu do MUTEXa-----
blokuj() ->
    mutex ! {blokuj, self()},   
    receive                     
        {mutex, mozna_wchodz} ->
            wchodze
    end.                        

odblokuj() ->
   mutex ! {odblokuj, self()}.



%--------- funkcje procesu MUTXa ----------
% wolny()  :  stan wolny
% zajęty() :  stan zajęty
% KOmentarz: proces mutexa składa się z dwóch funkcji. Nalezy przy tym pamiętać, że mimo to proces posiada 
%            tylko jedną skrzynkę odbiorczą z wiadomościami! Jest to ważne przy zrozumieniu procesu maszyny stanowej! 

wolny() ->
   receive
       {blokuj, Pid} ->
           Pid!{mutex, mozna_wchodz},
           zajety(Pid);
       {stop} ->  % zatrzymanie procesu mutexa
           true  
   end.

zajety(Pid) ->
   receive
      {odblokuj, Pid} ->
         wolny()
   end.







%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%-----------------------------------------  ALGORYTMY ----------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------


% Wstępna uwaga: Wszystko na temat algorytmów, elekcji itd jest ładnie opisane w pdf poniżej 
% Link: http://wazniak.mimuw.edu.pl/images/7/78/Sr-7-wyk-2.0.pdf
% Dodatkowo warto konfrontować to z tym co pisze PTM w pdf'ie algorytmy - harikrishna! :)





%#####################
% Zad 1. 
% Treść: Zaimplementuj Zegar wektorowy | Vector clock
% Opis algorytmu : http://en.wikipedia.org/wiki/Vector_clock + str 26 od PTM.



% 1.
% Wiki:  1. Initially all clocks are zero.
% PTM :  1. Początkowo wszystkie zegary ustawione są na 0 

% 2.
% Wiki:  2. Each time a process experiences an internal event, it increments its own logical clock in the vector by one.
%  PTM:  2. proces zwiększa swój zegar przed wystąpieniem każdego swojego zdarzenia

% 3.
% Wiki:  3. Each time a process prepares to send a message, it increments its own logical clock in the vector by one
%           and then sends its entire vector along with the message being sent.
%  PTM:  3. każdorazowo, przy wysyłaniu wiadomości proces zwiększa swój zegar o 
%           jeden i przesyła cały swój wektor wraz z wiadomością



% 4. (problem)
% Wiki:  4. Each time a process receives a message, it increments its own logical clock in the vector by one 
%           and updates each element in its vector by taking the maximum of the value in its own vector clock 
%           and the value in the vector in the received message (for every element).
% 
%  PTM:  4. każdorazowo, przy odbiorze wiadomości proces zwiększa swój zegar o jeden
%           i aktualizuje wszystkie elementy w swojej kopii zegara biorąc
%           maksimum z wartości lokalnej oraz z otrzymanego wektora
% 





-module(mod).
-compile(export_all).


%###########################################################
%------------------- funkcja procesu -----------------------
%  Numer : unikalny numer procesu - tylko w celach diagnostycznych.
%  ClockList : wewnętrzna kopia listy zegarów 
%  ProcessList : lista wszystkich procesów do których mogę wysłać wiadomość. 

loop(Numer, ClockList, ProcessList) ->

    NewClockList1 = internatEvent(Numer, ClockList),       % symulacja internal event : Krok (2) -> zwraca nową listę zegarów

    NewClockList2 = incListaZegarow(NewClockList1, Numer), % przygotowanie do wysłania msg : Krok (3) -> zwiększam o 1 wartośc swojego zegara

    losujProces(ProcessList) ! {msg, NewClockList2},       % wysłanie wiadomości do losowo wybranego procesu -> przesyłam wewnętrzną tablicę zegarów

    receive
        {msg, MsgList} ->
            NewClockList3 = func(NewClockList2, MsgList),  % odebranie wiadomości:  Krok(4) -> tworzenie nowej listy zegarów
            timer:sleep(2000),
            io:format("Proces nr: ~p  pid: ~p | innerClock: ~w  | receivedClock: ~w | combinedClock: ~w ~n",[Numer, self(), NewClockList2, MsgList, NewClockList3]),
            loop(Numer, NewClockList3, ProcessList)
    end.


%----------- funkcja tworząca proces -------
% Opis: dzięki tej funkcji mam możliwośc przekazania parametru ProcessList do funkcji loop! 

makeLoop(Numer, ClockList) ->
   receive
       {set, Lista} ->
                NewProcessList = Lista            
   end,
   loop(Numer, ClockList, NewProcessList).



%###########################################################






% ---------- Wykonuje 2 krok algorytmu : symulacja Inner Event -------------------
% Opis: Losowanie liczby od 1-100. Jeżeli reszta z dzielena przez 30 to 0 to traktuje to 
% jako wystąpienie 'internal event' i zwiększam zegar o 1 - zwracam nową tablicę zegarów po modyfikacji.


internatEvent(Numer,ClockList) ->
    case random:uniform(100) rem 30 == 0 of
        true -> 
            io:format("wystapil 'inner event' w procesie pid: ~p ~n",[self()]),
            incListaZegarow(ClockList, Numer);  % wysąpił internal event, zwiększ zegar o 1.
        false -> ClockList
    end.



%----  Używana w 2 i 3 kroku algorytmu przy inkrementacji zegara------------------------

incListaZegarow(ListaZegarow, Index) ->
    InkrementowanyZegar = lists:nth(Index, ListaZegarow),
    Temp = InkrementowanyZegar + 1,
    lists:sublist(ListaZegarow, Index-1) ++ [Temp] ++ lists:sublist(ListaZegarow, Index+1, length(ListaZegarow)).


%------- Wykonuje 4 krok algorytmu -----------------------

func([], []) -> [];
func([H1|T1], [H2|T2]) -> 
   [max(H1,H2) | func(T1, T2)].


%---------- Funkcja pomocnicza losująca proces do którego wysłać wiadomość ----------

losujProces(List) ->
    lists:nth(random:uniform(length(List)),List).



%--------- funkcja startowa ------------------------ !!!!!!!!!!!!!!!! JĄ NALEŻY ODPALIĆ, ABY URUCHOMIĆ PROGRAM ~!!!!!!!!!!
% N : liczba procesów 

start(N) ->
   ClockList = [0 || _<-lists:seq(1,N)],                        % list zegarów : domyślenie wszystko na zero 
   ListaProcesow = [spawn(mod, makeLoop, [X, ClockList]) || X <- lists:seq(1,N)],
   [X ! {set, ListaProcesow} || X <-ListaProcesow],
   {koniec_funkcji_startowej}.



%//------------------- PRZYKŁADOWY WYNIK -----------------------
%3> mod:start(5).
%{koniec_funkcji_startowej}
%Proces nr: 4  pid: <0.42.0> | innerClock: [0,0,0,1,0]  | receivedClock: [1,0,0,0,0] | combinedClock: [1,0,0,1,0] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [0,0,1,0,0]  | receivedClock: [1,0,0,2,0] | combinedClock: [1,0,1,2,0] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,0,0,2,0]  | receivedClock: [0,1,0,0,0] | combinedClock: [1,1,0,2,0] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,1,0,3,0]  | receivedClock: [0,0,1,0,0] | combinedClock: [1,1,1,3,0] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,0,2,2,0]  | receivedClock: [1,0,2,2,0] | combinedClock: [1,0,2,2,0] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,0,3,2,0]  | receivedClock: [1,1,0,3,0] | combinedClock: [1,1,3,3,0] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,1,1,4,0]  | receivedClock: [0,0,0,1,0] | combinedClock: [1,1,1,4,0] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,1,1,5,0]  | receivedClock: [0,0,0,0,1] | combinedClock: [1,1,1,5,1] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,1,4,3,0]  | receivedClock: [1,0,3,2,0] | combinedClock: [1,1,4,3,0] 
%Proces nr: 2  pid: <0.40.0> | innerClock: [0,1,0,0,0]  | receivedClock: [1,1,1,6,1] | combinedClock: [1,1,1,6,1] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,1,5,3,0]  | receivedClock: [1,1,1,5,0] | combinedClock: [1,1,5,5,0] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,1,1,6,1]  | receivedClock: [1,1,1,4,0] | combinedClock: [1,1,1,6,1] 
%Proces nr: 1  pid: <0.39.0> | innerClock: [1,0,0,0,0]  | receivedClock: [1,1,1,7,1] | combinedClock: [1,1,1,7,1] 
%Proces nr: 2  pid: <0.40.0> | innerClock: [1,2,1,6,1]  | receivedClock: [1,1,6,5,0] | combinedClock: [1,2,6,6,1] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,1,1,7,1]  | receivedClock: [1,1,4,3,0] | combinedClock: [1,1,4,7,1] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,1,6,5,0]  | receivedClock: [1,1,5,3,0] | combinedClock: [1,1,6,5,0] 
%Proces nr: 1  pid: <0.39.0> | innerClock: [2,1,1,7,1]  | receivedClock: [1,1,7,5,0] | combinedClock: [2,1,7,7,1] 
%Proces nr: 2  pid: <0.40.0> | innerClock: [1,3,6,6,1]  | receivedClock: [1,1,4,8,1] | combinedClock: [1,3,6,8,1] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,1,7,5,0]  | receivedClock: [1,2,1,6,1] | combinedClock: [1,2,7,6,1] 
%Proces nr: 2  pid: <0.40.0> | innerClock: [1,4,6,8,1]  | receivedClock: [1,2,8,6,1] | combinedClock: [1,4,8,8,1] 
%Proces nr: 4  pid: <0.42.0> | innerClock: [1,1,4,8,1]  | receivedClock: [1,4,6,8,1] | combinedClock: [1,4,6,8,1] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [1,2,8,6,1]  | receivedClock: [2,1,1,7,1] | combinedClock: [2,2,8,7,1] 
%Proces nr: 3  pid: <0.41.0> | innerClock: [2,2,9,7,1]  | receivedClock: [1,3,6,6,1] | combinedClock: [2,3,9,7,1] 






%#####################
% Zad 2. 
% Treść:  Algorytm pierścieniowy Tanenbauma 
% Opis:   32 pdf od PTM  + http://smurf.mimuw.edu.pl/node/948 + http://wazniak.mimuw.edu.pl/images/7/78/Sr-7-wyk-2.0.pdf


% Opis słowno-muzyczny algorytmów Elekcji -  stosuje się do Tyrana jak i do pierścieniowego 

%   1. Do wyboru koordynatora stosuje się tzw. algorytmy elekcji (ang. election algorithms).
%   2. Do ustalenia koordynatora niezbędna jest możliwość rozróżnienia procesów,które biorą udział w elekcji.
%   3. Każdy proces ma przypisaną pewną unikalną liczbę, pewnego rodzaju identyfikator.
%   4. Każdy proces zna identyfikatory pozostałych procesów. 
%   5. Algorytm elekcji powinien zapewnić, że jeżeli został
%      wybrany proces-koordynator, to zgodziły się na to wszystkie inne procesy.

%  Różnica między Tyranem i Pierścieniowym? : różnią się jedynie sposobem lokalizacji koordynatora.



% Opis algorytmu pierścieniowego : 

%   1. Gdy jakiś proces zauważy, że koordynator nie funkcjonuje, konstruuje
%      wiadomość ELEKCJA zawierającą jego własny numer i wysyła ją do swojego następcy. {election, NumerProcesu}
%   2. Ten z kolei dodaje do listy w otrzymanej wiadomości swój numer i
%      wysyła do swojego następcy itd. Dodanie numeru przez proces oznacza, że
%      bierze on udział w wyborach, jako jeden z kandydatów na koordynatora.
%   3. Ostatecznie wiadomość dociera z powrotem do procesu który ją zapoczątkował.
%   4. Typ wiadomości jest zmieniany na KOORDYNATOR i wiadomość puszczana jest
%      w obieg jeszcze raz. Tym razem jednak w celu powiadomienia wszystkich
%      procesów, kto jest koordynatorem (proces na liście o największym numerze).



-module(mod).
-compile(export_all).



%###########################################################
%----------------------------- funkcja procesu ------------------------------------------------------------
% Nr: Nr procesu, dzieki niemu mozna pobrac pid procesu z listy ProcessList
% IdList : lista wszystkich PIDów procesów w systemie. Dla procesu Nr N jego pid jest w liscie pod indeksem N 
% NrCurrentCordinator : Nr indexu biezacego koordynatora 
% Komentarz: Jeżeli koordynator nawali to w liscie ProcessList w jego miejscu wpisywane jest 0 - ułatwia to sprawę :) 


loop(Nr, ProcessList, NrCurrentCordinator) ->
   receive 
       {padl_koordynator} ->                                                                     %zaczynam elekecje 
             io:format("---- Proces nr: ~p pid: ~p | O kurka, koordynator padl! Zaczynam wybory, bez koordynatora nie ma zycia! ! ~n",[Nr, self()]),
             NewProcessList = eraseCoordinatorFomList(ProcessList, NrCurrentCordinator),         % w miejsce starego koordynatora wpisuje 0 w tablicy procesów
             io:format("---- Proces nr: ~p  pid: ~p : w miejsce starego koordynatora (index = ~p) na liscie aktywnych procesow wpisuje 0 | ListaProcesow: ~w ~n",[Nr, self(), NrCurrentCordinator, NewProcessList]),
             NextPid = getNextPid(Nr, NewProcessList),                                           % Następny element z listy do którego wyślę wiadomość, jeżeli następny element na liście to martwy koordynator, wybierz jeszcze następny element :) 
             io:format("Proces nr: ~p  pid: ~p | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: ~w ~n",[Nr, self(), [Nr]]),
             NextPid ! {election, [Nr], Nr, NewProcessList},                                     % dołącz swój numer do listy kandydatów w wyborach :) 
             loop(Nr, NewProcessList, 0); %zeruj miejsce gdzie był stary koordynator, on umarł!  % ... może uda mi się zostać koordynatorem...., ale było by fajnie! :D

        {election, ListOfCandidates, WhoStartedElection, List} when WhoStartedElection /= Nr ->  % wybory trwają w najlepsze 
             NextPid = getNextPid(Nr, List),
             NewListOfCandidates = ListOfCandidates ++ [Nr],      % Lista kandydatów biorących udział w elekcji 
             io:format("Proces nr: ~p  pid: ~p | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: ~w ~n",[Nr, self(), NewListOfCandidates]),
             NextPid ! {election, NewListOfCandidates, WhoStartedElection, List},
             loop(Nr, List, 0);

        {election, ListOfCandidates, WhoStartedElection, List} when WhoStartedElection == Nr ->   % cisza wyborcza - czekanie na wyniki:) (koniec kółka)
             io:format("Proces nr: ~p  pid: ~p | Proces zglaszania kandydatow juz minol!, zamykam liste! | Lista kandydatow: ~w ~n",[Nr, self(), ListOfCandidates]),
             NrNewCurrentCordinator = lists:max(ListOfCandidates),
             NextPid = getNextPid(Nr, List),                % Ogłoszenie światu nowego proroka......   
             io:format("Proces nr: ~p  pid: ~p | Nowym koordynatorem zostaje.... Proces Nr: ~p Pid: ~p | Oglosze to swiatu!.... ~n",[Nr, self(), NrNewCurrentCordinator,  lists:nth(NrNewCurrentCordinator, ProcessList)]),
             NextPid ! {coordynator, NrNewCurrentCordinator, Nr},
             io:format("Proces nr: ~p  pid: ~p wie, ze koordynatorem zostal proces Nr: ~p i przekazuje ta nowie dalej.... pid: ~p ~n",[Nr, self(), NrNewCurrentCordinator, NextPid]),
             loop(Nr, List, NrNewCurrentCordinator);

        {coordynator, NewNrCordinator, WhoStartedNotification} when WhoStartedNotification /= Nr ->
             NextPid = getNextPid(Nr, ProcessList),
             NextPid ! {coordynator, NewNrCordinator, WhoStartedNotification},
             io:format("Proces nr: ~p  pid: ~p wie, ze koordynatorem zostal proces Nr: ~p i przekazuje ta nowie dalej.... do pid: ~p ~n",[Nr, self(), NewNrCordinator, NextPid]),
             loop(Nr, ProcessList, NewNrCordinator);

        {coordynator, _NewIdCordinator, WhoStartedNotification} when WhoStartedNotification == Nr ->
              io:format("Proces nr: ~p  pid: ~p | Udalo sie znalesc nowego koordynatora!Proces ktory zaczol elekcje ja konczy!~n",[Nr, self()]),
              loop(Nr, ProcessList, NrCurrentCordinator); % czy to jest dobrze? sprawdz!

        {stop} ->              %wykorzysytwne do symulacji śmierci procesu który jest koordynatorem 
            io:format("Proces nr ~p pid: ~p | Koordynator pada, cos mnie zabilo! :( ! ~n",[Nr, self()]),
            exit(normal)

   end.

% ------ tworzy proces : chodzi o to aby ułatwić przekazanie parametrów do loop/3 nie zaciemniając sposobu jej działania -----
makeLoop(Nr, _ProcessList, NrCurrentCordinator) ->
    receive
        {set, List} ->
            NewProcessList = List
    end,
    loop(Nr, NewProcessList, NrCurrentCordinator).


%###########################################################


%---------- Funkcja pomocnicza losująca proces który zacznie elekcje ----------

losujProces(List) ->
    lists:nth(random:uniform(length(List)),List).



%---------- zeruj wskaznik na pozycji gdzie był kiedys koordynator -----------
% Komentarz:  Tam gdzie był numer koordynatora pojawia się zero. 

eraseCoordinatorFomList(Lista, Index) ->
    lists:sublist(Lista, Index-1) ++ [0] ++ lists:sublist(Lista, Index+1, length(Lista)).



%----------- Funkcja pomocnicza która ustala jaki jest PID następnego procesu do którego wysłać wiadomość ---
% Index : pobież pierwszy dostępny element który znajduje się przed podanym indexem 
% List  : lista aktywnych procesów. Jeżeli proces umarł w jego miejsce w tablicy wpisuje 0. 
% Uwaga: pamiętaj, że procesy są spiętę w pierścień!  Dlatego podaną listę należy obsługiwać jak by była swego rodzaju buforem cyklicznym.
getNextPid(Index, List) ->
   NewIndex = Index + 1,                     % index następnego elementu 
   case NewIndex > length(List) of           % sprawdź czy przypadkowo nie wychodzi poza zakres tablicy 
      true -> getNextPid(0, List);           % przekroczono zakres, zacznij od najmniejszej możliwej wartości 
      false -> 
          case lists:nth(NewIndex, List) /= 0 of  % sprawdz czy nie wybrano miejsca gdzie jest pochówek martwego procesu - oznaczone jako 0 :-)
             true -> lists:nth(NewIndex, List);   %  OK, udało sie wybrać 'następny' aktywny proces
             false -> getNextPid(NewIndex, List)  %  trafiliśmy na grób, sprawdź 'następny'
          end
   end.




%--------- funkcja startowa ------------------------ !!!!!!!!!!!!!!!! JĄ NALEŻY ODPALIĆ, ABY URUCHOMIĆ PROGRAM ~!!!!!!!!!!
% N : liczba procesów 

start(N) ->
   ListaProcesow = [spawn(mod, makeLoop, [X, [], 5]) || X <- lists:seq(1,N)],     % zakładamy, że proces o numerze 5 jest koordynatorem
   [X ! {set, ListaProcesow} || X <-ListaProcesow],                               % przesłanie listy wszystkich procesów do poszczególnych procsów
   %----- rozpoczęcie symulacji-------
   AktualnyKoordynator = lists:nth(5, ListaProcesow),
   AktualnyKoordynator ! {stop},                                                   % symulacja śmierci koordynatora 
   timer:sleep(2000),
   ProcesWykrywajacyAwarie = lists:nth(4, ListaProcesow),                          % zakładam, że proces nr 4 wykrywa awarie i zaczyna elekecje 
   ProcesWykrywajacyAwarie ! {padl_koordynator} ,                                   % wykrycie awari i zgłoszene problemu -> zaczynamy wybory nowego koordynatora.
   timer:sleep(2000).           % Daj czas na wybory :P 
   % tutaj jeszcze cos dopisz jak Ci się bedzie chciało 


% Aby odpalić program wpisz w konsole:
%     > c(mod)
%     > mod:start(N)  gdzie N to liczba calowita, polecam dla N <7,10> zeby bylo widac cos sie dzieje 
% Nalezy recznie ustawic ktory proces jest koordynatorem i ktory wykrywa awarie! To jest tylko symulacja! 





%//---------------------------------- PRZYKŁADOWY WYNIK: koordynatorem był index: 5 , awarie wykryl index (proces) : 4 --------------------------

% 33> c(mod).
% {ok,mod}
% 34> mod:start(7).
% ---- Proces nr: 4 pid: <0.200.0> | O kurka, koordynator padl! Zaczynam wybory, bez koordynatora nie ma zycia! ! 
% ---- Proces nr: 4  pid: <0.200.0> : w miejsce starego koordynatora (index = 5) na liscie aktywnych procesow wpisuje 0 | ListaProcesow: [<0.197.0>,<0.198.0>,<0.199.0>,<0.200.0>,0,<0.202.0>,<0.203.0>] 
% Proces nr: 4  pid: <0.200.0> | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: [4] 
% Proces nr: 6  pid: <0.202.0> | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: [4,6] 
% Proces nr: 7  pid: <0.203.0> | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: [4,6,7] 
% Proces nr: 1  pid: <0.197.0> | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: [4,6,7,1] 
% Proces nr: 2  pid: <0.198.0> | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: [4,6,7,1,2] 
% Proces nr: 3  pid: <0.199.0> | Zglaszam chec bycia koordynatorem, aktualna lista kandydatow: [4,6,7,1,2,3] 
% Proces nr: 4  pid: <0.200.0> | Proces zglaszania kandydatow juz minol!, zamykam liste! | Lista kandydatow: [4,6,7,1,2,3] 
% Proces nr: 4  pid: <0.200.0> | Nowym koordynatorem zostaje.... Proces Nr: 7 Pid: <0.203.0> | Oglosze to swiatu!.... 
% Proces nr: 4  pid: <0.200.0> wie, ze koordynatorem zostal proces Nr: 7 i przekazuje ta nowie dalej.... pid: <0.202.0> 
% Proces nr: 6  pid: <0.202.0> wie, ze koordynatorem zostal proces Nr: 7 i przekazuje ta nowie dalej.... do pid: <0.203.0> 
% Proces nr: 7  pid: <0.203.0> wie, ze koordynatorem zostal proces Nr: 7 i przekazuje ta nowie dalej.... do pid: <0.197.0> 
% Proces nr: 1  pid: <0.197.0> wie, ze koordynatorem zostal proces Nr: 7 i przekazuje ta nowie dalej.... do pid: <0.198.0> 
% Proces nr: 2  pid: <0.198.0> wie, ze koordynatorem zostal proces Nr: 7 i przekazuje ta nowie dalej.... do pid: <0.199.0> 
% Proces nr: 3  pid: <0.199.0> wie, ze koordynatorem zostal proces Nr: 7 i przekazuje ta nowie dalej.... do pid: <0.200.0> 
% Proces nr: 4  pid: <0.200.0> | Udalo sie znalesc nowego koordynatora!Proces ktory zaczol elekcje ja konczy!
% ok
% 35> 




%#####################
% Zad 4. 
% Treść:  Algorytm tyrana 
% Opis: 31 pdf od PTM + http://wazniak.mimuw.edu.pl/images/7/78/Sr-7-wyk-2.0.pdf + http://pjwstk.dyski.one.pl:81/public/ftp.pjwstk.edu.pl/zsuski/zso/Czesc%202/07-Koordynacja%20rozproszona.pdf


% Opis algorytmu :  Kiedy proces zauważy, że koordynator nie odpowiada to:
%
%
%    1. Proces P wysyła wiadomość ELEKCJA do wszystkich procesów z wyższym numerem.
%    2. jeśli nikt nie odpowiada to wygrywa i wysyła komunikat COORDINATOR do wszystkich procesów z niższymi numerami
%       Komunikat jest postaci {coordinator, Nr} gdzie Nr to numer/pid aktualnego koordynatora 
%    3. jeśli jakiś proces odpowiedział komunikatem ANSWER to proces
%       czeka na komunikat COORDINATOR ==> dlatego potrzebuje dwóch klauzul receive...end.
%    4. jeśli komunikat nie przychodzi to proces rozpoczyna następne wybory (1)
%    5.  jeśli otrzymał komunikat ELECTION to wysyła ANSWER i rozpoczyna nowe wybory




% Opis algorytmu :  Kiedy proces zauważy, że koordynator nie odpowiada to:
%
%
%    1. Proces P wysyła wiadomość ELEKCJA do wszystkich procesów z wyższym numerem.
%    2. jeśli nikt nie odpowiada to wygrywa i wysyła komunikat COORDINATOR do wszystkich procesów z niższymi numerami
%       Komunikat jest postaci {coordinator, Nr} gdzie Nr to numer/pid aktualnego koordynatora 
%    3. jeśli jakiś proces odpowiedział komunikatem ANSWER to proces
%       czeka na komunikat COORDINATOR ==> dlatego potrzebuje trzech klauzul receive...end.
%    4. jeśli komunikat nie przychodzi to proces rozpoczyna następne wybory (1)
%    5.  jeśli otrzymał komunikat ELECTION to wysyła ANSWER i rozpoczyna nowe wybory





-module(mod).
-compile(export_all).


%###########################################################
%-------------------- funkcja procesu ----------------------------
% Nr : numer procesu 
% ProcessList : lista wszystkich procesów w systemie 
% NrCurrentCordinator : index aktualnego koordynatora 

loop(Nr, ProcessList, NrCurrentCordinator) ->
   receive 
       {padl_koordynator} ->                              %zaczynam elekecje | KROK (1)  
           io:format("Proces nr: ~p pid: ~p | O Jeez!, koordynator padl! Zaczynam wybory, bez koordynatora nie ma zycia! ! ~n",[Nr, self()]),
           NewProcessList = eraseCoordinatorFomList(ProcessList, NrCurrentCordinator), % w miejsce starego Koord. wpisuje 0
           ListSendMsg = lists:sublist(NewProcessList, Nr+1, length(NewProcessList)),  % lista procesów z Nr > od bieżacego 
           io:format("{padl_koordynator} | Proces nr: ~p pid: ~p | Lista procesow z wyzszym Nr do ktorych wysylam wiadomosc elekecja ~n Lista =: ~w  ~n",[Nr, self(), ListSendMsg]),
           sengMsg(ListSendMsg, {election, self(), Nr}),      % Do procesów z wyższymi numerami wyślij wiadomość 
           io:format("{padl_koordynator} | Proces nr: ~p pid: ~p | idzie do drugiego receive...end ~n~n",[Nr, self()]);
           %.... a ja sobie ide do drugiego receive... 

       {election, Pid3, _PidNr3, koniec} ->  % sekcja(specjlana) dla potencjalnego koordynatora, ma odesłać answer do innego pida (z mniejszym numerem)
           io:format("---- Proces nr: ~p pid: ~p | to jest specjalna sekcja dla potencjalnego koordynatora ~n",[Nr, self()]),
           Pid3 ! {answer, self(), Nr};
           %.... ide do drugiej klauzuli, nie bedzie wiadomosci juz od nikogo
            

       {election, From, FromNr} ->      %  ---> kiedy lista procesow do ktorych wyslac wiadomosc jest pusa to, idz od razu do 2 reveice...end.
           io:format("{election,...} | Proces nr: ~p pid: ~p | otrzymal wiadomosc 'elekcja' od Procesu nr: ~p pid: ~p  ~n.....i wysyla komunikat 'answer' do Procesu nr: ~p pid: ~p  (czeka on w drugim receive) a on sam przejmuje kontrole nad algorytmem ~n~n",[Nr, self(), FromNr, From, FromNr, From]),
           From ! {answer, self(), Nr},
           NewProcessList = eraseCoordinatorFomList(ProcessList, NrCurrentCordinator), % w miejsce starego Coord. wpisuje 0
           ListSendMsg = lists:sublist(NewProcessList, Nr+1, length(NewProcessList)),  % lista procesów z Nr > od bieżacego 
           io:format("Proces nr: ~p pid: ~p | ide do sekcji {nowe_wybory,..} przejmuje kontrole nad algorytmem  ~n",[Nr, self()]),
           self() ! {nowe_wybory, ListSendMsg, NewProcessList}, % rozpoczynam nowe wybory, ale to w kolejne iteracji 
           loop(Nr, NewProcessList, 0);
            % .... w kolejnej iteracji pojde sobie do klauzuli {nowe_wybory ....}

       {coordinator, NewProcessList, NrNewCoordinator} -> % KROK(2) nr2 | ustawienie numeru nowego koordynatora , tylko procesy o mniejszym numerze niż aktualny koordynator otrzymają tą wiadomość 
           io:format("{coordinator,...} | Proces nr: ~p pid: ~p | Przestawiam sie na nowego koordynatora, jest nim Proces nr: ~p pid: ~p  ~n",[Nr, self(), NrNewCoordinator, lists:nth(NrNewCoordinator, NewProcessList)]),
           loop(Nr, NewProcessList, NrNewCoordinator);

       {stop} ->              %wykorzysytwne do symulacji śmierci procesu który jest koordynatorem 
           io:format("Proces nr ~p pid: ~p | Koordynator pada, cos mnie zabilo! :( ! ~n~n~n",[Nr, self()]),
           exit(normal);  

       {nowe_wybory, ListaGdzieWyslacMsg, NowaListaProcesow} ->  % Czesc Kroku (5)    
           io:format("*{nowe_wybory,..} | Proces nr: ~p pid: ~p | Przejmuje kontrole nad alogyrtmem i szukam dalej koordynatora! ~n",[Nr, self()]), 
           io:format("*{nowe_wybory,..} | Proces nr: ~p pid: ~p | Lista gdzie wysylam dane {nowe_wybory}... L=~w  ~n",[Nr, self(), ListaGdzieWyslacMsg]),
           case length(ListaGdzieWyslacMsg) > 2 of
               true -> 
                    sengMsg(ListaGdzieWyslacMsg, {election, self(), Nr}),    % Do procesów z wyższymi numerami wyślij wiadomość
                    io:format("*{nowe_wybory,..} | Proces nr: ~p pid: ~p | ide do drugiego receive ... end i czekam na 'answer' ~n~n",[Nr, self()]);
               false ->
                     case length(ListaGdzieWyslacMsg) == 2 of % de facto pid + 0
                     false  -> 
                        io:format("*{nowe_wybory,..} | Proces nr: ~p pid: ~p | wraca na poczatek pierwszego receive...end i nie wysylam wiadomosci ~n~n",[Nr, self()]),
                        loop(Nr, NowaListaProcesow, 0);
                     true -> 
                        io:format("*{nowe_wybory,..} | Proces nr: ~p pid: ~p | ide do drugiego receive ... end i czekam na 'answer' wysylam specjalna wiadomosc do pierwszego receive ~n~n",[Nr, self()]),
                        sengMsg(ListaGdzieWyslacMsg, {election, self(), Nr, koniec}),
                        ok % idę do drugiej klazuli receive...end.
                     end 

           end
           %.... a ja sobie ide do drugiego receive... 
   end,
       
   receive
       {answer, From2, FromNr2} ->  %  KROK(3) nr1  | Ktoś z wyższym numerem odpowiedział! Czekam przez określony czas na komunikat w drugiej klauzuli receive, jezeli nie nadejdzie to zaczynam jeszcze raz wybory.
           io:format("---- Proces nr: ~p pid: ~p | Otrzymalem komunikat 'answer' od Procesu nr: ~p pid:~p ! ide do trzeciej klauzuli receive...aby otrzymac info o nowym koordynatorze ~n",[Nr, self(), FromNr2,From2]),
           flush_buffer(),  % czyszczenie skrzynki odbiorczej 
           true

       after 1500 ->      % KROK(2) nr.1    % nikt z wyzszym numerem nie odpowiedział dlatego zostaje koordynatorem i powiadamiam procesy o niższych numerach o tym fakcie.
           io:format("-- Proces nr: ~p pid: ~p | Nikt z wyzszym numerem nie odpowiedzial - JESTEM NOWYM KOORDYNATOREM ~n",[Nr, self()]),
           ListSendMsg2 = lists:sublist(ProcessList, Nr-1),  % lista procesów z Nr > od bieżacego 
           io:format("-- Proces nr: ~p pid: ~p | NOWY KOORDYNATOR | Lista do powiadomienia o nowym koord.: ~w   ~n",[Nr, self(), ListSendMsg2]),
           sengMsg(ListSendMsg2, {coordinator, ProcessList, Nr}),
           loop(Nr, ProcessList, Nr)  
   end,

   receive

       {coordinator, NewProcessList3, NrNewCoordinator2} -> % KROK(3) nr2
           io:format("---- Proces nr: ~p pid: ~p | Przestawiam sie na nowego koordynatora, jest nim Proces nr: ~p pid: ~p  ~n",[Nr, self(), NrNewCoordinator2, lists:nth(NrNewCoordinator2, NewProcessList3)]),
           loop(Nr, NewProcessList3, NrNewCoordinator2)

       after 3000 ->  % KROK(3) nr3 | nie ma komunikatu znaczy zacznij jeszcze raz wybory 
           io:format("---- Proces nr: ~p pid: ~p | Rozpoczynam po raz drugi proces elekcji! (trzecia klauzula receive....end)  ~n",[Nr, self()]),
           NewProcessList3 = eraseCoordinatorFomList(ProcessList, NrCurrentCordinator), % w miejsce starego Koord. wpisuje 0
           ListSendMsg3 = lists:sublist(NewProcessList3, Nr+1, length(NewProcessList3)),  % lista procesów z Nr > od bieżacego 
           sengMsg(ListSendMsg3, {election, Nr}),    % Do procesów z wyższymi numerami wyślij wiadomość
           io:format("---- Proces nr: ~p pid: ~p | Lista procesow z wyzszym Nr do ktorych wysylam wiadomosc elekecja : ~w ! ~n",[Nr, self(), ListSendMsg3]),
           loop(Nr, NewProcessList3, 0)
   end.


% ------ tworzy proces : chodzi o to aby ułatwić przekazanie parametrów do loop/3 nie zaciemniając sposobu jej działania -----
makeLoop(Nr, _ProcessList, NrCurrentCordinator) ->
    receive
        {set, List} ->
            NewProcessList = List
    end,
    loop(Nr, NewProcessList, NrCurrentCordinator).


%###########################################################


%--------- funkcja startowa ------------------------ !!!!!!!!!!!!!!!! JĄ NALEŻY ODPALIĆ, ABY URUCHOMIĆ PROGRAM ~!!!!!!!!!!
% N : liczba procesów 
start(N) ->   
   ListaProcesow = [spawn(mod, makeLoop, [X, [], 8]) || X <- lists:seq(1,N)],     % zakładamy, że proces o numerze 8 jest koordynatorem
   [X ! {set, ListaProcesow} || X <-ListaProcesow],                               % przesłanie listy wszystkich procesów do poszczególnych procsów
   %----- rozpoczęcie symulacji-------
   AktualnyKoordynator = lists:nth(8, ListaProcesow),                             % koordynatorem będzie 8 proces na liscie
   AktualnyKoordynator ! {stop},                                                  % symulacja śmierci koordynatora ,
   ProcesWykrywajacyAwarie = lists:nth(4, ListaProcesow),                         % zakładam, że proces nr 4 wykrywa awarie i zaczyna elekecje 
   ProcesWykrywajacyAwarie ! {padl_koordynator}.                                  % wykrycie awari i zgłoszene problemu -> zaczynamy wybory nowego koordynatora.

   % tutaj jeszcze cos dopisz jak Ci się bedzie chciało 




%---------- Funkcja pomocnicza losująca proces który zacznie elekcje ----------

losujProces(List) ->
    lists:nth(random:uniform(length(List)),List).


%---------- zeruj wskaznik na pozycji gdzie był kiedys koordynator -----------
% Komentarz:  Tam gdzie był numer koordynatora pojawia się zero. 

eraseCoordinatorFomList(Lista, Index) ->
    lists:sublist(Lista, Index-1) ++ [0] ++ lists:sublist(Lista, Index+1, length(Lista)).

%---------------- wysyłanie wiadomości do procesów z wyższymi NR (o ile pod indeksem nie ma wartości 0) ------

sengMsg([], _Tresc) -> {pusta_lista};
sengMsg(Lista, Tresc) ->
    io:format("-------> funkcja sengMsg() : Lista: ~w  Wiadomosc: ~w ~n",[Lista, Tresc]),
   [X!Tresc || X <- Lista, X /= 0],
   {ok}.



%------------------------ czyszczenie skrzynki -------------------

flush_buffer() ->
receive
    _Any ->
    flush_buffer()
after 0 ->
    true
end.



%//------------------------------- Przypadek brzegowy:  Pada nr8 awarie wykrywa nr 7 
%6> mod:start(8).
%Proces nr 8 pid: <0.58.0> | Koordynator pada, cos mnie zabilo! :( ! 
%---- Proces nr: 7 pid: <0.57.0> | O kurka, koordynator padl! Zaczynam wybory, bez koordynatora nie ma zycia! ! 
%---- Proces nr: 7 pid: <0.57.0> | Lista procesow z wyzszym Nr do ktorych wysylam wiadomosc elekecja : [0] ! 
%ok
%---- Proces nr: 7 pid: <0.57.0> | Rozpoczolem elekcje, ale nikt z wyzszym numerem nie odpowiedzial - JESTEM NOWYM KOORDYNATOREM 
%---- Proces nr: 7 pid: <0.57.0> | Powiadamiam procesy z niÅ¼szymi numerami o tym, ze zostaÅem koordynatorem : Lista: [<0.51.0>,<0.52.0>,<0.53.0>,<0.54.0>,<0.55.0,<0.56.0>] 
%---- Proces nr: 1 pid: <0.51.0> | Przestawiam sie na nowego koordynatora, jest nim Proces nr: 7 pid: <0.57.0>  
%---- Proces nr: 2 pid: <0.52.0> | Przestawiam sie na nowego koordynatora, jest nim Proces nr: 7 pid: <0.57.0>  
%---- Proces nr: 3 pid: <0.53.0> | Przestawiam sie na nowego koordynatora, jest nim Proces nr: 7 pid: <0.57.0>  
%---- Proces nr: 4 pid: <0.54.0> | Przestawiam sie na nowego koordynatora, jest nim Proces nr: 7 pid: <0.57.0>  
%---- Proces nr: 5 pid: <0.55.0> | Przestawiam sie na nowego koordynatora, jest nim Proces nr: 7 pid: <0.57.0>  
%---- Proces nr: 6 pid: <0.56.0> | Przestawiam sie na nowego koordynatora, jest nim Proces nr: 7 pid: <0.57.0>
