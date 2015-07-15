%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SEMANA 03: 02.MAR.2015 - 06.MAR.2015

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Operacoes sobre listas.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

r :- consult('fichas/test.pl').

% ficha<b<c<.

% ficha<A<B< :- pai<B<A<.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento,Lista -> {V,F}

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \== Y,
    pertence( X,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista,Comprimento -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado quantos: Lista,Comprimento -> {V,F}

quantos(E,[],0).
quantos(E,[E|T],C) :- 
    quantos(E, T, Ca),
    C is Ca + 1.
quantos(E,[H|T],C) :- 
    quantos(E, T, C).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado apagar: Elemento,Lista,Resultado -> {V,F}

apaga(E,[],[]).
apaga(E,[E|T], T).
apaga(E,[Y|T], [Y|K]) :- 
    E \== Y,
    apaga(E,T,K).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado apagatudo: Elemento,Lista,Resultado -> {V,F}

apagaTudo(E,[],[]).
apagaTudo(E,[E|T], K) :- 
    apagaTudo(E,T,K).
apagaTudo(E,[Y|T], [Y|K]) :- 
    E \== Y,
    apagaTudo(E,T,K).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado adicionar: Elemento,Lista,Resultado -> {V,F}
% a lista que se recebe já nao tem repetidos para apagar repetidos ver outra função.

adicionar(E,[],[E]).
adicionar(E,L,[E|K]) :-
    apaga(E,L,K).


repetidos([], []).
repetidos([H|T], [H|R]) :-
    apagaTudo(H, T, K),
    repetidos(K, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: Lista1,Lista2,Resultado -> {V,F}

concatena([LH|LT], K, [LH|R]) :-
    concatena(LT,K,R).
concatena([], K, K).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado inverter: Lista,Resultado -> {V,F}

inverter([],[]).
inverter([H|T], R) :-
    inverter(T,Ri),
    concatena(Ri,[H],R).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sublista: SubLista,Lista -> {V,F}

sublista2(SL,L) :- concatena(L1,SL,L3), concatena(L3,L2,L).

s1 :- sublista2([1,2],[1,2]).
s2 :- sublista2([1,2],[1,2,3]).

sublista([],R).
sublista(S,R) :- sublistaInicio(S,R).
sublista(S,[H|T]) :- sublista(S,T).

sublistaInicio([],TB).
sublistaInicio([A|TA],[A|TB]) :-
    sublistaInicio(TA,TB).

t1 :- sublista([1,2],[1,2,3,4]).
t2 :- sublista([3,4],[1,2,3,4,5]).
t3 :- sublistaInicio([1],[1,2,3]).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% o que quereomos ?solucoes(F,filho(F,jose),S); a primeira variavel cerve para unificar as cenas.
% sixtus prolog
% 'findall' mesmos argumentoss


% solucoes(T,Q,S)


% solucoes(Q,R) :- 
%    Q(T),
%    assert(mem(T)),
%    fail.
%solucoes(Q,R) :-
%    procurar 

%resultado(RA,R) :-
%   retract(mem(T)),
%    resultado([T|RA],R).

%resultado(R,R).




% retract

