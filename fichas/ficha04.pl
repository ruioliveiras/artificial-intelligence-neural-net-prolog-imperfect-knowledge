
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SEMANA 05: 16.MAR.2015 - 20.MAR.2015

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic filho/2.
:- dynamic pai/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado filho: Filho,Pai -> {V,F,D}

consult('ficha4.pl').
t1 :- evolucao(ficho(joao,jose)).
t2 :- evolucao(ficho(ola,ola2)).


filho( joao, jose ).
filho( jose, manuel ).
filho( carlos,jose ).


% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+filho( F,P ) :: (solucoes( (F,P),(filho( F,P )),S ),
                  comprimento( S,N ), 
                  N == 1
                  ).

% Invariante Referencial: nao admitir mais do que 2 progenitores
%                         para um mesmo individuo

% +filho( F,P ) :: ( ...
%                   ...
%                   ...
%                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento
% findall().


evolucao( Termo ) :-
    findall(I,+T::I,S), %% aka solucoes
    insere(T),
    teste(S).

insere( T ) :- assert(T).
insere( T ) :- retract(T) , fail. % para backtrack

teste([]).
teste([I|L]) :-
    I,
    teste(L).

solucoes(A, T, S) :- 
    T,
    assert(tempo(A)),


comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.