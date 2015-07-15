
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% 1º Trabalho Pratico

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(toplevel_print_options, [quoted(true),numbervars(true),portrayed(true),max_depth(100)]).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic filho/2.
:- dynamic pai/2.

:- dynamic sobrinho/2.
:- dynamic tio/2.

:- dynamic neto/2.
:- dynamic avo/2.

:- dynamic primo/2.
:- dynamic irmao/2.

:- dynamic casado/2.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         UTILIDADES
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

rr :- consult('pt1.pl').

evolucao( T ) :-
    solucoes(I,+T::I,S), %% aka solucoes
    insere(T),
    teste(S).

remove( T ) :-
    solucoes(I,-T::I,S), %% aka solucoes
    apaga(T),
    teste(S).

whyevo( T , ER) :-
    solucoes(I,+T::I,S),
    assert(T),
    teste(S, ER),
    retract(T).

whyrem( T , ER) :-
    solucoes(I,-T::I,S),
    retract(T),
    teste(S, ER),
    assert(T).

insere( T ) :- assert(T).
insere( T ) :- retract(T) , ! , fail. % para backtrack

apaga( T ) :- retract(T).
apaga( T ) :- assert(T) , ! , fail. % para backtrack

teste([]).
teste([I|L]) :- I, teste(L).

teste([],ok).
teste([I|L], ER) :- I, teste(L, ER).
teste([I|L], I) :- nao(I). 

nao(X) :- X, !, fail.
nao(X).

solucoes(A,Q,S) :- findall(A,Q,S).


% Função de solucoes criada na aula teorica:
%
%solucoes(A,Q,S) :-
%    Q,
%    assert(temp(A)),
%    fail.
%solucoes(A,Q,S) :- obter([],S).
%obter(X,S) :-
%    retract(temp(A)), obter([A|X],S).
%obter(S,S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%         Funções Lista
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

concatena([LH|LT], K, [LH|R]) :-
    concatena(LT,K,R).
concatena([], K, K).


comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.

unicos(L,S) :- unicosaux([],L,S).

unicosaux(AUX,[H|T],S) :- naoexiste(H,AUX), unicosaux([H|AUX],T,S).
unicosaux(AUX,[H|T],S) :- nao(naoexiste(H,AUX)), unicosaux(AUX,T,S).
unicosaux(S,[],S).

naoexiste(A,[]).
naoexiste(A,[H|T]) :- A\=H, naoexiste(A,T).

tester([H|T]):-
  H.
tester([H|T]):-
  tester(T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

+filho( F,P ) :: (solucoes( (F,P),(filho( F,P )),S ), comprimento( S,N ),  N == 1 ).

% nao admitir mais do que 2 progenitores para um mesmo individuo
+filho( F,P ) :: ( solucoes( X, (filho( F, X )),S1),
                  unicos(S1,S),
                  comprimento( S,N ), 
                  N < 3).

% Pai - nao pode ser pai de si próprio
+pai(F,P) :: (solucoes((F,P),pai(P,F),S1), unicos(S1,S), comprimento(S,N), N == 0).

% Apenas podemos ter 4 avos
+neto(Ne,A) :: (solucoes(X,neto(Ne,X),S), comprimento(S,N), N =< 4 ).

% não inserir info repetida 
+neto( Ne,A ) :: (solucoes( (Ne,A),neto( Ne,A ),S ), comprimento( S,N ),  N == 1).

% nao inserir naturalidade repetida
+naturalidade(A,L,AN,AM) :: (solucoes(X,naturalidade(A,X,AN,AM),S), comprimento(S,N), N =< 1).

% nao inserir naturalidade com ano de nascimento maior que ano de morte
+naturalidade(A,L,AN,AM) :: AN =< AM.

% invariantes de exclusividade de relações
+avo(A,N) :: nao(pai(A,N)).
+avo(A,N) :: nao(filho(A,N)).
+avo(A,N) :: nao(neto(A,N)).
+neto(N,A) :: nao(filho(N,A)).
+neto(N,A) :: nao(pai(N,A)).
+neto(N,A) :: nao(avo(N,A)).
+pai(P,F) :: nao(avo(P,F)).
+pai(P,F) :: nao(filho(P,F)).
+pai(P,F) :: nao(neto(P,F)).

+tio(X,Y) :: nao(pai(X,Y)).
+tio(X,Y) :: nao(sobrinho(X,Y)).
+tio(X,Y) :: nao(irmao(X,Y)).

+sobrinho(X,Y) :: nao(pai(X,Y)).
+sobrinho(X,Y) :: nao(tio(X,Y)).
+sobrinho(X,Y) :: nao(irmao(X,Y)).

-tio(X,Y) :: (nao(tio(X,Y))).
-sobrinho(X,Y) :: (nao(sobrinho(X,Y))).
-primo(X,Y) :: (nao(primo(X,Y))).
-irmao(X,Y) :: (nao(irmao(X,Y))).

+irmao(X,Y) :: nao(pai(X,Y)).
+irmao(X,Y) :: nao(filho(X,Y)).
+irmao(X,Y) :: nao(primo(X,Y)).
+irmao(X,Y) :: nao(tio(X,Y)).
+irmao(X,Y) :: nao(sobrinho(X,Y)).
+irmao(X,Y) :: nao(avo(X,Y)).
+irmao(X,Y) :: nao(neto(X,Y)).

+primo(X,Y) :: nao(pai(X,Y)).
+primo(X,Y) :: nao(filho(X,Y)).
+primo(X,Y) :: nao(irmao(X,Y)).
+primo(X,Y) :: nao(tio(X,Y)).
+primo(X,Y) :: nao(sobrinho(X,Y)).
+primo(X,Y) :: nao(avo(X,Y)).
+primo(X,Y) :: nao(neto(X,Y)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicados e relações entre eles
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% self deterministic 
pai(X,Y) :- clause(filho(Y,X),true).
filho(X,Y) :- nao(clause(filho(X,Y),true)), pai(Y,X). %clause(pai(Y,X),true).

avo(X,Y) :- clause(neto(Y,X),true).
neto(X,Y) :- nao(clause(neto(X,Y),true)), avo(Y,X).  % nao e para evitar informação repetida

tio(X,Y) :- clause(sobrinho(Y,X),true).
sobrinho(X,Y) :- nao(clause(sobrinho(X,Y),true)), tio(Y,X).%clause(tio(X,Y),B), B\=sobrinho(Y,X). nao(clause(sobrinho(X,Y),true))
 
% ligações:
filho(X,Y) :- nao(clause(filho(X,Y),true)), nao(clause(pai(Y,X),true)), irmao(X,A), pai(Y,A).

avo(X,Y) :- nao(clause(avo(X,Y),true)), tester([avo1(X,Y),avo2(X,Y),avo3(X,Y),avo4(X,Y)]).
% avo
avo1(X,Y) :- clause(neto(Y,X),true).
avo2(X,Y) :- pai(X,A), pai(A,Y).
avo3(X,Y) :- clause(primo(Y,A),true), clause(avo(X,A),true).
avo4(X,Y) :- clause(primo(A,Y),true), clause(avo(X,A),true).

tio(X,Y) :- nao(clause(tio(X,Y),true)), tester([tio1(X,Y),tio2(X,Y),tio3(X,Y)]).
%tio
tio1(X,Y) :- irmao(X,A), pai(A,Y), X\=Y.
tio2(X,Y) :- pai(X,A), primo(Y,A), X\=Y.
tio3(X,Y) :- avo(A,Y), pai(A,X),  nao(pai(X,Y)),  X\=Y. %por a ordem inversa não funciona

primo(X,Y) :- nao(clause(primo(X,Y),true)), tester([primo1(X,Y),primo2(X,Y),primo3(X,Y),primo4(X,Y),primo5(X,Y)]).
%primo
primo1(X,Y) :- clause(primo(Y,X),true).
primo2(X,Y) :- pai(A,Y), pai(B,X), irmao(A,B), A\=B, X\=Y.
primo3(X,Y) :- pai(A,Y), clause(tio(A,X),true), X\=Y.
primo4(X,Y) :- pai(A,X), clause(tio(A,Y),true), X\=Y.
primo5(X,Y) :- nao(irmao(X,Y)), avo(A,Y), avo(A,X), X\=Y.

irmao(X,Y) :- nao(clause(irmao(X,Y),true)), tester([irmao1(X,Y),irmao2(X,Y)]).
%irmao
irmao1(X,Y) :- clause(irmao(Y,X),true).
irmao2(X,Y) :- paicomum(X,Y,P1), paicomum(X,Y,P2), P1 @< P2 . %, solucoes(X, (paicomum(X,Y),X\=Y), S), comprimento(S, N),N >= 1.
%irmao3(X,Y) :- paicomum(X,Y,P1), nao(paicomum(X,Y,P2), P1 \= P2).
paicomum(X,Y,A) :- pai(A,X), pai(A,Y), X\=Y.


%clause(neto(X,Y),(clause(filho(X,T1),true), clause(filho(T1,Y),true),  Y \= X),R).

% clause(neto(X,Y), clause(filho(X,T1),true), clause(filho(T1,Y),true),  Y \= X, R)
load :-
    evolucao(pai(a0,a2)),
    evolucao(pai(a0,a3)),
    evolucao(pai(a1,a2)),
    evolucao(pai(a1,a3)),
    evolucao(casado(b1,a2)),
    evolucao(filho(ab1,b1)),
    evolucao(filho(ab1,a2)),
    evolucao(filho(ab2,b1)),
    evolucao(filho(ab2,a2)),
    evolucao(casado(c2,a3)),
    evolucao(filho(ac1,c2)),
    evolucao(filho(ac1,a3)),
    evolucao(filho(ac2,c2)),
    evolucao(filho(ac2,a3)),
    evolucao(neto(c2,c1)),
    evolucao(neto(a1,i1)).
    
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Questão 3
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

pais(X,S) :- solucoes(A,pai(A,X),S1), unicos(S1,S).
avos(X,S) :- solucoes(A,avo(A,X),S1), unicos(S1,S).
filhos(X,S) :- solucoes(A,filho(A,X),S1), unicos(S1,S).
sobrinhos(X,S) :- solucoes(A,sobrinho(A,X),S1), unicos(S1,S).
netos(X,S) :- solucoes(A,neto(A,X),S1), unicos(S1,S).
primos(X,S) :- solucoes(A,primo(X,A),S1), unicos(S1,S).
tios(X,S) :- solucoes(A,tio(A,X),S1), unicos(S1,S).
irmaos(X,S) :- solucoes(A,irmao(X,A),S1), unicos(S1,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Questão 4
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% descendentes
descendentes(A,N,R) :- descendentesAux([A],N,R1), unicos(R1,R).

descendentesAux([],_,[]).
descendentesAux(_,N,[]) :- N =< 0.

descendentesAux([A],1,R) :-
    filhos(A,R).

descendentesAux([A],N,R) :- 
    N > 1,
    filhos(A, F1), N1 is N -1,
    netos(A, F2), N2 is N -2,
    descendentesAux(F1,N1,R1),
    descendentesAux(F2,N2,R2),
    concatena(F1,F2,A1), concatena(A1,R1,A2), concatena(A2,R2,R).

descendentesAux([A|T],N,R) :- 
    descendentesAux([A],N,R1),
    descendentesAux(T,N,R2),
    concatena(R1,R2,R).

% ascendentes
ascendentes(A,N,R) :- ascendentesAux([A],N,R1), unicos(R1,R).

ascendentesAux([],_,[]).
ascendentesAux(_,N,[]) :- N =< 0.

ascendentesAux([A],1,R) :-
    pais(A,R).

ascendentesAux([A],N,R) :- 
    N > 1,
    pais(A, F1), N1 is N -1,
    avos(A, F2), N2 is N -2,
    ascendentesAux(F1,N1,R1),
    ascendentesAux(F2,N2,R2),
    concatena(F1,F2,A1), concatena(A1,R1,A2), concatena(A2,R2,R).

ascendentesAux([A|T],N,R) :- 
    ascendentesAux([A],N,R1),
    ascendentesAux(T,N,R2),
    concatena(R1,R2,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Questão 5
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

relacoes(I1,I2,S) :-
   relacoesAux(I1,I2,Q),
   quais(Q,[],S1),
   unicos(S1,S). 

relacoesAux(X,Y,[filho(X,Y),pai(X,Y),primo(X,Y),sobrinho(X,Y),avo(X,Y),neto(X,Y),tio(X,Y),irmao(X,Y)]).

quais([H|Q],R,S) :-
    H,
    quais(Q,[H|R],S).

quais([H|Q],R,S) :-
%    nao(H),
    quais(Q,R,S).

quais([],S,S).