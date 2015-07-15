%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SEMANA 02: 23.FEV.2015 - 27.FEV.2015

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Operacoes aritmeticas e conjuntos (listas).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,Soma -> {V,F}

soma( X,Y,Soma ) :-
    Soma is X+Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,Z,Soma -> {V,F}

soma( X,Y,Z,Soma ) :-
    Soma is X+Y+Z.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: Lista,Soma -> {V,F}

soma( [],0 ).
soma( [X|L],Soma ) :-
    soma( L,S ),
    Soma is X+S.
