% Calling Prolog program from command line:
%   swipl -s program.swi -t go
%   swipl -s program.swi -t go -g "init(a,b)"


example(hamming,                         % Name of example
	'examples/hamming_q.swi',        % Query file 
	['examples/hamming_edb.dlv',     % List of rule files		
	 'examples/hamming_idb.dlv',		
	 'examples/hamming_viz.dlv'] ).		
example(rpq_paper,
	'examples/rpq_q.swi',
	['examples/rpq_wood_edb.dlv',		
	 'examples/rpq_idb.dlv',		
	 'examples/rpq_viz.dlv'] ).		
example(simple,
	'examples/simple_q.swi',
	['examples/simple_edb.dlv',		
	 'examples/simple_idb.dlv',		
	 'examples/simple_viz.dlv'] ).		

go(Ex) :-
    example(Ex,_,_),
    retractall(viz(_,_)),	% clear user-defined edge styles
    gen_rpq_facts(Ex,QDB),  % pre-process RPQ query, generating QDB file
    run_dlv(Ex,QDB),        % call DLV on current example plus QDB file
    read_dlv_model(Ex,Xs),
    sort(Xs,Ys), 
    gv_start('g.gv'),
    print_dlv(Ys),
    gv_stop.

gen_rpq_facts(Ex,QDB) :-
	example(Ex,Q,_),      % get query-file Q for Ex
	see(Q),                % open query-file Q for reading 
	read(Regexp),          % get the RPQ expression
	seen,
	atom_concat(Q,'.dlv',QDB),  % create DB-result file QDB ~ Q.dlv
	tell(QDB),             % open QDB file for writing
	gen_subexps(Regexp,Regexp1),         % gen subexps and write to QDB file
	format('root("~w").~n',[Regexp1]),   % also indicate the top-level regexp
	told,
	format('// User query is: ans(X,Y) :- e(X, "~w", Y).~n', [Regexp1]). 



run_dlv(Ex,QDB) :-
	example(Ex,_,Files),                 % get example Files for current example Ex
	atomic_list_concat(Files, ' ', Files1),  % glue them together
	format(atom(Cmd),
	       'dlv -silent ~w ~w > examples/~w_model.swi; echo \'.\' >> examples/~w_model.swi',
	       [QDB, Files1,Ex,Ex]),
	writeln(Cmd),
	shell(Cmd, ExitCode),
	(ExitCode = 0
	-> true
	; format('DLV ERROR: ~w', [ExitCode])
	).


read_dlv_model(Ex,Xs) :-
	format(atom(File), 'examples/~w_model.swi', [Ex]),
	see(File),
	read(T),
	arg(1,T,V),
	args_to_list(V,Xs),
	seen.

print_dlv(Xs) :-
	member(X,Xs),
	format('~p',[X]),
	fail
	;
	true.

portray(ans(F,L,T))  :-
	!,
	gv_edge(F,L,T).
% portray(n(data,N)) :-
% 	!,
% 	viz
% 	format(gv_node(N).
portray(viz(L, S)) :-
	format(' // STYLE ~w IS ~w~n',[L, S]).
portray(X) :-
	format(' // ~w~n',[X]).

args_to_list(A,[A1]) :-
	A \= (_,_),
	process(A,A1).
args_to_list((A,As),[A1|Xs]) :-
	process(A,A1),
	args_to_list(As,Xs).


process(ans(F,Label,T), ans(F,L,T)) :-
	!,
 	(is_list(Label)	->
	 atom_codes(L,Label)
 	;
	 L = Label
 	).
process(viz(Label, Style),viz(L,S)) :-
	!,
	atom_codes(L,Label), atom_codes(S,Style),
	assert(viz(L, S)).
process(Atom,Atom). 

% open file and start new graph
gv_start(FileName):-
	tell(FileName),
	format('digraph {~n'),
	format('rankdir=LR~n'),
%	format('node [shape=circle,style=filled,color=gold]~n').
	format('node [shape=circle]~n').

% finish graph and close file
gv_stop:-
	format('}'),
	told.

gv_edge(From, Label, To) :-
	(setof(Style, viz(Label, Style), Styles)
	->
	 atomic_list_concat(Styles, Styles_atom),
	 format('~w -> ~w [label="~w",~w]~n', [From,To,Label,Styles_atom])
	;
	 format('~w -> ~w [label="~w"]~n', [From,To,Label])
	).
	 
%% gv_edge(From, Label, To) :-
%% 	(viz(Label,Style) ->
%% 	 format('"~w" -> "~w" [~w,label="~w",fontsize="9"]~n', [From,To,Style,Label])
%% 	;
%% 	 format('"~w" -> "~w" [label="~w"]~n', [From,To,Label])
%% 	).



% gen_subexps(R, R1)
%   generates and pretty-prints (as DLV facts) all subexpressions of RegExp R
%   R1 is the prettified version of R
gen_subexps(conc(L,M),L1M1) :-
	!,
	gen_subexps(L,L1),
	gen_subexps(M,M1),
	format(string(L1M1), "~w.~w",[L1,M1]),           % create prettified L1M1 = L1.M1
	format('conc("~w","~w","~w").~n',[L1,M1,L1M1]).  % print it (typically to a file)
gen_subexps(or(L,M),L1M1) :-
	!,
	gen_subexps(L,L1),
	gen_subexps(M,M1),
	format(string(L1M1), "(~w | ~w)",[L1,M1]),
	format('or("~w","~w","~w").~n',[L1,M1,L1M1]).
gen_subexps(plus(L),L1_plus) :-
	!,
	gen_subexps(L,L1),
	format(string(L1_plus), "(~w)+",[L1]),
	format('plus("~w","~w").~n',[L1,L1_plus]).
gen_subexps(star(L),L1_star) :-
	!,
	gen_subexps(L,L1),
	format(string(L1_star), "~w*",[L1]),
	format('star("~w","~w").~n',[L1,L1_star]).
gen_subexps(Atom,Atom).

