[
	File -- V [ _1 _2 ],
	Formula -- H hs=0 ["fof('" _1 "', " _2 ", " _3 ")."],
	Axiom -- "axiom",
	Conjecture -- "conjecture",
	All -- "(![" _1 "] : " _2 ")",
	All.1:iter-star-sep -- _1 ",",
	Exists -- "(?[" _1 "] : " _2 ")",
	Exists.1:iter-star-sep -- _1 ",",
	Impl -- "(" _1 " => " _2 ")",
	Neq -- "(" _1 "!=" _2 ")",
	Eq -- "(" _1 "=" _2 ")",
	Pred -- _1 "(" _2 ")",
	Pred.2:iter-star-sep -- _1 ",",
	TermApp -- _1 "(" _2 ")",
	TermApp.2:iter-star-sep -- _1 ",",
	And -- "(" _1 " & " _2 ")",
	Or -- "(" _1 " | " _2 ")",
	EqualityJudgement -- "(" _1 " = " _2 ")",
	True -- "$true",
	False -- "$false",
	
	MetaVariable -- _1,
	Meta -- _1,
	Var -- _1,
	Nat -- _1,

	PremiseList -- _1,
	ConsequenceList -- _1
	
]