[
	File -- V [ _1 ],
	Formula -- H hs=1 ["fof('" _1 "'," _2 "," _3 ")."],
	Axiom -- "axiom",
	All -- "![" _1 "] : (" _2 ")",
	All.1:iter-star-sep -- _1 ",",
	Impl -- "(" _1 "=>" _2 ")",
	Pred -- _1 "(" _2 ")",
	Pred.2:iter-star-sep -- _1 ",",
	Abs -- "abs("_1 "," _2 "," _3 ")",
	App -- "app(" _1 "," _2 ")",
	TyNat -- "nat",
	TyFunction -- "arrow(" _1 "," _2 ")",
	PremiseCons -- "(" _1 "&" _2 ")",
	
	MetaVariable -- _1,
	Meta -- _1,
	Var -- _1,
	PremiseList -- _1,
	MetaVariable -- _1
]