package de.tu_darmstadt.veritas.backend;

import org.spoofax.interpreter.terms.IStrategoTerm;
import org.strategoxt.lang.Context;
import org.strategoxt.lang.Strategy;

/**
 * Java interface of the BackendStrategy, such that it can be used from Stratego as an external
 * strategy. 
 * 
 * NOTE: Apparently, the Java strategy name uses these _args1_args2 suffixes for name mangling,
 * more specifically for signifying the number of term/strategy arguments.
 */
public class backend_consistency_strategy_0_0 extends Strategy {

	public static backend_consistency_strategy_0_0 instance = new backend_consistency_strategy_0_0();

	@Override
	public IStrategoTerm invoke(Context context, IStrategoTerm current) {
		return Backend$.MODULE$.runAsConsistencyStrategy(context, current);
	}

}
