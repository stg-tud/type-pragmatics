package VeritasSpoofax.strategies;

import org.strategoxt.lang.JavaInteropRegisterer;
import org.strategoxt.lang.Strategy;

public class InteropRegisterer extends JavaInteropRegisterer {
    public InteropRegisterer() {
        super(new Strategy[] { 
        		backend_strategy_0_0.instance,
        		backend_proof_strategy_0_0.instance,
        		backend_consistency_strategy_0_0.instance
    		});
    }
}
