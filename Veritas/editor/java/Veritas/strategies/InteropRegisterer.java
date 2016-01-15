package Veritas.strategies;

import org.strategoxt.lang.JavaInteropRegisterer;
import org.strategoxt.lang.Strategy;

import de.tu_darmstadt.veritas.backend.backend_strategy_0_0;
import de.tu_darmstadt.veritas.backend.backend_consistency_strategy_0_0;

/**
 * Helper class for {@link java_strategy_0_0}.
 */
public class InteropRegisterer extends JavaInteropRegisterer {

  public InteropRegisterer() {
    super(new Strategy[] { 
    		java_strategy_0_0.instance,
    		backend_strategy_0_0.instance,
    		backend_consistency_strategy_0_0.instance
		});
  }
}
