package st.redline.compiler;

import st.redline.ScriptListener;

public class GeneratorFactory {

	public Executor generatorFor(ParsedSource parsedSource, ScriptListener scriptListener) {
		if (parsedSource.hasClassDefinition())
			return new ClassFileGenerator(parsedSource, scriptListener);
		return null;
	}
}
