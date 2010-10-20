package st.redline.compiler;

import st.redline.ScriptListener;

public class ScriptClassFileGenerator extends ClassFileGenerator {

	public ScriptClassFileGenerator(ParsedSource parsedSource, ScriptListener scriptListener) {
		super(parsedSource, scriptListener);
	}

	public void execute() {
		// TODO - generate DoIt - Class Definition and doIt method.
		System.out.println("TODO - generate DoIt - Class Definition and doIt method.");
		super.execute();
	}
}