package st.redline.compiler;

import st.redline.ScriptListener;

import java.util.List;

public class ScriptClassFileGenerator extends ClassFileGenerator {

	public ScriptClassFileGenerator(ParsedSource parsedSource, ScriptListener scriptListener) {
		super(parsedSource, scriptListener);
		parsedSource.isScript(true);
		parsedSource.add(new ScriptClassDefinition(parsedSource));
		parsedSource.add(new ScriptDoItMethod());
	}

	public void execute() {
		super.execute();
	}

	private class ScriptClassDefinition extends ClassDefinition {

		private final String[] EMPTY = new String[0];
		private final String SUPERCLASS = "Object";
		private final String CATEGORY = "Script";

		private String subclass;

		public ScriptClassDefinition(ParsedSource parsedSource) {
			super(null);
			subclass = parsedSource.sourcePath().substring(parsedSource.sourceFolder().length());
			if (subclass.endsWith(".st"))
				subclass = subclass.substring(0, subclass.length()-3);
		}

		public boolean isScript() { return true; };
		public Token rawInstanceVariableNames() { return null; }
		public Token rawClassVariableNames() { return null; }
		public Token rawPoolDictionaries() { return null; }
		public String subclass() { return subclass; }
		public int lineNumber() { return 0; }
		public String superclass() { return SUPERCLASS; }
		public String[] classVariableNames() { return EMPTY; }
		public String[] instanceVariableNames() { return EMPTY; }
		public String[] poolDictionaries() { return EMPTY; }
		public String category() { return CATEGORY; }
	}

	private class ScriptDoItMethod extends Method {
		private final MessagePattern messagePattern = new ScriptDoItMessagePattern();
		public MessagePattern messagePattern() { return messagePattern; }
	}

	private class ScriptDoItMessagePattern extends MessagePattern {
		public ScriptDoItMessagePattern() { super(null); }
		public int lineNumber() { return 0; }
		public String pattern() { return "doIt"; }
		public int argumentCount() { return 0; }
	}
}