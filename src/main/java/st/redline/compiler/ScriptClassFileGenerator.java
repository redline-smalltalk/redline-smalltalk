package st.redline.compiler;

import st.redline.ScriptListener;

public class ScriptClassFileGenerator extends ClassFileGenerator {

	private static final Primary SUPERCLASS_NAME = new PrimaryVariable(new PretendVariable("Object", true));
	private static final UnarySend SUPERCLASS_NAME_UNARY_SEND = new UnarySend(SUPERCLASS_NAME);
	private static final Method DOIT_METHOD = new ScriptDoItMethod();
	private static final Token SUBCLASS_KEYWORD = new PretendToken("subclass:");
	private static final Token INSTANCE_VARIABLE_NAMES_KEYWORD = new PretendToken("instanceVariableNames:");
	private static final Token CLASS_VARIABLE_NAMES_KEYWORD = new PretendToken("classVariableNames:");
	private static final Token POOL_DICTIONARIES_KEYWORD = new PretendToken("poolDictionaries:");
	private static final Token CATEGORY_KEYWORD = new PretendToken("category:");
	private static final KeywordSend INSTANCE_VARIABLE_NAMES_KEYWORDSEND = new KeywordSend(INSTANCE_VARIABLE_NAMES_KEYWORD);
	private static final KeywordSend CLASS_VARIABLE_NAMES_KEYWORDSEND = new KeywordSend(CLASS_VARIABLE_NAMES_KEYWORD);
	private static final KeywordSend POOL_DICTIONARIES_KEYWORDSEND = new KeywordSend(POOL_DICTIONARIES_KEYWORD);
	private static final KeywordSend CATEGORY_KEYWORDSEND = new KeywordSend(CATEGORY_KEYWORD);
	private static final Primary EMPTY_STRING_VARIABLE = new PrimaryLiteral(new PretendStringLiteral(""));
	private static final UnarySend EMPTY_STRING_PRIMARY_VARIABLE = new UnarySend(EMPTY_STRING_VARIABLE);
	static {
		INSTANCE_VARIABLE_NAMES_KEYWORDSEND.add(EMPTY_STRING_PRIMARY_VARIABLE);
		CLASS_VARIABLE_NAMES_KEYWORDSEND.add(EMPTY_STRING_PRIMARY_VARIABLE);
		POOL_DICTIONARIES_KEYWORDSEND.add(EMPTY_STRING_PRIMARY_VARIABLE);
		CATEGORY_KEYWORDSEND.add(EMPTY_STRING_PRIMARY_VARIABLE);
	}

	public ScriptClassFileGenerator(ParsedSource parsedSource, ScriptListener scriptListener) {
		super(parsedSource, scriptListener);
		parsedSource.isScript(true);
		parsedSource.add(scriptClassDefinition());
		parsedSource.add(DOIT_METHOD);
	}

	private ClassDefinition scriptClassDefinition() {
		String subclassName = filenameWithoutExtension();
		KeywordSend subclassKeywordSend = new KeywordSend(SUBCLASS_KEYWORD);
		subclassKeywordSend.add(new UnarySend(new PrimaryLiteral(new PretendSymbolLiteral(subclassName))));
		ClassDefinition script = new ClassDefinition(SUPERCLASS_NAME_UNARY_SEND);
		script.add(subclassKeywordSend);
		script.add(INSTANCE_VARIABLE_NAMES_KEYWORDSEND);
		script.add(CLASS_VARIABLE_NAMES_KEYWORDSEND);
		script.add(POOL_DICTIONARIES_KEYWORDSEND);
		script.add(CATEGORY_KEYWORDSEND);
		return script;
	}

	public void execute() {
		super.execute();
	}

	public static class ScriptDoItMessagePattern extends MessagePattern {
		public ScriptDoItMessagePattern() { super(null); }
		public int lineNumber() { return 0; }
		public String pattern() { return "doIt"; }
		public int argumentCount() { return 0; }
	}

	public static class ScriptDoItMethod extends Method {
		private final MessagePattern messagePattern = new ScriptDoItMessagePattern();
		public MessagePattern messagePattern() { return messagePattern; }
	}

	public static class PretendStringLiteral extends StringLiteral {

		private final String value;

		public PretendStringLiteral(String value) {
			super(null);
			this.value = value;
		}

		public String toString() { return value; }
		public int lineNumber() { return 0; }
	}

	public static class PretendSymbolLiteral extends SymbolLiteral {

		private final String value;

		public PretendSymbolLiteral(String value) {
			super(null);
			this.value = value;
		}

		public String toString() { return value; }
		public int lineNumber() { return 0; }
	}

	public static class PretendVariable extends Variable {

		private final String value;
		private final boolean isClass;

		public PretendVariable(String value, boolean isClass) {
			super(null);
			this.value = value;
			this.isClass = isClass;
		}

		public String toString() { return value; }
		public boolean isClass() { return isClass; }
		public int lineNumber() { return 0; }
	}

	private static class PretendToken extends Token {

		private final String value;

		public PretendToken(String value) {
			this.value = value;
			super.beginLine = 0;
		}

		public String toString() { return value; }
	}
}
