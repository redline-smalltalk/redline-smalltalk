package st.redline;

import java.io.*;
import st.redline.compiler.*;

public class Script {

	private static final String DEFAULT_OUTPUT_FOLDER = "out";

	private final java.lang.String scriptPath;
	private final java.lang.String outputPath;
	private final ScriptListener scriptListener;

	private java.lang.String contents;
	private ParserFactory parserFactory;
	private GeneratorFactory generatorFactory;
	private ParsedSource parsedSource;

	public static void main(java.lang.String[] args) {
		Script.fromFile(args[0]).doIt();
	}

	public static Script fromFile(java.lang.String scriptFilename) {
		return Script.fromFile(scriptFilename, DEFAULT_OUTPUT_FOLDER, new NullScriptListener());
	}

	public static Script fromFile(java.lang.String scriptFilename, java.lang.String outputPath, ScriptListener scriptListener) {
		return Script.fromFile(new ScriptFile(scriptFilename), outputPath, scriptListener);
	}

	public static Script fromFile(ScriptFile scriptFile, java.lang.String outputPath, ScriptListener scriptListener) {
		return new Script(scriptFile, outputPath, scriptListener);
	}

	public Script(ScriptFile scriptFile, java.lang.String outputpath, ScriptListener scriptListener) {
		this(scriptFile.contents(), scriptFile.getAbsolutePath(), outputpath, scriptListener);
	}

	public Script(java.lang.String script, java.lang.String scriptPath, java.lang.String outputPath, ScriptListener scriptListener, ParserFactory parserFactory, GeneratorFactory generatorFactory) {
		contents = script;
		this.scriptPath = scriptPath;
		this.outputPath = outputPath;
		this.scriptListener = scriptListener;
		this.parserFactory = parserFactory;
		this.generatorFactory = generatorFactory;
	}

	public Script(java.lang.String script, java.lang.String scriptPath, java.lang.String outputPath, ScriptListener scriptListener) {
		this(script, scriptPath, outputPath, scriptListener, new ParserFactory(), new GeneratorFactory());
	}

	public void doIt() {
		preprocess();
		parse();
		execute();
	}

	private void parse() {
		Parser parser = createParser();
		try {
			notifyStartParse();
			parsedSource = (ParsedSource) parser.parse();
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private void notifyStartParse() {
		if (scriptListener.isEnabled())
			scriptListener.parsing(scriptPath);
	}

	protected Parser createParser() {
		return parserFactory.parserOn(contentsAsInputStream(), scriptPath, outputPath);
	}

	private void execute() {
		executor().execute();
	}

	private Executor executor() {
		return generatorFactory.generatorFor(parsedSource, scriptListener);
	}

	private InputStream contentsAsInputStream() {
		return new ByteArrayInputStream(contents.getBytes());
	}

	private void preprocess() {
		contents = new Preprocessor(contents).preprocess();
	}

	private static class NullScriptListener implements ScriptListener {
		public boolean isEnabled() { return false; }
		public void parsing(String scriptPath) {}
		public void generated(String fullClassName, String outputPath) {}
	}
}