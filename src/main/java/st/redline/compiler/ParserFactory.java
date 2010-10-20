package st.redline.compiler;

import java.io.InputStream;
import java.io.InputStreamReader;

public class ParserFactory {

	private static final String PARSER_CLASS_NAME = "st.redline.compiler.SourceParser";

	public Parser parserOn(InputStream methodBodyInputStream, String sourcePath, String outputPath) {
		Parser parser = createParserOn(methodBodyInputStream);
		parser.sourcePath(sourcePath);
		parser.outputPath(outputPath);
		return parser;
	}

	private Parser createParserOn(InputStream methodBodyInputStream) {
		try {
			return parserClass()
					.getConstructor(new Class[] {InputStream.class})
					.newInstance(new Object[] {methodBodyInputStream});
		} catch (Throwable e) {
			throw new IllegalStateException(e);
		}
	}

	private Class<Parser> parserClass() throws ClassNotFoundException {
		return (Class<Parser>) Class.forName(PARSER_CLASS_NAME);
	}
}