/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import st.redline.compiler.*;

import java.util.List;

public class Compiler {

	private final SourceFile sourceFile;
	private final boolean verbose;
	private boolean ignoreCompilerErrors;

	public Compiler(SourceFile sourceFile, boolean verbose, boolean ignoreCompilerErrors) {
		this.sourceFile = sourceFile;
		this.verbose = verbose;
		this.ignoreCompilerErrors = ignoreCompilerErrors;
	}

	protected byte[] compile() {
		return compileSource(preprocessSource());
	}

	private byte[] compileSource(Source source) {
		Program program = parse(source.source());
		Analyser analyser = analyse(program);
		return analyser.classBytes();
	}

	private Analyser analyse(Program program) {
		Analyser analyser = analyser();
		program.accept(analyser);
		return analyser;
	}

	private Analyser analyser() {
		return new Analyser(sourceFile.shortName(), sourceFile.packageName(), verbose);
	}

	private Program parse(String sourceCode) {
		SmalltalkLexer smalltalkLexer = lexorOn(sourceCode);
		SmalltalkParser smalltalkParser = parserUsing(smalltalkLexer);
		try {
			if (!ignoreCompilerErrors && !smalltalkLexer.getExceptions().isEmpty())
				throw RedlineException.withMessage(messageFrom(smalltalkLexer.getExceptions()));
			return smalltalkParser.program();
		} catch (RecognitionException e) {
			throw RedlineException.withCause(e);
		}
	}

	private String messageFrom(List<RecognitionException> recognitionExceptions) {
		StringBuffer messages = new StringBuffer();
		for (RecognitionException recognitionException : recognitionExceptions)
			messages.append(recognitionException.toString()).append("\n");
		return messages.toString();
	}

	private SmalltalkParser parserUsing(SmalltalkLexer smalltalkLexer) {
		return new SmalltalkParser(new CommonTokenStream(smalltalkLexer));
	}

	private SmalltalkLexer lexorOn(String sourceCode) {
		return new SmalltalkLexer(new ANTLRStringStream(sourceCode));
	}

	private Source preprocessSource() {
		return preprocessor().parse(sourceFile);
	}

	private Preprocessor preprocessor() {
		return new Preprocessor();
	}
}
