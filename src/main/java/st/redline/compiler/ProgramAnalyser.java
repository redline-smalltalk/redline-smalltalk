/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.HashMap;
import java.util.Map;

class ProgramAnalyser implements AnalyserDelegate {

	private final ClassBytecodeWriter writer;
	private Map<String, Integer> temporariesRegistry;
	private int temporariesIndex = 0;

	ProgramAnalyser(String className, String packageName, boolean verbose) {
		this(new ClassBytecodeWriter(className, packageName, verbose));
	}

	ProgramAnalyser(ClassBytecodeWriter classBytecodeWriter) {
		writer = classBytecodeWriter;
	}

	ClassBytecodeWriter classBytecodeWriter() {
		return writer;
	}

	int temporariesIndex() {
		return temporariesIndex;
	}

	Map<String, Integer> temporariesRegistry() {
		return temporariesRegistry;
	}

	public byte[] classBytes() {
		return writer.contents();
	}

	public void visitBegin(Program program) {
		writer.openClass();
	}

	public void visitEnd(Program program) {
		writer.closeClass();
	}

	public void visitBegin(Temporaries temporaries) {
		initializeTemporariesRegistration();
		writer.invokeContextTemporariesInit(temporaries.size());
	}

	void initializeTemporariesRegistration() {
		temporariesIndex = 0;
		temporariesRegistry = new HashMap<String, Integer>();
	}

	public void visitEnd(Temporaries temporaries) {
		// we don't take any action when visiting end of temporaries.
	}

	public void visitBegin(Statements statements) {
		// we don't take any action when visiting statements.
	}

	public void visitEnd(Statements statements) {
		// we don't take any action when visiting statements.
	}

	public void visit(Temporary temporary, String value, int line) {
		// todo.jcl - output a warning if registered twice?
		temporariesRegistry.put(value, temporariesIndex++);
	}

	public void visit(PrimaryExpression primaryExpression, int line) {
	}

	public void visit(PrimaryStatements primaryStatements, int line) {
	}

	public void visitBegin(SimpleExpression simpleExpression) {
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		if (simpleExpression.isResultDuplicatedOnStack())
			writer.pushDuplicate();
		if (!simpleExpression.isResultLeftOnStack())
			writer.pop();
	}

	public void visitBegin(Cascade cascade) {
		writer.pushDuplicate();
	}

	public void visitEnd(Cascade cascade) {
		writer.pop();
	}

	public void visitBegin(UnaryExpression unaryExpression) {
	}

	public void visitEnd(UnaryExpression unaryExpression) {
	}

	public void visitBegin(BinaryExpression binaryExpression) {
	}

	public void visitEnd(BinaryExpression binaryExpression) {
	}

	public void visitBegin(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
	}

	public void visitEnd(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
	}

	public void visitBegin(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
	}

	public void visitBegin(AssignmentExpression assignmentExpression) {
	}

	public void visitEnd(AssignmentExpression assignmentExpression) {
	}

	public void visitBegin(Array array) {
	}

	public void visitEnd(Array array) {
	}

	public void visitBegin(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
	}

	public void visitBegin(Block block, int line) {
	}

	public void visitEnd(Block block, int line) {
	}

	public void visitBegin(BlockArguments blockArguments, int argumentCount) {
	}

	public void visitEnd(BlockArguments blockArguments, int argumentCount) {
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(Identifier identifier, String value, int line) {
	}

	public void visit(Number number, String value, int line) {
	}

	public void visit(BlockArgument blockArgument, String value, int line) {
	}

	public void visit(Self self, int line) {
		writer.visitLine(line);
		writer.pushReceiver();
	}

	public void visit(Super aSuper, int line) {
	}

	public void visit(True aTrue, int line) {
		pushPrimObjectField("TRUE", line);
	}

	public void visit(False aFalse, int line) {
		pushPrimObjectField("FALSE", line);
	}

	public void visit(Nil nil, int line) {
		pushPrimObjectField("NIL", line);
	}

	public void visit(JVM jvm, int line) {
	}

	public void visit(ArrayConstant arrayConstant, int line) {
	}

	public void visit(UnarySelector unarySelector, String selector, int line) {
		invokeObjectPerform(selector, 0, line);
	}

	public void visit(BinarySelector binarySelector, String selector, int line) {
		invokeObjectPerform(selector, 1, line);
	}

	public void visit(CharacterConstant characterConstant, String value, int index, int line) {
	}

	public void visit(StringConstant stringConstant, String value, int index, int line) {
	}

	public void visit(Symbol symbol, String value, int index, int line) {
	}

	public void visit(SymbolConstant symbolConstant, String value, int line) {
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String selector, int line) {
		invokeObjectPerform(selector, 0, line);
	}

	void invokeObjectPerform(String selector, int argumentCount, int line) {
		writer.visitLine(line);
		writer.invokeObjectPerform(selector, argumentCount);
	}

	void pushPrimObjectField(String field, int line) {
		writer.visitLine(line);
		writer.pushObjectStaticField(field);
	}
}
