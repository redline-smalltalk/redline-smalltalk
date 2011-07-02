/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline.interpreter;

import st.redline.RMethod;
import st.redline.RObject;
import st.redline.Smalltalk;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class Analyser implements NodeVisitor {

	public static final int START_METHOD_ARGUMENT_OFFSET = 3;

	private final Generator generator;
	private final String className;
	private final String packageName;
	private final String sourceFileExtension;
	private final Map<String, VariableName> classVariableRegistry;
	private final Map<String, VariableName> methodVariableRegistry;
	private final Stack<String> blockContext;

	protected boolean inMethod = false;
	protected boolean inClassMethod = false;
	protected boolean sendIsToSuper = false;
	protected int methodArgumentCount = 0;
	protected int methodTemporariesCount = 0;
	protected String blockName;

	public Analyser(Smalltalk smalltalk, String className, String packageName, String sourceFileExtension, int startingLineNumber) {
		// System.out.println("Analyser() " + className + " " + packageName + " " + sourceFileExtension);
		this.className = className;
		this.packageName = packageName;
		this.sourceFileExtension = sourceFileExtension;
		this.generator = new Generator(smalltalk, startingLineNumber);
		this.classVariableRegistry = new HashMap<String, VariableName>();
		this.methodVariableRegistry = new HashMap<String, VariableName>();
		this.blockContext = new Stack<String>();
	}

	private RObject resolveTargetClass() {
		RObject object = smalltalk().cachedObject0(className);
		if (object != null)
			return object;
		return new NullTargetClass();
	}

	public byte[] classBytes() {
		return generator().build();
	}

	private Generator generator() {
		return generator;
	}

	private Smalltalk smalltalk() {
		return Smalltalk.instance();
	}

	public void visit(Program program) {
		// System.out.println("visit(Program) " + packageName);
		generator().openClass(className, packageName, sourceFileExtension);
	}

	public void visitEnd(Program program) {
		// System.out.println("visitEnd(Program program)");
		generator().closeClass();
	}

	public void visit(Temporaries temporaries) {
		// System.out.println("visit(Temporaries temporaries)");
		methodTemporariesCount = temporaries.size();
		temporaries.indexFrom(START_METHOD_ARGUMENT_OFFSET + methodArgumentCount);
		// 0 = this.
		// 1 = receiver.
		// 2 = class method was found in.
	}

	public void visitEnd(Temporaries temporaries) {
		// System.out.println("visitEnd(Temporaries temporaries)");
	}

	public void visit(Temporary temporary, int index, String value, int line) {
		// System.out.println("visit(Temporary) " + value + " @ " + temporary.index);
		registerVariable(temporary, false);
	}

	public void visit(VariableName variableName, String value, int line) {
		// System.out.println("visit(VariableName) " + value + " @ " + line);
		VariableName reference = variableLookup(value, variableName);
		if (reference == null) {
			if (variableName.isClassReference())
				generator().classLookup(value, line);
			else
				throw new IllegalStateException("Reference of undefined variable or temporary '" + value + "'.");
		} else {
			if (reference.isField())
				handleOperationOnClassField(variableName.isOnLoadSideOfExpression(), reference);
			else
				handleOperationOnLocalVariable(variableName.isOnLoadSideOfExpression(), reference);
		}
	}

	public void visit(Statements statements) {
		// System.out.println("visit(Statements statements)");
	}

	public void visitEnd(Statements statements) {
		// System.out.println("visitEnd(Statements statements)");
	}

	public void visit(AnswerExpression answerExpression) {
		// System.out.println("visit(AnswerExpression answerExpression)");
	}

	public void visit(Methods methods) {
		// System.out.println("visit(Methods)");
		inMethod = true;
	}

	public void visitEnd(Methods methods) {
		// System.out.println("visitEnd(Methods)");
		inMethod = false;
	}

	public void visit(InstanceMethod instanceMethod) {
		// System.out.println("visit(InstanceMethod instanceMethod)");
		inClassMethod = false;
		initializePerMethodItems();
	}

	public void initializePerMethodItems() {
		methodArgumentCount = 0;
		methodTemporariesCount = 0;
		methodVariableRegistry.clear();
	}

	public void visitEnd(InstanceMethod instanceMethod) {
		// System.out.println("visitEnd(InstanceMethod instanceMethod)");
		Generator generator = generator();
		if (instanceMethod.isEmpty())
			generator.pushReceiver();
		generator.closeMethod();
		generator.closeMethodClass();
	}

	public void visit(ClassMethod classMethod) {
		// System.out.println("visit(ClassMethod classMethod)");
		inClassMethod = true;
		initializePerMethodItems();
	}

	public void visitEnd(ClassMethod classMethod) {
		// System.out.println("visitEnd(ClassMethod classMethod)");
		Generator generator = generator();
		if (classMethod.isEmpty())
			generator.pushReceiver();
		generator.closeMethod();
		generator.closeMethodClass();
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
		// System.out.println("visit(UnarySelectorMessagePattern) " + value);
		generator().openMethod(0);
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
		// System.out.println("visit(BinarySelectorMessagePattern) " + binarySelector);
		registerVariable(variableName, false);
		methodArgumentCount = 1;
		generator().openMethod(1);
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
		// System.out.println("visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames)");
		registerVariables(variableNames);
		methodArgumentCount = variableNames.size();
		generator().openMethod(methodArgumentCount);
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
		// System.out.println("visit(UnarySelector) " + value);
		generator().unarySend(value, line, sendIsToSuper);
		sendIsToSuper = false;
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
		// System.out.println("visit(BinarySelector binarySelector, String value, int line)");
	}

	public void visit(Keyword keyword, String value, int line) {
		// System.out.println("visit(Keyword keyword, String value, int line)");
	}

	public void visit(AssignmentExpression assignmentExpression) {
		// System.out.println("visit(AssignmentExpression assignmentExpression)");
	}

	public void visit(SimpleExpression simpleExpression) {
		// System.out.println("visit(SimpleExpression simpleExpression)");
		sendIsToSuper = false;
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		// System.out.println("visitEnd(SimpleExpression simpleExpression)");
		// System.out.println("visitEnd(SimpleExpression)");
		if (simpleExpression.isResultDuplicatedOnStack())
			generator().pushStackTop();
		if (!simpleExpression.isResultLeftOnStack())
			generator().stackPop();
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		// System.out.println("visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line)");
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
		// System.out.println("visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription)");
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions) {
		// System.out.println("visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions)");
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
		// System.out.println("visit(UnaryObjectDescription unaryObjectDescription)");
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
		// System.out.println("visit(BinaryObjectDescription binaryObjectDescription)");
	}

	public void visit(UnaryExpression unaryExpression) {
		// System.out.println("visit(UnaryExpression unaryExpression)");
	}

	public void visitEnd(UnaryExpression unaryExpression) {
		// System.out.println("visitEnd(UnaryExpression unaryExpression)");
	}

	public void visit(BinaryExpression binaryExpression) {
		// System.out.println("visit(BinaryExpression binaryExpression)");
	}

	public void visit(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		// System.out.println("visit(KeywordExpression) " + keywords);
	}

	public void visitEnd(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		// System.out.println("visitEnd(KeywordExpression) " + keywords);
		generator().keywordSend(keywords, argumentCount, line, sendIsToSuper);
		sendIsToSuper = false;
	}

	public void visit(PrimaryExpression primaryExpression) {
		// System.out.println("visit(PrimaryExpression primaryExpression)");
	}

	public void visit(PrimaryStatements primaryStatements) {
		// System.out.println("visit(PrimaryStatements primaryStatements)");
	}

	public void visit(Primitive primitive, String value, int line) {
		// System.out.println("visit(Primitive) " + value);
		generator().callToPrimitiveByNumber(methodArgumentCount, methodTemporariesCount, value, line);
	}

	public void visit(Symbol symbol, String value, int line) {
		// System.out.println("visit(Symbol symbol, String value, int line)");
	}

	public void visit(Array array) {
		// System.out.println("visit(Array array)");
	}

	public void visit(Identifier identifier, String value, int line) {
		// System.out.println("visit(Identifier identifier, String value, int line)");
	}

	public void visit(LiteralSymbol literalSymbol, String value, int line) {
		// System.out.println("visit(LiteralSymbol) " + literalSymbol.value);
		generator().primitiveSymbolConversion(value, line);
	}

	public void visit(LiteralArray literalArray) {
		// System.out.println("visit(LiteralArray literalArray)");
	}

	public void visit(ArrayConstantElement arrayConstantElement) {
		// System.out.println("visit(ArrayConstantElement arrayConstantElement)");
	}

	public void visit(CharacterConstant characterConstant, String value, int line) {
		// System.out.println("visit(CharacterConstant characterConstant, String value, int line)");
	}

	public void visit(StringConstant stringConstant, String value, int line) {
		// System.out.println("visit(StringConstant stringConstant, String value, int line)");
	}

	public void visit(StringChunk stringChunk, String value, int line) {
		// System.out.println("visit(StringChunk) " + value);
		generator().primitiveStringChunkConversion(value, line);
	}

	public void visit(LiteralString literalString, String value, int line) {
		// System.out.println("visit(LiteralString) " + value);
		generator().primitiveStringConversion(value, line);
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
		// System.out.println("visit(LiteralCharacter literalCharacter, String value, int line)");
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
		// System.out.println("visit(NumberConstant)" + value + " @ " + line);
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
		// System.out.println("visit(LiteralNumber) " + value + " @ " + line);
		generator().primitiveNumberConversion(value, line);
	}

	public void visit(Block block) {
		// System.out.println("visit(Block block)");
		blockName = className + "_Block" + String.valueOf(block.hashCode());
		blockContext.push(blockName);
		Generator generator = generator();
		generator.openBlockClass(blockName, packageName, sourceFileExtension);
		generator.openBlock(methodArgumentCount, block.hasStatements());
	}

	public void visitEnd(Block block) {
		// System.out.println("visitEnd(Block block)");
		Generator generator = generator();
		generator.closeBlock();
		byte[] blockClass = generator.closeBlockClass();
		smalltalk().defineClass(blockClass, false);
		generator.createBlock(blockName, packageName, sourceFileExtension, methodArgumentCount);
		blockName = blockContext.pop();
	}

	public void visit(SelfReservedWord selfReservedWord, int line) {
		// System.out.println("visit(self)");
		generator().pushReceiver();
	}

	public void visit(SuperReservedWord superReservedWord, int line) {
		// System.out.println("visit(super)");
		if (!inMethod)
			throw new IllegalStateException("Can't reference super outside of method.");
		sendIsToSuper = true;
		generator().pushForSuperCall(line);
	}

	public void visit(TrueReservedWord selfReservedWord, int line) {
		// System.out.println("visit(true)");
		generator().trueLookup(line);
	}

	public void visit(FalseReservedWord selfReservedWord, int line) {
		// System.out.println("visit(false)");
		generator().falseLookup(line);
	}

	public void visit(NilReservedWord selfReservedWord, int line) {
		// System.out.println("visit(nil)");
		generator().nilLookup(line);
	}

	public void registerVariables(List<VariableName> variableNames) {
		for (VariableName variableName : variableNames)
			registerVariable(variableName, false);
	}

	public void registerVariable(VariableName variableName, boolean isClassField) {
		// System.out.println("registerVariable " + variableName.value + " @ " + variableName.index + " " + isClassField);
		if (methodVariableRegistry.containsKey(variableName.value) || classVariableRegistry.containsKey(variableName.value))
			throw new IllegalStateException("Variable '" + variableName.value + "' already defined. Could be a Class field?");
		if (isClassField)
			classVariableRegistry.put(variableName.value, variableName);
		else
			methodVariableRegistry.put(variableName.value, variableName);
	}

	private VariableName variableLookup(String name, VariableName variable) {
		VariableName foundVariableName = methodVariableRegistry.get(name);
		if (foundVariableName != null)
			return foundVariableName;
		foundVariableName = classVariableRegistry.get(name);
		if (foundVariableName != null)
			return foundVariableName;
		return probeTargetClassVariables(name);
	}

	private VariableName probeTargetClassVariables(String name) {
		// System.out.println("probeTargetClassVariables() " + name);
		RObject target = resolveTargetClass();
		if (target.primitiveHasInstanceVariableNamed(name))
			return new InstanceVariableName(name, 0);
		if (target.primitiveHasClassVariableNamed(name))
			return new ClassVariableName(name, 0);
		if (target.primitiveHasClassInstanceVariableNamed(name))
			return new ClassInstanceVariableName(name, 0);
		if (target.primitiveHasPoolNamed(name))
			return new PoolVariableName(name, 0);
		return null;
	}

	private void handleOperationOnClassField(boolean onLoadSideOfExpression, VariableName reference) {
	//        I    C    Ci
	//   CM   N    Y    Y
	//   IM   Y    Y    N
		if (inClassMethod) {
			if (reference.isInstanceField()) {
				throw new IllegalStateException("Can't access instance variable from Class method.");
			} else if (reference.isClassField()) {
				if (onLoadSideOfExpression)
					generator().loadFromInstanceField(reference.value());
				else
					generator().storeIntoInstanceField(reference.value());
			} else if (reference.isClassInstanceField()) {
				if (onLoadSideOfExpression)
					generator().loadFromClassInstanceField(reference.value());
				else
					generator().storeIntoClassInstanceField(reference.value());
			}
		} else {
			if (reference.isInstanceField()) {
				if (onLoadSideOfExpression)
					generator().loadFromInstanceField(reference.value());
				else
					generator().storeIntoInstanceField(reference.value());
			} else if (reference.isClassField()) {
				if (onLoadSideOfExpression)
					generator().loadFromClassField(reference.value());
				else
					generator().storeIntoClassField(reference.value());
			} else if (reference.isClassInstanceField()) {
				throw new IllegalStateException("Can't access Class instance variable from instance method.");
			}
		}
	}

	private void handleOperationOnLocalVariable(boolean onLoadSideOfExpression, VariableName reference) {
		if (onLoadSideOfExpression)
			generator().loadFromLocal(reference.index);
		else
			generator().storeIntoLocal(reference.index);
	}

	private class NullTargetClass extends RObject {
		public boolean primitiveHasPoolNamed(String variable) { return false; }
		public boolean primitiveHasClassInstanceVariableNamed(String name)  { return false; }
		public boolean primitiveHasClassVariableNamed(String variable) { return false; }
		public boolean primitiveHasInstanceVariableNamed(String variable) { return false; }
	}
}
