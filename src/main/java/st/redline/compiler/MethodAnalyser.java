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
package st.redline.compiler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MethodAnalyser extends Analyser {

	private final Map<String, VariableName> argumentRegistry;

	public MethodAnalyser(String className, String packageName, int countOfArguments) {
		super(className, packageName, countOfArguments);
		argumentRegistry = new HashMap<String, VariableName>();
	}

	protected void initialize() {
		classBytecodeWriter = new MethodBytecodeWriter(className, packageName, countOfArguments);
	}

	public int methodTemporariesCount() {
		return 0;
	}

	public boolean continueMethodVisit() {
		return true;
	}

	protected boolean isMethodArgument(String value) {
		return argumentRegistry.containsKey(value);
	}

	protected void loadMethodArgument(String value) {
		VariableName variableName = argumentRegistry.get(value);
		classBytecodeWriter.stackPushLocal(variableName.index + 3);  // 0 = method, 1 = receiver, 2 = class method found in, then local fields.
	}

	public void visit(InstanceMethod instanceMethod) {
		classBytecodeWriter.openClass();
	}

	public void visitEnd(InstanceMethod instanceMethod) {
		if (instanceMethod.isEmpty())
			classBytecodeWriter.stackPushReceiver(instanceMethod.line());
		classBytecodeWriter.closeClass();
	}

	public void visit(ClassMethod classMethod) {
		classBytecodeWriter.openClass();
	}

	public void visitEnd(ClassMethod classMethod) {
		if (classMethod.isEmpty())
			classBytecodeWriter.stackPushReceiver(classMethod.line());
		classBytecodeWriter.closeClass();
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
		registerMethodArgument(variableName);
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
		registerMethodArguments(variableNames);
	}

	private void registerMethodArguments(List<VariableName> variableNames) {
		for (VariableName variableName : variableNames)
			registerMethodArgument(variableName);
	}

	private void registerMethodArgument(VariableName variableName) {
		String name = variableName.value();
		if (isMethodArgument(name))
			throw new IllegalStateException("Duplicate method argument name '" + name + "'.");
		argumentRegistry.put(name, variableName);
	}
}
