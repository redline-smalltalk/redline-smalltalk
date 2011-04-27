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
package st.redline.smalltalk.interpreter;

import st.redline.smalltalk.Smalltalk;
import st.redline.smalltalk.SourceFile;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class AnalyserContexts {

	private final Stack<AnalyserContext> analyserContexts;

	public static AnalyserContexts create(Smalltalk smalltalk, Generator generator) {
		return new AnalyserContexts(smalltalk, generator);
	}

	private AnalyserContexts(Smalltalk smalltalk, Generator generator) {
		analyserContexts = new Stack<AnalyserContext>();
		push(new AnalyserContext(smalltalk, generator));
	}

	public AnalyserContext current() {
		return analyserContexts.peek();
	}

	private void push(AnalyserContext analyserContext) {
		analyserContexts.push(analyserContext);
	}

	public class AnalyserContext {

		private final Smalltalk smalltalk;
		private final Generator generator;

		private String methodClassName;
		private String methodSelector;
		private int methodArgumentCount;
		private int methodTemporariesCount;
		private Map<String, VariableName> methodVariableAndTemporaryRegistry = new HashMap<String, VariableName>();

		public AnalyserContext(Smalltalk smalltalk, Generator generator) {
			this.smalltalk = smalltalk;
			this.generator = generator;
		}

		public byte[] classResult() {
			return generator.classBytes();
		}

		public Generator generator() {
			return generator;
		}

		public String methodClassName() {
			return methodClassName;
		}

		public String methodSelector() {
			return methodSelector;
		}

		public int methodArgumentCount() {
			return methodArgumentCount;
		}

		public int methodTemporariesCount() {
			return methodTemporariesCount;
		}

		public void registerVariables(List<VariableName> variableNames) {
			for (VariableName variableName : variableNames)
				registerVariable(variableName);
		}

		public void registerVariable(VariableName variableName) {
			if (methodVariableAndTemporaryRegistry.containsKey(variableName.value))
				throw new IllegalStateException("Variable '" + variableName.value + "' already defined.");
			methodVariableAndTemporaryRegistry.put(variableName.value, variableName);
		}

		public String sourceFileName() {
			return sourceFile().nameWithoutExtension();
		}

		public SourceFile sourceFile() {
			return smalltalk.currentFile();
		}

		private List<String> sourcePaths() {
			return smalltalk.sourcePaths();
		}

		public String sourceFileParentPathWithoutSourcePaths() {
			String parentPath = sourceFile().parentPathWithoutUserPath();
			for (String path : sourcePaths()) {
				if (parentPath.length() == path.length())
					return "";
				if (parentPath.startsWith(path))
					return parentPath.substring(path.length() + 1);
			}
			return parentPath;
		}

		public void initializePerMethodItems() {
			methodArgumentCount = 0;
			methodTemporariesCount = 0;
			methodVariableAndTemporaryRegistry = new HashMap<String, VariableName>();
		}

		public VariableName variableLookup(String variableName) {
			return methodVariableAndTemporaryRegistry.get(variableName);
		}

		public void methodArgumentCount(int methodArgumentCount) {
			this.methodArgumentCount = methodArgumentCount;
		}

		public void methodTemporariesCount(int methodTemporariesCount) {
			this.methodTemporariesCount = methodTemporariesCount;
		}

		public void methodClassName(String methodClassName) {
			this.methodClassName = methodClassName;
		}

		public void methodSelector(String methodSelector) {
			this.methodSelector = methodSelector;
		}
	}
}
