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

import java.util.ArrayList;
import java.util.List;

public class KeywordExpression implements MessageExpression {

	private final List<BinaryObjectDescription> binaryObjectDescriptions;
	private final StringBuffer keywords;
	private int line = -1;
	private boolean monitoringSubclassFields = false;
	private boolean definesClassFields = false;
	private List<VariableName> instanceVariableNames;
	private List<VariableName> classVariableNames;
	private List<VariableName> poolDictionaries;

	public KeywordExpression() {
		binaryObjectDescriptions = new ArrayList<BinaryObjectDescription>();
		keywords = new StringBuffer();
	}

	public void add(Keyword keyword, BinaryObjectDescription binaryObjectDescription) {
		if (line == -1)
			line = keyword.line;
		keywords.append(keyword.value);
		binaryObjectDescriptions.add(binaryObjectDescription);
		if (monitoringSubclassFields)
			monitorSubclassField(keyword.value, binaryObjectDescription);
		else
			monitoringSubclassFields = keyword.value.startsWith("subclass:");
	}

	private void monitorSubclassField(String keyword, BinaryObjectDescription binaryObjectDescription) {
		if (instanceVariableNames == null && keyword.startsWith("instanceVariableNames:")) {
			definesClassFields = true;
			instanceVariableNames = binaryObjectDescription.toVariableNames();
		} else if (classVariableNames == null && keyword.startsWith("classVariableNames:")) {
			definesClassFields = true;
			classVariableNames = binaryObjectDescription.toVariableNames();
		} else if (poolDictionaries == null && keyword.startsWith("poolDictionaries:")) {
			definesClassFields = true;
			poolDictionaries = binaryObjectDescription.toVariableNames();
		}
	}

	public List<VariableName> instanceVariableNames() {
		return instanceVariableNames;
	}

	public List<VariableName> classVariableNames() {
		return classVariableNames;
	}

	public List<VariableName> poolDictionaries() {
		return poolDictionaries;
	}

	public boolean definesClassFields() {
		return definesClassFields;
	}

	public void accept(NodeVisitor nodeVisitor) {
		String keywords = this.keywords.toString();
		nodeVisitor.visit(this, keywords, binaryObjectDescriptions.size(), line);
		for (BinaryObjectDescription binaryObjectDescription : binaryObjectDescriptions)
			binaryObjectDescription.accept(nodeVisitor);
		nodeVisitor.visitEnd(this, keywords, binaryObjectDescriptions.size(), line);
	}
}
