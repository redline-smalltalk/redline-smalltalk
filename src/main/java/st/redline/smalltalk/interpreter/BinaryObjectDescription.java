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

public class BinaryObjectDescription implements VisitableNode {

	private final Primary primary;
	private final List<UnarySelector> unarySelectors;
	private final List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions;

	public BinaryObjectDescription(Primary primary) {
		this.primary = primary;
		this.unarySelectors = new ArrayList<UnarySelector>();
		this.binarySelectorUnaryObjectDescriptions = new ArrayList<BinarySelectorUnaryObjectDescription>();
	}

	public void add(UnarySelector unarySelector) {
		unarySelectors.add(unarySelector);
	}

	public void add(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		add(new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription));
	}

	protected void add(BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription) {
		binarySelectorUnaryObjectDescriptions.add(binarySelectorUnaryObjectDescription);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		primary.accept(visitor);
		for (UnarySelector unarySelector : unarySelectors)
			unarySelector.accept(visitor);
		for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions)
			binarySelectorUnaryObjectDescription.accept(visitor);
	}

	// todo.jcl - work out how to use generics here.

	public List<InstanceVariableName> toInstanceVariableNames() {
		int line = primary.line();
		String[] names = primary.value().split(" ");
		List<InstanceVariableName> variableNames = new ArrayList<InstanceVariableName>();
		for (String name : names)
			variableNames.add(new InstanceVariableName(name, line));
		return variableNames;
	}

	public List<ClassVariableName> toClassVariableNames() {
		int line = primary.line();
		String[] names = primary.value().split(" ");
		List<ClassVariableName> variableNames = new ArrayList<ClassVariableName>();
		for (String name : names)
			variableNames.add(new ClassVariableName(name, line));
		return variableNames;
	}

	public List<PoolVariableName> toPoolVariableNames() {
		int line = primary.line();
		String[] names = primary.value().split(" ");
		List<PoolVariableName> variableNames = new ArrayList<PoolVariableName>();
		for (String name : names)
			variableNames.add(new PoolVariableName(name, line));
		return variableNames;
	}
}
