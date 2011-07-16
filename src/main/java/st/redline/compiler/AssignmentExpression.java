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

public class AssignmentExpression implements Expression {

	private final VariableName variableName;
	private final Expression expression;

	public AssignmentExpression(VariableName variableName, Expression expression) {
		this.variableName = variableName;
		this.expression = expression;
	}

	public void leaveResultOnStack() {
		expression.duplicateResultOnStack();
	}

	public void duplicateResultOnStack() {
		throw new IllegalStateException("Assignment asked to duplicate stack top!");
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		expression.leaveResultOnStack();
		expression.accept(visitor);
		variableName.onStoreSideOfExpression();
		variableName.accept(visitor);
	}
}
