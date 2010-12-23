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
*/
package st.redline.smalltalk.interpreter;

import st.redline.smalltalk.Smalltalk;

public class Analyser implements NodeVisitor {

	private final Smalltalk smalltalk;

	public Analyser(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public void visit(Program program) {
		program.sequence().accept(this);
	}

	public void visit(Sequence sequence) {
		sequence.statements().accept(this);
	}

	public void visit(Statements statements) {
		statements.statementList().accept(this);
	}

	public void visit(StatementList statementList) {
		statementList.eachAccept(this);
	}

	public void visit(Expression expression) {
		expression.cascade().accept(this);
	}

	public void visit(Cascade cascade) {
		cascade.messageSend().accept(this);
	}

	public void visit(MessageSend messageSend) {
		messageSend.unaryMessageSend().accept(this);
	}

	public void visit(UnaryMessageSend unaryMessageSend) {
		unaryMessageSend.primary().accept(this);
		unaryMessageSend.eachAccept(this);
	}

	public void visit(Primary primary) {
		System.out.println(primary);
	}

	public void visit(UnaryMessage unaryMessage) {
		System.out.println(unaryMessage);
	}
}
