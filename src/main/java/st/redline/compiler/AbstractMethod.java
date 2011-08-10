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

public abstract class AbstractMethod implements Method {

	protected final String objectName;
	protected final MessagePattern messagePattern;
	protected final Primitive primitive;
	protected final Temporaries temporaries;
	protected final Statements statements;
	protected final boolean empty;

	public AbstractMethod(String objectName, MessagePattern messagePattern, Primitive primitive, Temporaries temporaries, Statements statements) {
		this.objectName = objectName;
		this.messagePattern = messagePattern;
		this.primitive = primitive;
		this.temporaries = temporaries;
		this.statements = statements;
		this.empty = primitive == null && statements == null;
	}

	public boolean isEmpty() {
		return empty;
	}

	public String objectName() {
		return objectName;
	}

	public int line() {
		return messagePattern.line();
	}

	public void accept(NodeVisitor visitor) {
		messagePattern.accept(visitor);
		// we don't visit the rest during class analysis, but we do during method analysis.
		if (!visitor.continueMethodVisit())
			return;
		if (primitive != null)
			primitive.accept(visitor);
		if (temporaries != null)
			temporaries.accept(visitor);
		if (statements != null)
			statements.accept(visitor);
	}
}
