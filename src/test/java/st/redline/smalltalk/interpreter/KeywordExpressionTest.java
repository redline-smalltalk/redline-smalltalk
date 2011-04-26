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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static junit.framework.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class KeywordExpressionTest {

	@Mock NodeVisitor visitor;
	@Mock BinaryObjectDescription binaryObjectDescription;
	private Keyword keyword;
	private KeywordExpression keywordExpression;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		keyword = new Keyword("at:", 10);
		keywordExpression = new KeywordExpression();
		keywordExpression.add(keyword, binaryObjectDescription);
	}

	@Test public void shouldVisitKeywordExpression() {
		keywordExpression.accept(visitor);
		verify(visitor).visit(keywordExpression, "at:", 1, 10);
		verify(binaryObjectDescription).accept(visitor);
	}

	@Test public void shouldEndVisitation() {
		keywordExpression.accept(visitor);
		verify(visitor).visitEnd(keywordExpression, "at:", 1, 10);
	}

	@Test public void shouldKnowWhenExpressionDefinesClass() {
		keywordExpression.add(new Keyword("subclass:", 10), binaryObjectDescription);
		keywordExpression.add(new Keyword("instanceVariableNames:", 10), binaryObjectDescription);
		keywordExpression.add(new Keyword("classVariableNames:", 10), binaryObjectDescription);
		keywordExpression.add(new Keyword("poolDictionaries:", 10), binaryObjectDescription);
		keywordExpression.accept(visitor);
		assertTrue(keywordExpression.definesClassFields());
	}
}
