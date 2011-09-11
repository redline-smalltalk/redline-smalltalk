/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Methods implements VisitableNode {

	private final List<Method> methods;

	public Methods() {
		methods = new ArrayList<Method>();
	}

	public boolean isEmpty() {
		return methods.isEmpty();
	}

	public void add(Method method) {
		methods.add(method);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		for (Method method : methods)
			method.accept(visitor);
		visitor.visitEnd(this);
	}
}
