/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

class PrimBasicDoesNotUnderstand extends PrimObject {

	protected PrimObject invoke(PrimObject receiver, PrimContext context) {
		if (!"basicDoesNotUnderstand:".equals(context.selector))
			return receiver.perform0("basicDoesNotUnderstand:", copySelectorAndArguments(context));
		outputDoesNotUnderstandError(receiver, context);
		return PRIM_NIL;
	}

	private void outputDoesNotUnderstandError(PrimObject receiver, PrimContext context) {
		System.err.print("Object '" + receiver + "' does not understand '" + context.arguments[0].javaValue + "'");
		if (context.arguments.length > 1) {
			System.err.println(" with arguments:");
			for (PrimObject argument : context.arguments)
				System.err.println(argument);
		}
		System.err.println(".");
	}

	private PrimObject[] copySelectorAndArguments(PrimContext context) {
		PrimObject selectorObject = new PrimObject();
		selectorObject.javaValue(context.selector);
		PrimObject[] argumentsWithSelector = new PrimObject[context.arguments.length + 1];
		argumentsWithSelector[0] = selectorObject;
		System.arraycopy(context.arguments, 0, argumentsWithSelector, 1, context.arguments.length);
		return argumentsWithSelector;
	}
}
