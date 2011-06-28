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
package st.redline.bootstrap;

import st.redline.RMethod;
import st.redline.RObject;
import st.redline.Smalltalk;

public class ClassBuilderBuildSubclassMethod extends RMethod {

	public RObject applyToWith(RObject receiver, RObject classMethodWasFoundIn, RObject superclass, RObject subclass, RObject instanceVariableNames, RObject classVariableNames,
							   RObject classInstanceVariableNames, RObject poolDictionaries, RObject category) {
		// This is where we build a class.
		// we are doing the minimum to get us moving.
		System.out.println("** BUILDING CLASS ** superclass: " + superclass + " subclass: " + subclass + " [Method was found in " + classMethodWasFoundIn + "]");
		subclass = send0(subclass, Bootstrapper.INITIALIZE_CLASSINSTVARS_SELECTOR);
		subclass = send0(subclass, Bootstrapper.ADDINSTVAR_SELECTOR, instanceVariableNames);
		subclass = send0(subclass, Bootstrapper.ADDCLASSVAR_SELECTOR, classVariableNames);
		subclass = send0(subclass, Bootstrapper.ADDCLASSINSTVAR_SELECTOR, classInstanceVariableNames);
		subclass = send0(subclass, Bootstrapper.ADDSHAREDPOOL_SELECTOR, poolDictionaries);
		subclass = send0(subclass, Bootstrapper.CATEGORY_SELECTOR, category);
		return subclass;
	}

	private RObject send0(RObject subclass, String addVariableKeyword, RObject variables) {
		String string = variables.primitiveValue().toString();
		if (string.length() == 0)
			return subclass;
		RObject receiver = subclass;
		String[] vars = string.split(" ");
		for (String variable : vars)
			receiver = RObject.send(receiver, smalltalk().stringFromPrimitive(variable), addVariableKeyword, null);
		return receiver;
	}

	private RObject send0(RObject subclass, String selector) {
		return RObject.send(subclass, selector, null);
	}
}
