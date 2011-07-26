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
package st.redline;

public class Bootstrapper {

	private final ProtoObject protoObject;
	private final ProtoObject protoObjectMetaclass;
	private final ProtoObject symbol;
	private final ProtoObject symbolMetaclass;
	private final ProtoObject metaclass;

	protected Bootstrapper(ProtoObject protoObject) {
		this.protoObject = protoObject;
		this.protoObjectMetaclass = protoObject;
		this.symbol = new ProtoObject();
		this.symbolMetaclass = new ProtoObject(false);
		this.metaclass = new ProtoObject();
	}

	public void bootstrap() {
		associateClasses();
		registerClasses();
	}

	private void markBootstrappedClasses() {
		metaclass.bootstrapped();
		symbol.bootstrapped();
	}

	private void associateClasses() {
		protoObjectMetaclass.cls(metaclass);
		protoObject.cls(protoObjectMetaclass);
		symbolMetaclass.cls(metaclass);
		symbol.cls(symbolMetaclass);
	}

	private void registerClasses() {
		ProtoObject.primitiveRegisterAs(protoObject, "ProtoObject");
		ProtoObject.primitiveRegisterAs(symbol, "Symbol");
	}
}
