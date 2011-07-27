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

	private final ProtoObject metaclass;
	private final ProtoObject protoObject;
	private final ProtoObject protoObjectMetaclass;
	private final ProtoObject cls;
	private final ProtoObject clsMetaclass;
	private final ProtoObject symbol;
	private final ProtoObject symbolMetaclass;

	protected Bootstrapper(ProtoObject protoObject) {
		this.protoObject = protoObject;
		this.protoObjectMetaclass = new ProtoObject();
		this.cls = new ProtoObject();
		this.clsMetaclass = new ProtoObject();
		this.symbol = new ProtoObject();
		this.symbolMetaclass = new ProtoObject();
		this.metaclass = new ProtoObject();
	}

	public void bootstrap() {
		markBootstrappedClasses();
		associateClasses();
		registerClasses();
	}

	private void markBootstrappedClasses() {
		metaclass.bootstrapped();
		cls.bootstrapped();
		clsMetaclass.bootstrapped();
		symbol.bootstrapped();
		symbolMetaclass.bootstrapped();
	}

	private void associateClasses() {
		protoObjectMetaclass.cls(metaclass);
		protoObjectMetaclass.superclass(cls);
		protoObject.cls(protoObjectMetaclass);
		cls.cls(clsMetaclass);
		symbolMetaclass.cls(metaclass);
		symbol.cls(symbolMetaclass);
	}

	private void registerClasses() {
		ProtoObject.primitiveRegisterAs(protoObject, "ProtoObject");
		ProtoObject.primitiveRegisterAs(symbol, "Symbol");
	}
}
