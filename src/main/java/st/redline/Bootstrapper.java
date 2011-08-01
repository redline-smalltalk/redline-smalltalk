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

import st.redline.bootstrap.ClassSubclassMethod;

import java.io.File;

public class Bootstrapper {

	private final ProtoObject metaClassMetaClass;
	private final ProtoObject metaClass;
	private final ProtoObject protoObject;
	private final ProtoObject protoObjectMetaClass;
	private final ProtoObject object;
	private final ProtoObject objectMetaClass;
	private final ProtoObject behavior;
	private final ProtoObject behaviorMetaClass;
	private final ProtoObject classDescription;
	private final ProtoObject classDescriptionMetaClass;
	private final ProtoObject cls;
	private final ProtoObject clsMetaClass;
	private final ProtoObject string;
	private final ProtoObject stringMetaClass;
	private final ProtoObject symbol;
	private final ProtoObject symbolMetaClass;

	protected Bootstrapper(ProtoObject protoObject) {
		this.protoObject = protoObject;
		this.protoObjectMetaClass = new ProtoObject();
		this.object = protoObject;
		this.objectMetaClass = new ProtoObject();
		this.behavior = new ProtoObject();
		this.behaviorMetaClass = new ProtoObject();
		this.classDescription = new ProtoObject();
		this.classDescriptionMetaClass = new ProtoObject();
		this.cls = new ProtoObject();
		this.clsMetaClass = new ProtoObject();
		this.symbol = new ProtoObject();
		this.symbolMetaClass = new ProtoObject();
		this.string = new ProtoObject();
		this.stringMetaClass = new ProtoObject();
		this.metaClass = new ProtoObject();
		this.metaClassMetaClass = new ProtoObject();
	}

	public void bootstrap() {
		bootstrapClasses();
		bootstrapMethods();
	}

	private void bootstrapMethods() {
		cls.methodAtPut("<", new ClassSubclassMethod());
	}

	private void bootstrapClasses() {
		markBootstrappedClasses();
		associateClasses();
		registerClasses();
		mapPackages();
	}

	private void markBootstrappedClasses() {
		metaClassMetaClass.bootstrapped();
		metaClass.bootstrapped();
		object.bootstrapped();
		objectMetaClass.bootstrapped();
		behavior.bootstrapped();
		behaviorMetaClass.bootstrapped();
		classDescription.bootstrapped();
		classDescriptionMetaClass.bootstrapped();
		cls.bootstrapped();
		clsMetaClass.bootstrapped();
		symbol.bootstrapped();
		symbolMetaClass.bootstrapped();
		string.bootstrapped();
		stringMetaClass.bootstrapped();
	}

	private void associateClasses() {
		protoObjectMetaClass.cls(metaClass);
		protoObjectMetaClass.superclass(cls);
		protoObject.cls(protoObjectMetaClass);

		objectMetaClass.cls(metaClass);
		objectMetaClass.superclass(protoObjectMetaClass);
		object.cls(objectMetaClass);
		object.superclass(protoObject);

		behaviorMetaClass.cls(metaClass);
		behaviorMetaClass.superclass(objectMetaClass);
		behavior.cls(behaviorMetaClass);
		behavior.superclass(object);

		classDescriptionMetaClass.cls(metaClass);
		classDescriptionMetaClass.superclass(behaviorMetaClass);
		classDescription.cls(classDescriptionMetaClass);
		classDescription.superclass(behavior);

		clsMetaClass.cls(metaClass);
		clsMetaClass.superclass(classDescriptionMetaClass);
		cls.cls(clsMetaClass);
		cls.superclass(classDescription);

		metaClassMetaClass.superclass(classDescriptionMetaClass);
		metaClass.cls(metaClassMetaClass);
		metaClass.superclass(classDescription);

		stringMetaClass.cls(metaClass);
		string.cls(stringMetaClass);

		symbolMetaClass.cls(metaClass);
		symbolMetaClass.superclass(stringMetaClass);
		symbol.cls(symbolMetaClass);
		symbol.superclass(string);
	}

	private void registerClasses() {
		ProtoObject.primitiveRegisterAs(protoObject, "st.redline.ProtoObject");
		ProtoObject.primitiveRegisterAs(object, "st.redline.Object");
		ProtoObject.primitiveRegisterAs(metaClass, "st.redline.MetaClass");
		ProtoObject.primitiveRegisterAs(behavior, "st.redline.Behavior");
		ProtoObject.primitiveRegisterAs(classDescription, "st.redline.ClassDescription");
		ProtoObject.primitiveRegisterAs(cls, "st.redline.Class");
		ProtoObject.primitiveRegisterAs(symbol, "st.redline.Symbol");
		ProtoObject.primitiveRegisterAs(string, "st.redline.String");
	}

	private void mapPackages() {
		ProtoObject.packageMap.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st/redline")) {
			String packageName = sourceFile.substring(0, sourceFile.lastIndexOf("/"));
			String name = sourceFile.substring(packageName.length() + 1, sourceFile.lastIndexOf("."));
//			System.out.println(packageName + " " + name + " " + packageName.replaceAll(File.separator, ".") + "." + name);
			ProtoObject.packageMap.put(name, packageName.replaceAll(File.separator, ".") + "." + name);
		}
	}
}
