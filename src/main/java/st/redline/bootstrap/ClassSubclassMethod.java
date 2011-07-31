package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		System.out.println("ClassSubclassMethod() " + receiver + " " + String.valueOf(argument.javaValue()));
		String name = String.valueOf(argument.javaValue());
		ProtoObject subclass = ProtoObject.primitiveCreateSubclass(receiver);
		ProtoObject.primitiveRegisterAs(subclass, fullyQualifiedClassName(name));
		return subclass;
	}

	private String fullyQualifiedClassName(String name) {
		String packageName = ProtoObject.primitivePackageRegistryCurrent();
		return packageName == null ? name : packageName + "." + name;
	}
}
