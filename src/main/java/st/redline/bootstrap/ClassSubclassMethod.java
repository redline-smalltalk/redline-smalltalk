package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		ProtoObject subclass = createSubclass(createClass(receiver), createMetaclass(receiver));
		ProtoObject.primitiveRegisterAs(subclass, fullyQualifiedClassName(argument));
		return subclass;
	}

	private String fullyQualifiedClassName(ProtoObject argument) {
		String name = String.valueOf(argument.javaValue());
		String packageName = ProtoObject.primitiveSpecialRegisterAt(name);
		if (packageName != null)
			ProtoObject.primitiveSpecialRegisterRemove(name);
		return packageName == null ? name : packageName + "." + name;
	}

	private ProtoObject createSubclass(ProtoObject aClass, ProtoObject metaclass) {
		aClass.cls(metaclass);
		return aClass;
	}

	private ProtoObject createClass(ProtoObject receiver) {
		ProtoObject cls = new ProtoObject();
		cls.superclass(receiver);
		return cls;
	}

	private ProtoObject createMetaclass(ProtoObject receiver) {
		ProtoObject metaclass = new ProtoObject();
		metaclass.cls(ProtoObject.primitiveResolveObject(receiver, "Metaclass"));  // TODO.JCL Should this be 'Metaclass new'?
		metaclass.superclass(receiver.cls());
		return metaclass;
	}
}
