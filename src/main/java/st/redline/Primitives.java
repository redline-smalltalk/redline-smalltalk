/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.compiler.*;

import java.math.BigInteger;
import java.util.*;

public class Primitives {

	protected static boolean bootstrapping = false;

	private static final ThreadLocal<Stack<String>> packageRegistry = new ThreadLocal<Stack<String>>();
	private static final Map<String, AbstractMethod> methodsToBeCompiled = new HashMap<String, AbstractMethod>();
	private static final Map<String, Block> blocksToBeCompiled = new HashMap<String, Block>();
	private static final Map<String, ProtoBlock> blocksRegistry = new HashMap<String, ProtoBlock>();
	private static final ThreadLocal<Stack<ProtoObject>> classInitialisationRegistry = new ThreadLocal<Stack<ProtoObject>>();

	public static ProtoObject p1(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).add((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p2(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).subtract((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p3(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) < 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p4(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) > 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p5(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		int result = ((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue());
		if (result <= 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p6(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		int result = ((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue());
		if (result >= 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p7(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) == 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p8(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) != 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p9(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).multiply((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p10(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).divide((BigInteger) arg1.javaValue()));
	}


	public static ProtoObject p21(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).add((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p22(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).subtract((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p23(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) < 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p24(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) > 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p25(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		int result = ((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue());
		if (result <= 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p26(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		int result = ((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue());
		if (result >= 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p27(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) == 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p28(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		if (((BigInteger) receiver.javaValue()).compareTo((BigInteger) arg1.javaValue()) != 0)
			return ProtoObject.TRUE;
		return ProtoObject.FALSE;
	}

	public static ProtoObject p29(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).multiply((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p30(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		return instanceLike(receiver).javaValue(((BigInteger) receiver.javaValue()).divide((BigInteger) arg1.javaValue()));
	}

	public static ProtoObject p60(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// basicAt: / at:
//		System.out.println("** p60 at: " + arg1.javaValue());
		ProtoObject[] slots = (ProtoObject[]) receiver.javaValue();
		int index = ((BigInteger) arg1.javaValue()).intValue();
		if (index == 0)
			throw new IllegalStateException("Index to slot cannot be zero!");
		return slots[index];
	}

	public static ProtoObject p61(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
//		System.out.println("** p61 at: " + arg1.javaValue() + " put: " + arg2);
		// basicAt:put: / at:put:
		ProtoObject[] slots = (ProtoObject[]) receiver.javaValue();
		int index = ((BigInteger) arg1.javaValue()).intValue();
		if (index == 0)
			throw new IllegalStateException("Index to slot cannot be zero!");
		slots[index] = arg2;
		return arg2; // <- returns stored value according to BlueBook.
	}

	public static ProtoObject p62(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// does this primitive make sense for Redline?
		return ProtoObject.NIL;
	}

	public static ProtoObject p68(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// does this primitive make sense for Redline?
		return ProtoObject.NIL;
	}

	public static ProtoObject p69(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// does this primitive make sense for Redline?
		return receiver;
	}

	public static ProtoObject p70(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// create a new instance of the receiver (a class).
		return new ProtoObject(receiver);
	}

	public static ProtoObject p71(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// create a new instance of the receiver (a class) with the number of indexable fields specified by the size argument.
		ProtoObject instance = p70(receiver, thisContext, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		int size = ((BigInteger) arg1.javaValue()).intValue();
		instance.javaValue(new ProtoObject[size + 1]);   // <- +1 because Smalltalk indexes from 1.
		return instance;
	}

	public static ProtoObject p73(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// does this primitive make sense for Redline?
		return ProtoObject.NIL;
	}

	public static ProtoObject p74(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// does this primitive make sense for Redline?
		return receiver;
	}

	public static ProtoObject p110(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// ==
		return receiver == arg1 ? ProtoObject.TRUE : ProtoObject.FALSE;
	}

	public static ProtoObject p111(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// Object >> class
		return receiver.cls();
	}

	//
	//  Redline specific primitives
	//

	public static ProtoObject p201(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// [] value
		return ((ProtoBlock) receiver).applyTo(receiver, thisContext);
	}

	public static ProtoObject p202(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// [] value: arg
		return ((ProtoBlock) receiver).applyTo(receiver, thisContext, arg1);
	}

	public static ProtoObject p203(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// [] value: arg1 value: arg2
		return ((ProtoBlock) receiver).applyTo(receiver, thisContext, arg1, arg2);
	}

	public static ProtoObject p204(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// [] value: arg1 value: arg2 value: arg3
		return ((ProtoBlock) receiver).applyTo(receiver, thisContext, arg1, arg2, arg3);
	}

	public static ProtoObject p205(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		// [] value: arg1 value: arg2 value: arg3  value: arg4
		return ((ProtoBlock) receiver).applyTo(receiver, thisContext, arg1, arg2, arg3, arg4);
	}

	public static ProtoObject p210(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		ProtoBlock aBlock = callback210(receiver, thisContext, arg1);
		send(send(receiver, "value", thisContext), aBlock, "ifTrue:", thisContext);
		return ProtoObject.NIL;
	}

	private static ProtoBlock callback210(final ProtoObject receiver, final ThisContext thisContext, final ProtoObject trueBlock) {
		return new ProtoBlock() {
			public ProtoObject applyTo(ProtoObject r, ThisContext t) {
				System.out.println("callback210() evaluating true block.");
				Primitives.send(trueBlock, "value", thisContext);
				Primitives.send(Primitives.send(receiver, "value", thisContext), this, "ifTrue:", thisContext);
				return ProtoObject.NIL;
			}
		};
	}

	public static ProtoObject p211(ProtoObject receiver, ThisContext thisContext, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		ProtoBlock aBlock = callback211(receiver, thisContext, arg1);
		send(send(receiver, "value", thisContext), aBlock, "ifFalse:", thisContext);
		return ProtoObject.NIL;
	}

	private static ProtoBlock callback211(final ProtoObject receiver, final ThisContext thisContext, final ProtoObject falseBlock) {
		return new ProtoBlock() {
			public ProtoObject applyTo(ProtoObject r, ThisContext t) {
				System.out.println("callback210() evaluating false block.");
				Primitives.send(falseBlock, "value", thisContext);
				Primitives.send(Primitives.send(receiver, "value", thisContext), this, "ifFalse:", thisContext);
				return ProtoObject.NIL;
			}
		};
	}

	public static ProtoObject putAt(ProtoObject receiver, ProtoObject value, int index) throws ClassNotFoundException {
		// answers receiver.
//		System.out.println("putAt() " + receiver + " put: " + value + " at: " + index);
		ProtoObject slot = createInteger(receiver, index);
		send(receiver, slot, value, "at:put:", null);
		return receiver;
	}

	private static ProtoObject newWithValue(Object value) {
		ProtoObject instance = new ProtoObject();
		instance.javaValue(value);
		return instance;
	}

	private static ProtoObject newWith(ProtoObject receiver, String className, Object value) throws ClassNotFoundException {
		ProtoObject cls = resolveObject(receiver, className);
		ProtoObject instance = p70(cls, null, null, null, null, null, null, null, null);
		instance.javaValue(value);
		return instance;
	}

	public static ProtoObject newWith(ProtoObject receiver, Object value) throws ClassNotFoundException {
		return newWith(receiver, "ProtoObject", value);
	}

	public static ProtoObject createSymbol(ProtoObject receiver, String value) throws ClassNotFoundException {
		return createWith("Symbol", receiver, value);
	}

	private static ProtoObject createWith(String classname, ProtoObject receiver, Object value) throws ClassNotFoundException {
		if (bootstrapping)
			return newWithValue(value);
		ProtoObject cls = resolveObject(receiver, classname);
		ProtoObject instance = send(cls, "new", null);
		instance.javaValue(value);
		return instance;
	}

	private static ProtoObject createWith(String classname, ProtoObject receiver) throws ClassNotFoundException {
		if (bootstrapping)
			return newWithValue(null);
		ProtoObject cls = resolveObject(receiver, classname);
		return send(cls, "new", null);
	}

	public static ProtoObject createString(ProtoObject receiver, String value) throws ClassNotFoundException {
		return createWith("String", receiver, value);
	}

	public static ProtoObject createInteger(ProtoObject receiver, long value) throws ClassNotFoundException {
		return createWith("Integer", receiver, BigInteger.valueOf(value));
	}

	public static ProtoObject createInteger(ProtoObject receiver, String value) throws ClassNotFoundException {
		return createWith("Integer", receiver, new BigInteger(value));
	}

	public static ProtoObject createArray(ProtoObject receiver, int size) throws ClassNotFoundException {
		ProtoObject slots = createInteger(receiver, size);
		ProtoObject cls = resolveObject(receiver, "Array");
		return send(cls, slots, "new:", null);
	}

	public static ProtoObject createSubclass(ProtoObject superclass, String name) {
//		System.out.println("createSubclass() of " + superclass + " called " + name + "  **  " + superclass.cls());
		ProtoObject classClass = new ProtoObject(ProtoObject.METACLASS_INSTANCE);
		classClass.superclass0(superclass.cls());
		ProtoObject cls = new ProtoObject(classClass);
		cls.name(name);
		cls.superclass(superclass);
		addToClassInitialisationRegistry(cls);
		return cls;
	}

	protected static void addToClassInitialisationRegistry(ProtoObject cls) {
		Stack<ProtoObject> classRegistry = classInitialisationRegistry.get();
		if(classRegistry == null) {
			classRegistry = new Stack<ProtoObject>();
			classInitialisationRegistry.set(classRegistry);
		}
		classRegistry.push(cls);
	}

	public static ProtoObject createEigenSubclass(ProtoObject superclass, String name, ProtoObject loader) throws ClassNotFoundException {
//		System.out.println("createEigenSubclass() of " + superclass + " called " + name + "  **  " + superclass.cls() + " " + loader);
		ProtoObject cls = createSubclass(superclass, name);
		if (cls.cls().superclass() == null)
			cls.cls().superclass(resolveObject(superclass, "ProtoObject"));
		return cls;
	}

    public static ProtoObject createCharacter(ProtoObject receiver, String value) throws ClassNotFoundException {
        return createWith("Character", receiver, value);
    }

	public static ProtoObject registerAs(ProtoObject receiver, String name) {
//		System.out.println("registerAs() " + String.valueOf(name) + " " + receiver);
		return receiver.registerAs(name);
	}

	protected static ProtoObject instanceLike(ProtoObject receiver) {
		return new ProtoObject(receiver.cls());
	}

	public static ProtoObject resolveObject(ProtoObject receiver, String className) throws ClassNotFoundException {
//		System.out.println("resolveObject() " + receiver + " " + className);
		return receiver.resolveObject(className);
	}

	public static void packageAtPut(ProtoObject receiver, String name, String javaPackageName) {
//		System.out.println("packageAtPut() " + receiver + " " + name + " " + javaPackageName);
		receiver.packageAtPut(name, javaPackageName);
	}

	public static String packageAt(ProtoObject receiver, String name) {
		return receiver.packageAt(name);
	}

	public static void methodAtPut(ProtoObject receiver, String name, ProtoMethod method) {
		receiver.methodAtPut(name, method);
	}

	public static ProtoMethod methodAt(ProtoObject receiver, String name) {
		return receiver.methodAt(name);
	}

	public static void addVariables(ProtoObject receiver, String inputVariables) {
		Map<String, ProtoObject> variables = receiver.variables();
		if (variables == null)
			receiver.variables(ProtoObject.variablesMapInstance());
		for (String variable : inputVariables.split(" "))
			addVariable(receiver, variable);
	}

	private static void addVariable(ProtoObject receiver, String variable) {
		if (receiver.hasVariableNamed(variable))
			throw new IllegalStateException("Variable '" + variable + "' already defined.");
		receiver.variables().put(variable, ProtoObject.NIL);
	}

	public static void addInstanceVariables(ProtoObject receiver, String inputVariables) {
		Map<String, ProtoObject> variables = receiver.instanceVariables();
		if (variables == null)
			receiver.instanceVariables(ProtoObject.variablesMapInstance());
		for (String variable : inputVariables.split(" "))
			addInstanceVariable(receiver, variable);
	}

	private static void addInstanceVariable(ProtoObject receiver, String variable) {
		if (receiver.hasInstanceVariableNamed(variable))
			throw new IllegalStateException("Instance variable '" + variable + "' already defined.");
		receiver.instanceVariables().put(variable, ProtoObject.NIL);
	}

	public static void addClassInstanceVariables(ProtoObject receiver, String inputVariables) {
		if (receiver.cls() == null)
			throw new IllegalStateException("Receiver has no class.");
		Map<String, ProtoObject> variables = receiver.cls().variables();
		if (variables == null)
			receiver.cls().variables(ProtoObject.variablesMapInstance());
		for (String variable : inputVariables.split(" "))
			addClassInstanceVariable(receiver, variable, true);
	}

	public static void addClassInstanceVariable(ProtoObject receiver, String variable, boolean noDuplicates) {
		if (noDuplicates && receiver.cls().hasVariableNamed(variable))
			throw new IllegalStateException("Class instance variable '" + variable + "' already defined.");
		if (receiver.cls().variables() == null)
			receiver.cls().variables(ProtoObject.variablesMapInstance());
		receiver.cls().variables().put(variable, ProtoObject.NIL);
	}

	public static void registerMethodToBeCompiledAs(AbstractMethod method, String name) {
		String methodName = (method instanceof ClassMethod ? "+" : "-") + name;
		if (methodsToBeCompiled.containsKey(methodName))
			throw new IllegalStateException("Method to be compiled registered twice: " + methodName);
		methodsToBeCompiled.put(methodName, method);
	}

	public static void registerBlockToBeCompiledAs(Block block, String name) {
//		System.out.println("registerBlockToBeCompiledAs() " + name);
		if (blocksToBeCompiled.containsKey(name))
			throw new IllegalStateException("Block to be compiled registered twice: " + name);
		blocksToBeCompiled.put(name, block);
	}

	public static ProtoBlock compileBlock(ProtoObject receiver, String fullBlockName, String blockName, String className, String packageName, int countOfArguments, boolean isClassMethod, ThisContext thisContext) {
		// TODO.JCL clean this up.
//		System.out.println("primitiveCompileBlock() " + receiver + " " + fullBlockName + " " + blockName + " " + className + " " + packageName + " " + countOfArguments + " " + isClassMethod + " " + thisContext);
		if (blocksRegistry.containsKey(fullBlockName)) {
//			System.out.println("** existing block ** " + fullBlockName);
			try {
				ProtoBlock block = blocksRegistry.get(fullBlockName).getClass().newInstance();
				block.outerContext(thisContext);
				return block;
			} catch (Exception e) {
				throw new RedlineException(e);
			}
		}
		Block blockToBeCompiled = blocksToBeCompiled.remove(fullBlockName);
		if (blockToBeCompiled == null)
			throw new IllegalStateException("Block to be compiled '" + fullBlockName + "' not found.");
		BlockAnalyser blockAnalyser = new BlockAnalyser(className + '$' + blockName, packageName, countOfArguments, isClassMethod, blockToBeCompiled.analyser());
		blockToBeCompiled.accept(blockAnalyser);
		Class blockClass = ((SmalltalkClassLoader) Thread.currentThread().getContextClassLoader()).defineClass(blockAnalyser.classBytes());
		try {
//			System.out.println("** Instantiating block ** " + fullBlockName);
			ProtoBlock block = (ProtoBlock) blockClass.newInstance();
			block.outerContext(thisContext);
			blocksRegistry.put(fullBlockName, block);
			return block;
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
	}

	public static void compileMethod(ProtoObject receiver, String fullMethodName, String methodName, String className, String packageName, int countOfArguments, boolean isClassMethod) {
		// TODO.JCL clean this up.
//		System.out.println("primitiveCompileMethod() " + receiver + " " + fullMethodName + " " + methodName + " " + className + " " + packageName + " " + countOfArguments + " " + isClassMethod);
		AbstractMethod methodToBeCompiled = methodsToBeCompiled.remove((isClassMethod ? "+" : "-") + fullMethodName);
		if (methodToBeCompiled == null)
			throw new IllegalStateException("Method to be compiled '" + fullMethodName + "' not found.");

        final String fullyQualifiedMethodName;
        if(isClassMethod)
            fullyQualifiedMethodName = String.format("%s$$%s", className, methodName);
        else
            fullyQualifiedMethodName = String.format("%s$%s", className, methodName);

		MethodAnalyser methodAnalyser = new MethodAnalyser(fullyQualifiedMethodName, packageName, countOfArguments, isClassMethod, methodToBeCompiled.analyser());
		methodToBeCompiled.accept(methodAnalyser);
		Class methodClass = ((SmalltalkClassLoader) Thread.currentThread().getContextClassLoader()).defineClass(methodAnalyser.classBytes());
		ProtoMethod method;
		try {
			method = (ProtoMethod) methodClass.newInstance();
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
		receiver.methodAtPut(methodName, method);
	}

	public static String packageRegistryCurrent() {
		Stack<String> stack = packageRegistry.get();
		if (stack != null)
			return stack.peek();
		return "";
	}

	public static void packageRegistryCurrent(String packageName) {
		Stack<String> stack = packageRegistry.get();
		if (stack == null) {
			stack = new Stack<String>();
			packageRegistry.set(stack);
		}
		stack.push(packageName.replace("/", "."));
	}

	public static void packageRegistryRemove() {
		Stack<String> stack = packageRegistry.get();
		if (stack != null)
			stack.pop();
	}

	public static void temporariesInit(ThisContext thisContext, int size) {
		thisContext.temporariesInit(size);
	}

	public static ProtoObject outerContextMethodArgumentAt(ProtoObject receiver, int index) {
		return receiver.outerContext().argumentAt(index);
	}

	public static ProtoObject temporaryAt(ProtoObject receiver, ThisContext thisContext, int index, boolean isLocal) {
		if (isLocal)
			return thisContext.temporaryAt(index);
		ThisContext outerContext = receiver.outerContext();
		if (outerContext != null)
			return outerContext.temporaryAt(index);
		throw new IllegalStateException("Temporary being fetched from invalid outerContext.");
	}

	public static void temporaryPutAt(ProtoObject value, ProtoObject receiver, ThisContext thisContext, int index, boolean isLocal) {
		if (!isLocal) {
			ThisContext outerContext = receiver.outerContext();
			if (outerContext == null)
				throw new IllegalStateException("Temporary being updated in invalid outerContext.");
			outerContext.temporaryAtPut(index, value);
		} else {
			thisContext.temporaryAtPut(index, value);
		}
	}

	public static ProtoObject variableAt(ProtoObject receiver, String name, boolean isClassMethod, ThisContext thisContext) throws ClassNotFoundException {
//		System.out.println("variableAt() " + receiver + " " + name + " " + isClassMethod);
		ProtoObject value;
		if ((value = receiver.variableAt(name)) != null)
			return value;
		if ((value = receiver.cls().variableAt(name)) != null)
			return value;
		return receiver.resolveObject(name);
	}

	public static ProtoObject variablePutAt(ProtoObject value, String name, ProtoObject receiver, boolean isClassMethod, ThisContext thisContext) {
		if (receiver.variableAtPut(name, value) != null)
			return receiver;
		if (receiver.cls().variableAtPut(name, value) != null)
			return receiver;
		throw new IllegalStateException("'Variable '" + name + "' not found.");
	}

	private static ProtoMethod methodFor(ProtoObject object, String selector, ProtoObject[] methodForResult) {
//		System.out.println("methodFor() " + selector + " from " + object);
		if (object == null)
			return null;
		ProtoMethod method;
		ProtoObject superclass = object;
		while ((method = superclass.methodAt(selector)) == null)
			if ((superclass = superclass.superclass()) == null)
				break;
		methodForResult[0] = superclass;
		return method;
	}

	private static ProtoObject sendDoesNotUnderstand(ProtoObject receiver, String selector, ThisContext thisContext, ProtoObject[] arguments) {
		throw RedlineException.withMessage("TODO -  need to implement send of doesNotUnderstand - '" + selector + "' " + receiver);
	}

	public static ProtoObject send(ProtoObject receiver, String selector, ThisContext thisContext) {
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls()));
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0]));
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, String selector, ThisContext thisContext) {
//		System.out.println("send " + receiver + " " + selector + " " + thisContext + " arg: " + arg1 );
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1), arg1);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1), arg1);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, String selector, ThisContext thisContext) {
//		System.out.println("send " + receiver + " " + selector + " " + " " + thisContext + " arg1: " + arg1 + " arg2: " + arg2);
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1, arg2), arg1, arg2);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2), arg1, arg2);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, String selector, ThisContext thisContext) {
//		System.out.println("send " + receiver + " " + selector + " " + " " + classMethodWasFoundIn + " arg: " + arg1 );
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1, arg2, arg3), arg1, arg2, arg3);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3), arg1, arg2, arg3);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, String selector, ThisContext thisContext) {
//		System.out.println("send " + receiver + " " + selector + " " + " " + classMethodWasFoundIn + " arg: " + arg1 );
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1, arg2, arg3, arg4), arg1, arg2, arg3, arg4);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4), arg1, arg2, arg3, arg4);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, String selector, ThisContext thisContext) {
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1, arg2, arg3, arg4, arg5), arg1, arg2, arg3, arg4, arg5);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4, arg5), arg1, arg2, arg3, arg4, arg5);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4, arg5});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, String selector, ThisContext thisContext) {
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6), arg1, arg2, arg3, arg4, arg5, arg6);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4, arg5, arg6), arg1, arg2, arg3, arg4, arg5, arg6);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4, arg5, arg6});
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7, String selector, ThisContext thisContext) {
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6, arg7), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4, arg5, arg6, arg7});
	}

	public static ProtoObject superSend(ProtoObject receiver, String selector, ThisContext thisContext) {
//		System.out.println("superSend " + receiver + " " + selector + " " + classMethodWasFoundIn);
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass()));
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0]));
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, String selector, ThisContext thisContext) {
//		System.out.println("superSend " + receiver + " " + selector + " " + " " + classMethodWasFoundIn + " arg: " + arg1 );
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1), arg1);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1), arg1);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, String selector, ThisContext thisContext) {
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1, arg2), arg1, arg2);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2), arg1, arg2);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, String selector, ThisContext thisContext) {
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1, arg2, arg3), arg1, arg2, arg3);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3), arg1, arg2, arg3);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, String selector, ThisContext thisContext) {
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1, arg2, arg3, arg4), arg1, arg2, arg3, arg4);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4), arg1, arg2, arg3, arg4);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, String selector, ThisContext thisContext) {
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5), arg1, arg2, arg3, arg4, arg5);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4, arg5), arg1, arg2, arg3, arg4, arg5);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4, arg5});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, String selector, ThisContext thisContext) {
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6), arg1, arg2, arg3, arg4, arg5, arg6);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4, arg5, arg6), arg1, arg2, arg3, arg4, arg5, arg6);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4, arg5, arg6});
	}

	public static ProtoObject superSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7, String selector, ThisContext thisContext) {
		ProtoMethod method = thisContext.classMethodFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(thisContext.classMethodFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6, arg7), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		ProtoObject[] methodForResult = {null};
		method = methodFor(thisContext.classMethodFoundIn.superclass().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, new ThisContext(methodForResult[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		return sendDoesNotUnderstand(receiver, selector, thisContext, new ProtoObject[]{arg1, arg2, arg3, arg4, arg5, arg6, arg7});
	}

	public static void initialiseNewClasses() {
		Stack<ProtoObject> classRegistry = classInitialisationRegistry.get();
		while(classRegistry != null && !classRegistry.empty())
            send(classRegistry.pop(), "initialize", null);
	}
}
