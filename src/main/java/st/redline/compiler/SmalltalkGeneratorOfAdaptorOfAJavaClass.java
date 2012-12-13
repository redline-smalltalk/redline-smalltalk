/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import st.redline.core.PrimObject;
import st.redline.core.PrimObjectMetaclass;
import st.redline.core.RedlineException;
import st.redline.core.SmalltalkEnvironment;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;

public class SmalltalkGeneratorOfAdaptorOfAJavaClass
{
	public static void main(String[] args) throws NoSuchMethodException, SecurityException
	{
		System.out.println(onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "invokeRightJavaMethod").getParameterTypes()[0]);
		//System.out.println(methodNamed(String.class, "equals", new Class[] {String.class}) );
		//System.out.println(new SmalltalkGeneratorOfAdaptorOfAJavaClass("java.util.ArrayList").adaptorSource());
		
	}

	String fullyQualifiedJavaClassName;

	public SmalltalkGeneratorOfAdaptorOfAJavaClass(String thefullyQualifiedJavaClassName)
	{
		fullyQualifiedJavaClassName = thefullyQualifiedJavaClassName;
	}

	String fullyQualifiedAdaptorClassName()
	{
		return fullyQualifiedNameOfSmalltalkClassToUseToAdaptJavaClass(javaClassToAdapt());
	}

	String unQualifiedAdaptorClassName()
	{
		return stringAfterLastOrEmptyString(fullyQualifiedAdaptorClassName(), ".");
	}

	String fullyQualifiedJvmClassNameOfJavaClassToBeAdapted()
	{
		return fullyQualifiedJavaClassName.replace(".", "/");
	}

	static String fullyQualifiedJvmClassNameOfJavaClass(Class<?> c)
	{
		return c.getName().replace(".", "/");
	}
	static PrimObject adaptingSmalltalkClassForJavaClass(Class<?> javaType)
	{
		return  PrimObjectMetaclass.METACLASS.resolveObject(fullyQualifiedNameOfSmalltalkClassToUseToAdaptJavaClass(javaType));
	}
	Class<?> javaClassToAdapt()
	{
		return classForName(fullyQualifiedJavaClassName);
	}

	public String adaptorSource()
	{
		return adaptorSourceIndividualSmalltalkMethods();
		//return adaptorSourceDirectCalling();
	}
	public String adaptorSourceIndividualSmalltalkMethods()
	{
		StringBuffer sourceBuf = new StringBuffer();
		Class<?> javaClassToAdapt = javaClassToAdapt();
		sourceBuf.append("\"@: " + fullyQualifiedAdaptorClassName() + "\"\n");

		sourceBuf.append(smalltalkSourceToImportRequiredJavaClasses());

		sourceBuf.append("Object < #" + unQualifiedAdaptorClassName() + ".\n");
	
		for (Constructor<?> constructor : javaClassToAdapt.getDeclaredConstructors())
		{
			sourceBuf.append(smalltalkSourceForMethodThatAdaptsConstructor(constructor));
			sourceBuf.append("\n");
		}
		
		sourceBuf.append(smalltalkSourceForConstructorMultiplexingToIndividualSmalltalkMethods());
		sourceBuf.append(smalltalkSourceForSuperNew());
		sourceBuf.append(smalltalkSourceForClassMethod_selectorForWithPrefix());
		
		for (Method javaMethod : methodsToAdapt())
		{
			sourceBuf.append(smalltalkSourceForMethodThatAdaptsJavaMethod(javaMethod));
			sourceBuf.append("\n");
		}
		for (String javaMethodName : methodNamesThatNeedMultiplexing())
		{
			sourceBuf.append(smalltalkSourceForMethodMultiplexingToIndividualSmalltalkMethodsFor(javaMethodName));	
			sourceBuf.append("\n");
		}
		
		sourceBuf.append(smalltalkSourceForMethod_selectorForWithPrefix());
		
		sourceBuf.append("\n" + unQualifiedAdaptorClassName() + " initialize.\n");
		return sourceBuf.toString();
	}
	public String adaptorSourceDirectCalling()
	{
		StringBuffer sourceBuf = new StringBuffer();
		sourceBuf.append("\"@: " + fullyQualifiedAdaptorClassName() + "\"\n");

		sourceBuf.append(smalltalkSourceToImportRequiredJavaClasses());

		sourceBuf.append("Object < #" + unQualifiedAdaptorClassName() + ".\n");
		
		sourceBuf.append(smalltalkSourceForConstructorMultiplexingDirectlyTojavaConstructors());
		sourceBuf.append(smalltalkSourceForSuperNew());
		sourceBuf.append(smalltalkSourceForClassMethod_performJavaConstructorFromClassWithArgs());
		
		for (Method javaMethod: methodsToAdapt())
		{
			sourceBuf.append(smalltalkSourceForMethodMultiplexingDirectlyToJavaMethodFor(javaMethod.getName()));			
			sourceBuf.append("\n");
		}
		
		sourceBuf.append(smalltalkSourceForMethod_performJavaMethodNamedOnWithArgs());
		
		sourceBuf.append("\n" + unQualifiedAdaptorClassName() + " initialize.\n");
		return sourceBuf.toString();
	}

	private String smalltalkSourceToImportRequiredJavaClasses()
	{
		StringBuffer sourceBuf = new StringBuffer();

		for (Class<?> javaClass : javaClassesToImport())
		{
			String localName = localSmalltalkClassNameForImportedJavaClass(javaClass);
			sourceBuf.append("import: '" + javaClass.getName() + "' as: '" + localName + "'.\n");
		}

		return sourceBuf.toString();
	}

	Collection<Class<?>> javaClassesToImport()
	{
		Set<Class<?>> classesToImport = new HashSet<Class<?>>();
		for (Method m : methodsToAdapt())
		{
			classesToImport.addAll(classesToImportForReferenceTo(m.getReturnType()));
		}
		return classesToImport;
	}

	public Collection<Class<?>> classesToImportForReferenceTo(Class<?> referedToClass)
	{

		if (referedToClass.getName() == "void" || javaClassHasANonGeneratedAdaptor(referedToClass))
		{
			return new HashSet<Class<?>>();
		}
		else if (referedToClass.getName().startsWith("[L")) // import the classes whose instances are return in arrays
		{
			String elementClassname = referedToClass.getClass().getName().substring(0); // should this not have been substring 2 - note this is what I first tried
			Class<?> elementClass = classForName(elementClassname);
			return classesToImportForReferenceTo(elementClass);
		}
		else
		{
			Set<Class<?>> classesToImport = new HashSet<Class<?>>();
			classesToImport.add(referedToClass);
			return classesToImport;
		}

	}

	private static String nonGeneratedClassNameThatAdaptsJavaClassNamedOrNull(String javaClassName)
	{
		if (javaClassName.equals("java.lang.String"))
		{
			return "String";
		}
		if (javaClassName.equals("int") | javaClassName.equals("long") | javaClassName.equals("java.lang.Integer") |  javaClassName.equals("java.lang.Long"))
		{
			return "Integer";
		}
		if (javaClassName.equals("boolean"))
		{
			return "Boolean";
		}
		if (javaClassName.equals("java.lang.Boolean"))
		{
			return "Boolean";
		}
		if (javaClassName.startsWith("[L"))
		{
			return "Array";
		}
		return null;
	}

	static boolean javaClassHasANonGeneratedAdaptor(Class<?> javaClass)
	{
		return javaClassHasANonGeneratedAdaptor(javaClass.getName());
	}

	static boolean javaClassHasANonGeneratedAdaptor(String javaClassName)
	{
		return nonGeneratedClassNameThatAdaptsJavaClassNamedOrNull(javaClassName) != null;
	}
	static boolean smalltalkObjectIsAnInstanceOfAJavaAdaptorClass(PrimObject smalltalkObject)
	{
		//System.out.println("********************* in smalltalkObjectIsAnInstanceOfAJavaAdaptorClass classname=>"+((PrimObjectMetaclass) smalltalkObject.cls()).name() + "<");
		return PrimObject.classIsAnAdaptorClass(smalltalkObject.cls());
		
	}


	static public String fullyQualifiedNameOfSmalltalkClassToUseToAdaptJavaClass(Class<?> javaClassToAdapt)
	{
		return fullyQualifiedNameOfSmalltalkClassToUseToAdaptJavaClass(javaClassToAdapt.getName());
	}

	static public String fullyQualifiedNameOfSmalltalkClassToUseToAdaptJavaClass(String fullyQualifiednameOfJavaClassToAdapt)
	{
		if (javaClassHasANonGeneratedAdaptor(fullyQualifiednameOfJavaClassToAdapt))
		{
			return nonGeneratedClassNameThatAdaptsJavaClassNamedOrNull(fullyQualifiednameOfJavaClassToAdapt);
		}
		return "smalltalkClassesThatAdaptJavaClasses." + fullyQualifiednameOfJavaClassToAdapt + "Adaptor"; // ensure never attempt to use a restricted package like java.util and also ensure I never have a namespace clash
	}

	static String localSmalltalkClassNameForImportedJavaClass(Class<?> c)
	{
		return fullyQualifiedNameOfSmalltalkClassToUseToAdaptJavaClass(c).replace(".", "_");
	}

	static Method smalltalkObjectForJavaValueMethodForJavaType(Class<?> classOfJavaValue)
	{
		// need to have different methods because I failed to be able to call one java method in smalltalk with all the required types - this seemed the least worst option
		String classNameOfJavaValue = classOfJavaValue.getName();
		if (classNameOfJavaValue.startsWith("[L"))
		{
			return onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "smalltalkObjectForJavaArray");
		}
		if (classNameOfJavaValue.contains("."))
		{
			return methodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "smalltalkObjectForJavaValue", new Class[] { Object.class });
		}
		if (classNameOfJavaValue.equals("int"))
		{
			return methodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "smalltalkObjectForJavaInt", new Class[] { int.class });
		}
		if (classNameOfJavaValue.equals("long"))
		{
			return methodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "smalltalkObjectForJavaLong", new Class[] { long.class });
		}
		if (classNameOfJavaValue.equals("boolean"))
		{
			return methodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "smalltalkObjectForJavaBoolean", new Class[] { boolean.class });
		}
		throw new IllegalStateException("need to implement method to convert java instance of " + classOfJavaValue.getName());
	}
	public static Object javaObjectForSmalltalkObject(PrimObject smalltalkObject)
    {
		
//    	System.out.println("in javaObjectForSmalltalkObject smalltalkObject=" + smalltalkObject);
		if (smalltalkObjectIsAnInstanceOfAJavaAdaptorClass(smalltalkObject))
		{
//			System.out.println("end javaObjectForSmalltalkObject smalltalkObject=" + smalltalkObject);
    		return smalltalkObject.javaValue();
		}
		else if (smalltalkObject.javaValue().getClass().isArray())
    	{
	    		Object[] arr = (Object[]) smalltalkObject.javaValue();
	        	Object[] newArr = new Object[arr.length];
	        	for(int i= 0; i < arr.length; i++)
	        	{
	        		PrimObject p = (PrimObject) arr[i];
	        		newArr[i]=javaObjectForSmalltalkObject(p);
	        	}
	
//	    		System.out.println("end javaObjectForSmalltalkObject smalltalkObject=" + smalltalkObject);
	        	return newArr;
    		
    	}
    	// JAMES how do you test to see if a object is of a particular Smalltalk type
    	else if(smalltalkObject.javaValue().getClass() == ArrayList.class)
    	{
    		ArrayList l = (ArrayList) smalltalkObject.javaValue();
    		ArrayList newList = new ArrayList(); 
    		
            for (int i = 1; i < l.size(); i++)
        	{
        		PrimObject p = (PrimObject) l.get(i);
        		newList.add(javaObjectForSmalltalkObject(p));
        	}
	    	if(((PrimObjectMetaclass)smalltalkObject.cls()).name().javaValue().equals("String") )
	    	{
	    		StringBuffer b = new StringBuffer();
	    		for(Object e : newList)
	        	{
	        		char c = (Character) e;
	        		b.append(c);
	        	}
//	    		System.out.println("end javaObjectForSmalltalkObject smalltalkObject=" + smalltalkObject);
	    		return b.toString();
	    	}
			else
			{
//				System.out.println("end javaObjectForSmalltalkObject smalltalkObject=" + smalltalkObject);
	    		return newList;
			}
    	}
    	else
    	{ 
//    		System.out.println("end javaObjectForSmalltalkObject smalltalkObject=" + smalltalkObject);
    		return smalltalkObject.javaValue();
    	}
    	
			
    	
    			
    }
	
	static public  PrimObject smalltalkObjectForJavaValue(Object javaValue)
	{
		Class<?> javaType = javaValue.getClass();

		//System.out.println("in smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
		if (!javaClassHasANonGeneratedAdaptor(javaType))
		{
			 
			PrimObject smalltalkObject = adaptingSmalltalkClassForJavaClass(javaType).perform("superNew");
			smalltalkObject.javaValue(javaValue);

			//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
			return smalltalkObject;
		}
		else
		{
			// dont want to use reflection here as it might be slow????
			if (javaType==String.class)
			{
				//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
				return PrimObject.string(javaValue); // .string(javaType.cast(javaType));
			}
			else if (javaType==Integer.class)
			{
				//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
				return PrimObject.number((Integer)javaValue);
			}
			else if (javaType==Boolean.class)
			{
				//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
				return ((Boolean) javaValue)?PrimObject.TRUE:PrimObject.FALSE;
			}
			else if (javaType==int.class)
			{
				//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
				return PrimObject.number((Integer)javaValue);
			}
			else if (javaType.isArray())
			{
				//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
				return smalltalkObjectForJavaArray(javaValue);
			}
			else
			{
				//System.out.println("end smalltalkObjectForJavaValue javaValue=" + javaValue + " javaType.getName()=" + javaType.getName());
				throw new IllegalStateException("converter need to be built for " + javaType.getName());
			}
		}
	}

	

	public static <X> PrimObject smalltalkObjectForJavaArray(Object arr1)
	{
		X[] arr = (X[]) arr1;
		PrimObject stArray = PrimObject.array(arr.length);
		for (int i = 1; i <= arr.length; i++)
		{
			PrimObject element = smalltalkObjectForJavaValue(arr[i - 1]);
			PrimObject.putAt(stArray, element, i);
		}
		return stArray;

	}

	public static PrimObject smalltalkObjectForJavaInt(int v)
	{
		return smalltalkObjectForJavaValue(v);
	}

	public static PrimObject smalltalkObjectForJavaLong(long v)
	{
		return smalltalkObjectForJavaValue(v);
	}

	public static PrimObject smalltalkObjectForJavaBoolean(boolean v)
	{
		return smalltalkObjectForJavaValue(v);
	}

	private static final Map<String, String> IGNORED_METHODS = new HashMap<String, String>();
	static
	{
		IGNORED_METHODS.put("getClass", "getClass");
	}

	Collection<Method> methodsToAdapt()
	{
		Set<Method> methodsToAdapt = new HashSet<Method>();
		for (Method m : javaClassToAdapt().getMethods())
		{
			if (Modifier.isPublic(m.getModifiers()) && !IGNORED_METHODS.containsKey(m.getName()))
			{
				methodsToAdapt.add(m);
			}
		}
		return methodsToAdapt;
	}

	Collection<String> methodNamesThatNeedMultiplexing()
	{
		Set<String> methodnames = new HashSet<String>();
		for (Method m : methodsToAdapt())
		{
			if (m.getParameterTypes().length > 0)
			{
				methodnames.add(m.getName());
			}
		}
		return methodnames;
	}

	/*private String smalltalkSourceForMethodThatAdaptsConstructor_old(Constructor<?> constructor)
	{
		StringBuffer sourceBuf = new StringBuffer();

		boolean constructorHasNoArgs = constructor.getParameterTypes().length == 0;
		sourceBuf.append(unQualifiedAdaptorClassName() + " class atSelector: #" + methodNameForMethodAdaptingIndividualConstructor(constructor) + " put: [");
		sourceBuf.append(constructorHasNoArgs ? " " : " :args |").append("| obj |\n  obj := ").append(constructorHasNoArgs ? "super new.\n" : "self new.\n");
		sourceBuf.append("  JVM atTemp: 0;\n");
		sourceBuf.append("      new: '" + fullyQualifiedJvmClassNameOfJavaClassToBeAdapted() + "';\n");
		sourceBuf.append("      dup.\n");
		sourceBuf.append(smalltalkSourceToCovertArgumentsAndPutOnStack(constructor.getParameterTypes()));
		sourceBuf.append("  JVM invokeSpecial: '" + fullyQualifiedJvmClassNameOfJavaClassToBeAdapted() + "' method: '<init>' matching: '(" + javaArgsSignature(constructor.getParameterTypes()) + ")V';\n")
		.append("      invokeVirtual: 'st/redline/core/PrimObject' method: 'javaValue' matching: '(Ljava/lang/Object;)Lst/redline/core/PrimObject;'.\n")
		.append("  ^ obj.\n")
		.append("].\n");
		return sourceBuf.toString();
	}*/
	private String smalltalkSourceForMethodThatAdaptsConstructor(Constructor<?> constructor)
	{
		StringBuffer sourceBuf = new StringBuffer();

		boolean constructorHasNoArgs = constructor.getParameterTypes().length == 0;
		sourceBuf.append(unQualifiedAdaptorClassName() + " class atSelector: #" + methodNameForMethodAdaptingIndividualConstructor(constructor) + " put: [");
		sourceBuf.append(constructorHasNoArgs ? " " : " :args |").append("| obj |\n");
		sourceBuf.append(smalltalkToCallConstructorOnSmalltalkArgsViaInitAndNew("JVM putTemp: 0.", constructor, ARGS_IN_COLLECTION_ON_STACK));
		sourceBuf.append("  ^ obj.\n");
		sourceBuf.append("].\n");
		return sourceBuf.toString();
		
	}

	private String smalltalkSourceForMethodThatAdaptsJavaMethod(Method javaMethod)
	{
		StringBuffer sourceBuf = new StringBuffer();
		
		boolean methodHasNoArgs = javaMethod.getParameterTypes().length == 0;
		//boolean methodIsVoid = javaMethod.getReturnType().getName().equals("void"); //in the case of no  return value  and rtn variable has no effect except to return nil
		sourceBuf.append(unQualifiedAdaptorClassName() + " atSelector: #" + methodNameForMethodAdaptingIndividualMethod(javaMethod) + " put: [");
		sourceBuf.append(methodHasNoArgs ? " " : " :args |");
		
		sourceBuf.append("| rtn |\n");
		
		sourceBuf.append(smalltalkToCallJavaMethod_putAnswerInTemp0__useCurrentReciever_ArgsInCollectionOnStack(javaMethod));
		sourceBuf.append("  ^rtn.\n");
		
		sourceBuf.append("].\n");

		return sourceBuf.toString();
	}
	Method primaryPerformMethod()
	{
		try
		{
			return PrimObject.class.getMethod("perform", new Class[] {	PrimObject.class,PrimObject.class} );
		}
		catch (NoSuchMethodException e)
		{
			throw new IllegalStateException(e);
		}
		catch (SecurityException e)
		{
			throw new IllegalStateException(e);
		}
	}
	
	private String smalltalkSourceForConstructorMultiplexingToIndividualSmalltalkMethods()
	{
		StringBuffer sourceBuf = new StringBuffer();
		sourceBuf.append(unQualifiedAdaptorClassName() + " class atSelector: #with: put: [ :args || selector javaClassName|\n");
		sourceBuf.append("  javaClassName := '"+fullyQualifiedJavaClassName+"'.");
		sourceBuf.append(smalltalkToCallStaticJavaMethod("JVM putTemp: 0.", onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class,
				"smalltalkSelectorForMethodThatWrapsRightJavaConstructorForClassNamed"), "JVM atTemp: 1.","JVM arg: 0.")); // selector := smalltalkSelectorForMethodThatWrapsRightJavaConstructorForClassNamed(javaClassName,args).
		sourceBuf.append(smalltalkToCallJavaMethod("","JVM aload: 1.",primaryPerformMethod() , "JVM arg: 0.", "JVM atTemp: 0."));
		sourceBuf.append("].\n").append("\n");

	/*	sourceBuf.append(unQualifiedAdaptorClassName() + " class atSelector: #newWithoutCallingAConstructor put: [");
		sourceBuf.append("^super new.\n").append("].\n").append("\n");*/

		return sourceBuf.toString();
	}
	private String smalltalkSourceForConstructorMultiplexingDirectlyTojavaConstructors()
	{
		StringBuffer sourceBuf = new StringBuffer();
		sourceBuf.append(unQualifiedAdaptorClassName())
	 	.append(" class atSelector: #new put: [ |args | args := Array new: 0.\n");
		sourceBuf.append("^self performWithArgs: args javaConstructorOfClassNamed: '"+this.fullyQualifiedJavaClassName+"'.\n"); //this method is constructed in smalltalkSourceForClassMethod_performJavaConstructorFromClassWithArgs
		sourceBuf.append("].\n").append("\n");
		
		sourceBuf.append(unQualifiedAdaptorClassName())
		 	.append(" class atSelector: #with: put: [ :args | \n");
		sourceBuf.append("^self performWithArgs: args javaConstructorOfClassNamed: '"+this.fullyQualifiedJavaClassName+"'.\n"); //this method is constructed in smalltalkSourceForClassMethod_performJavaConstructorFromClassWithArgs
		sourceBuf.append("].\n").append("\n");
		
		
		return sourceBuf.toString();
	}
	private String smalltalkSourceForSuperNew()
	{
		StringBuffer sourceBuf = new StringBuffer();
		
		sourceBuf.append(unQualifiedAdaptorClassName())
	 		.append(" class atSelector: #superNew put: [ :args | \n");
		sourceBuf.append("^super new.\n"); 
		sourceBuf.append("].\n").append("\n");
		
		return sourceBuf.toString();
	}
	
	
	

	private String smalltalkSourceForMethodMultiplexingToIndividualSmalltalkMethodsFor(String methodName)
	{
		// note that methods without parameters will already exist eg fred()
		// will have already been adapted as fred
		// this multiplexor is only required if there exists a method like
		// fred(int,Object) which will have need adapted as fredIObject:
		// and this multiplexor will be called fred: (which takes an args
		// collection)
		// a future optimization for the case that there is only ONE java
		// function fred(with parameters) would be to adapt it directly to fred:
		
		StringBuffer sourceBuf = new StringBuffer();
		Method smalltalkSelectorForMethodThatWrapsRightJavaMethodFor = onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class,"smalltalkSelectorForMethodThatWrapsRightJavaMethodFor");
		sourceBuf.append(unQualifiedAdaptorClassName()+ " atSelector: #" + methodName + ": put: [ :args || selector methodName|\n"); 
		sourceBuf.append("  methodName := '" + methodName + "'.\n");
		sourceBuf.append(smalltalkToCallStaticJavaMethod("JVM putTemp: 0.",smalltalkSelectorForMethodThatWrapsRightJavaMethodFor , "JVM aload: 1.","JVM atTemp: 1.","JVM arg: 0.")); // selector := smalltalkSelectorForMethodThatWrapsRightJavaMethodFor(receiver, methodName, args).
		sourceBuf.append(smalltalkToCallJavaMethod("","JVM aload: 1.",primaryPerformMethod() , "JVM arg: 0.", "JVM atTemp: 0.")+ "].\n\n");
		
			
		return sourceBuf.toString();
	}
	
	
	private String smalltalkSourceForMethodMultiplexingDirectlyToJavaMethodFor(String methodName)
	{
		
		StringBuffer sourceBuf = new StringBuffer();
		List<Method> methodsWithName = methodsNamed(javaClassToAdapt(), methodName);
		if(methodsWithName.size()==1 && methodsWithName.get(0).getParameterTypes().length==0)
		{
			sourceBuf.append(unQualifiedAdaptorClassName())
		 	.append(" atSelector: #" + methodName + " put: [ |args | args := Array new:0.\n");
	
		}
		else
		{
			sourceBuf.append(unQualifiedAdaptorClassName())
		 	.append(" atSelector: #" + methodName + ": put: [ :args | \n");
	
		}
		sourceBuf.append("^self performWithArgs: args javaMethodNamed: '"+methodName+"'.\n"); // this method is constructed by smalltalkSourceForMethod_performJavaMethodNamedOnWithArgs
		sourceBuf.append("].\n").append("\n");
		
		
		return sourceBuf.toString();
			
	}
	
	private String smalltalkSourceForClassMethod_performJavaConstructorFromClassWithArgs()
	{	
		StringBuffer sourceBuf = new StringBuffer();
		Method invokeConstructor = onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "invokeRightJavaConstructorForClassNamedWithArgsInList") ;
		sourceBuf.append(unQualifiedAdaptorClassName() + " class atSelector: #performWithArgs:javaConstructorOfClassNamed: put: [:args  :className || rtn |\n"); 
		sourceBuf.append(smalltalkToCallStaticJavaMethod("JVM putTemp: 0.",invokeConstructor,"JVM arg: 1.","JVM arg: 0."));
		sourceBuf.append("  ^rtn.\n");
		sourceBuf.append("].\n").append("\n");
				
		return sourceBuf.toString();			
	}
	private String smalltalkSourceForMethod_performJavaMethodNamedOnWithArgs()
	{	
		StringBuffer sourceBuf = new StringBuffer();

		Method invokeMethod = onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "invokeRightJavaMethodArgsInList") ;
		sourceBuf.append(unQualifiedAdaptorClassName() + " atSelector: #performWithArgs:javaMethodNamed: put: [:args  :selector || rtn |\n"); 
		sourceBuf.append(smalltalkToCallStaticJavaMethod("JVM putTemp: 0.",invokeMethod,"JVM aload:1.","JVM arg: 1.","JVM arg: 0."));
		sourceBuf.append("  ^rtn.\n");
		sourceBuf.append("].\n").append("\n");
				
		return sourceBuf.toString();			
	}
	
	private String smalltalkSourceForClassMethod_selectorForWithPrefix()
	{
		StringBuffer sourceBuf = new StringBuffer();
		sourceBuf.append(unQualifiedAdaptorClassName())
		 	.append(" class atSelector: #selectorFor:withPrefix: put: [ :args :prefix |\n")
		 	.append("  <primitive: 227>\n")
		 	.append("].\n");
		return sourceBuf.toString();
	}
	private String smalltalkSourceForMethod_selectorForWithPrefix()
	{
		StringBuffer sourceBuf = new StringBuffer();
		sourceBuf.append(unQualifiedAdaptorClassName())
		 	.append(" atSelector: #selectorFor:withPrefix: put: [ :args :prefix |\n")
		 	.append("  <primitive: 227>\n")
		 	.append("].\n");
		return sourceBuf.toString();
	}
	static String methodNameForMethodAdaptingIndividualMethod(Method m)
	{
		String methodName = m.getName();
		for (Class<?> parameterType : m.getParameterTypes())
		{
			methodName = methodName + methodSymbolComponentForType(parameterType);
		}
		boolean methodHasNoArgs = m.getParameterTypes().length == 0;
		if (!methodHasNoArgs)
		{
			methodName = methodName + ":";
		}
		return methodName;
	}

	static String methodNameForMethodAdaptingIndividualConstructor(Constructor<?> c)
	{
		if (c.getParameterTypes().length == 0)
		{
			return "new";
		}
		String methodName = "with";
		for (Class<?> parameterType : c.getParameterTypes())
		{
			methodName = methodName + methodSymbolComponentForType(parameterType);
		}
		return methodName = methodName + ":";
	}

	/* obsolete 
	private static String smalltalkSourceToCovertArgumentsAndPutOnStack(Class<?>[] argTypes)
	{
		StringBuffer sourceBuf = new StringBuffer();
		for (int i = 0; i < argTypes.length; i++)
		{
			sourceBuf.append(smalltalkSourceToToPushTheArgumentTheAprropriateJavaValueInTheStack(i, argTypes[i]));
		}
		return sourceBuf.toString();
	}
	*/

	/* obsolete 
	private static String smalltalkSourceToToPushTheArgumentTheAprropriateJavaValueInTheStack(int argIndex, Class<?> argType)
	{
		StringBuffer sourceBuf = new StringBuffer();
		sourceBuf.append("  JVM arg: 0 at: " + (argIndex + 1) + ".\n");// put the smalltalk array element  ON the sttack
		sourceBuf.append(smalltalkSourceToConvertASmalltalkVariableOnStackToTypeAndLeaveThatOnStack(argType));
		return sourceBuf.toString();
	}
	*/

	private static String smalltalkSourceToConvertASmalltalkVariableOnStackToTypeAndLeaveThatOnStack(Class<?> argType)
	{
		String type = jvmParameterDescriptorForClass(argType);
		StringBuffer sourceBuf = new StringBuffer();
		if(PrimObject.class.isAssignableFrom(argType)) { return "";}  // in this case we want the smalltalk type on the stack
		sourceBuf.append(smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "javaObjectForSmalltalkObject"));
//System.out.println("chad1>type="+type);
		if (type.startsWith("["))
		{
			sourceBuf.append(smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "convertToArrayOfObjects"));
			sourceBuf.append("  JVM checkcast: '[Ljava/lang/Object;'.\n");
		}
		else if (type.startsWith("L"))
		{
			sourceBuf.append("  JVM checkcast: '" + type.substring(1, type.length() - 1) + "'.\n");
		}
		else
		{
			// type is primitive so map from Redline internal type to java type.
			if (type.equals("I"))
			{
				sourceBuf.append("  JVM checkcast: 'java/math/BigDecimal'.\n");
				sourceBuf.append("  JVM invokeVirtual: 'java/math/BigDecimal' method: 'intValue' matching: '()I'.\n");
			}
			else if (type.equals("J"))
			{
				sourceBuf.append("  JVM checkcast: 'java/math/BigDecimal'.\n");
				sourceBuf.append("  JVM invokeVirtual: 'java/math/BigDecimal' method: 'longValue' matching: '()J'.\n");
			}

			else if (type.equals("Z"))
			{
				sourceBuf.append("  JVM checkcast: 'Z';\n");
			}
			else
			{
				throw new IllegalStateException("Need to cater for conversion of type '" + type + "'.");
			}
		}
		return sourceBuf.toString();
	}

	protected static final Map<String, String> PRIMITIVE_TO_SIGNATURE_TYPE = new HashMap<String, String>();
	static
	{
		PRIMITIVE_TO_SIGNATURE_TYPE.put("long", "J");
		PRIMITIVE_TO_SIGNATURE_TYPE.put("int", "I");
		PRIMITIVE_TO_SIGNATURE_TYPE.put("char", "C");
		PRIMITIVE_TO_SIGNATURE_TYPE.put("byte", "B");
		PRIMITIVE_TO_SIGNATURE_TYPE.put("void", "V");
		PRIMITIVE_TO_SIGNATURE_TYPE.put("boolean", "Z");
	}

	public static String smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(Class<?> cls,String methodName,Class<?>[] parameterTypes)
	{
		return smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(methodNamed(cls, methodName,parameterTypes));
	}
	public static String smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(Class<?> cls,String methodName)
	{
		return smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(onlyPublicMethodNamed(cls, methodName));
	}
	public static String smalltalkToDirectlyInvokeJavaMethodStackAlreadyPrepared(Method javaMethod)
	{
		boolean methodIsStatic = Modifier.isStatic(javaMethod.getModifiers());
		String methodName=		javaMethod.getName();
		StringBuffer sourceBuf = new StringBuffer();
		String invoke = methodName.equals("<init>") ? "invokeSpecial" : (methodIsStatic?"invokeStatic":"invokeVirtual");
		sourceBuf.append("  JVM "+invoke+": '" + fullyQualifiedJvmClassNameOfJavaClass(javaMethod.getDeclaringClass()) + "' method: '" + methodName + "' matching: '" + jvmMethodDescriptorForMethod(javaMethod) + "'.\n");
		return sourceBuf.toString();
	}


	public static String ARGS_IN_COLLECTION_ON_STACK= new String("121212");
	public static String smalltalkToCallJavaMethod(boolean methodIsStatic, Class<?> returnType,String smalltalkCodeToPullAnswerFromStack,Class<?> declaringClass,String smalltalkReceiverToStackCode, String methodName, Class<?>[] parameterTypes,String ... smalltalkCodeToPutArgumentsOnStack  )
	{
		StringBuffer sourceBuf = new StringBuffer();
		
		boolean methodIsConstructor = declaringClass.getName().equals(methodName );
		
		if (!methodIsStatic&& !methodIsConstructor)
		{
			sourceBuf.append("  " + smalltalkReceiverToStackCode+ "\n");// put the receiver on the stack
			sourceBuf.append(smalltalkSourceToConvertASmalltalkVariableOnStackToTypeAndLeaveThatOnStack(declaringClass));
		}
		if(methodIsConstructor) 
		{
			sourceBuf.append("  JVM new: '" + fullyQualifiedJvmClassNameOfJavaClass(returnType)+ "'.\n  JVM dup.");
		}
		
		if( !(smalltalkCodeToPutArgumentsOnStack.length == parameterTypes.length 
				|| (smalltalkCodeToPutArgumentsOnStack.length != parameterTypes.length  &&  smalltalkCodeToPutArgumentsOnStack[0]==ARGS_IN_COLLECTION_ON_STACK ) )) 
			{ throw new IllegalStateException("should not happen");}  
		if( smalltalkCodeToPutArgumentsOnStack[0]==ARGS_IN_COLLECTION_ON_STACK)
		{
			for(int i=0 ; i<parameterTypes.length;i++)
			{
				sourceBuf.append("  JVM arg:0 at:" + ( i + 1 ) + ".\n");
				sourceBuf.append( smalltalkSourceToConvertASmalltalkVariableOnStackToTypeAndLeaveThatOnStack(parameterTypes[i]));
			};
		}
		else
		{
			for(int i=0 ; i<parameterTypes.length;i++)			{
				sourceBuf.append("  " + smalltalkCodeToPutArgumentsOnStack[i]+ "\n");
				sourceBuf.append( smalltalkSourceToConvertASmalltalkVariableOnStackToTypeAndLeaveThatOnStack(parameterTypes[i]));
			};
		}
		if (methodIsConstructor)
		{
			sourceBuf.append("  JVM invokeSpecial: '" + fullyQualifiedJvmClassNameOfJavaClass(declaringClass) + "' method: '<init>' matching: '" + jvmMethodDescriptorFor(void.class,parameterTypes) + "'.\n");
		}
		else
		{
			String invoke =  (methodIsStatic?"invokeStatic":"invokeVirtual");
			sourceBuf.append("  JVM "+invoke+": '" + fullyQualifiedJvmClassNameOfJavaClass(declaringClass) + "' method: '" + methodName + "' matching: '" + jvmMethodDescriptorFor(returnType,parameterTypes) + "'.\n");
		}
		
		

		boolean methodIsVoid = (returnType==null)|| (returnType.getName().equals("void"));
		boolean wantsToReturnTheSmalltalkObject = returnType!=null && PrimObject.class.isAssignableFrom(returnType);
		if (!methodIsVoid && !wantsToReturnTheSmalltalkObject)
		{
			Method smalltalkObjectForJavaValueMethod = smalltalkObjectForJavaValueMethodForJavaType(returnType);// 'smalltalkObjectForJavaValue'
			sourceBuf.append("  JVM invokeStatic: 'st/redline/compiler/SmalltalkGeneratorOfAdaptorOfAJavaClass' method:'" + smalltalkObjectForJavaValueMethod.getName() + "' matching: '(" + javaArgsSignature(smalltalkObjectForJavaValueMethod.getParameterTypes()) + ")Lst/redline/core/PrimObject;'.\n");
		}
		if (!methodIsVoid) {sourceBuf.append("  " + smalltalkCodeToPullAnswerFromStack+"\n");}
		
		return sourceBuf.toString();
	}
	public static String smalltalkToCallJavaMethod( String smalltalkCodeToPullAnswerFromStack,String smalltalkReceiverToStackCode, Method javaMethod, String ... smalltalkCodeToPutArgumentsOnStack )
	{
		return smalltalkToCallJavaMethod(Modifier.isStatic(javaMethod.getModifiers()),javaMethod.getReturnType(),smalltalkCodeToPullAnswerFromStack, javaMethod.getDeclaringClass(),smalltalkReceiverToStackCode,javaMethod.getName(),javaMethod.getParameterTypes(),smalltalkCodeToPutArgumentsOnStack);
	}

	public static  String smalltalkToCallStaticJavaMethod(String smalltalkCodeToPullAnswerFromStack,Method javaMethod, String ... smalltalkRefToStackCodes )
	{
		return smalltalkToCallJavaMethod(smalltalkCodeToPullAnswerFromStack,null,javaMethod,smalltalkRefToStackCodes);
	}
	public static String smalltalkToCallConstructorOnSmalltalkArgsViaInitAndNew( String smalltalkCodeToPullAnswerFromStack, Constructor constructor, String ... smalltalkCodeToPutArgumentsOnStack )
	{
		
		// use smalltalkToCallJavaMethod so that parameter wrapping code is written only once
			return smalltalkToCallJavaMethod(false,constructor.getDeclaringClass(),smalltalkCodeToPullAnswerFromStack, constructor.getDeclaringClass(),"",
					constructor.getName(),constructor.getParameterTypes(),smalltalkCodeToPutArgumentsOnStack);
		
		/*
		// Beginning of attempt to not trick smalltalkToCallJavaMethod and treat <init> aas A real void method -  probably wont work because is relaies on handling an uninitialized java object
		StringBuffer sourceBuf = new StringBuffer();
		
		sourceBuf.append("JVM new:'" + fullyQualifiedJvmClassNameOfJavaClass(constructor.getDeclaringClass())+ "'.\n"); // new the object
		
		// wrap in smalltalk object leaving smalltalk object on stack
		Method smalltalkObjectForJavaValueMethod = smalltalkObjectForJavaValueMethodForJavaType(constructor.getDeclaringClass());
		sourceBuf.append("  JVM invokeStatic: 'st/redline/compiler/SmalltalkGeneratorOfAdaptorOfAJavaClass' method:'" + smalltalkObjectForJavaValueMethod.getName() + "' matching: '(" + javaArgsSignature(smalltalkObjectForJavaValueMethod.getParameterTypes()) + ")Lst/redline/core/PrimObject;'.\n");
		
		// somehow put the st object to where it is supposed to go via smalltalkCodeToPullAnswerFromStack and also leave on stack
		sourceBuf.append("JVM dup.\n");
		sourceBuf.append(smalltalkCodeToPullAnswerFromStack +"\n");
		
		// call init
		sourceBuf.append( smalltalkToCallJavaMethod(false,void.class,"", constructor.getDeclaringClass(),
				"", // reciever is already on stack
				"<init>",constructor.getParameterTypes(),smalltalkCodeToPutArgumentsOnStack));
		return sourceBuf.toString();
		*/
	}
	
	
	
	private static String smalltalkToCallJavaMethod_putAnswerInTemp0__useCurrentReciever_ArgsInCollectionOnStack(Method javaMethod)
	{
		return smalltalkToCallJavaMethod("JVM putTemp: 0.", "JVM aload: 1.", javaMethod, ARGS_IN_COLLECTION_ON_STACK);
	}
	/*
	private static String smalltalkCodeToInvokeJavaMethodByByteCodeWithArgsInSmalltalkArrayAtArg0LeavingSmalltalkReturnIfAnyOnStack(Method m)
	{
		
		StringBuffer sourceBuf = new StringBuffer();
		boolean methodIsStatic = Modifier.isStatic( m.getModifiers());
		if(!methodIsStatic)
		{
			sourceBuf.append("  JVM aload: 1; checkcast: 'st/redline/core/PrimObject'.\n"); // put the reciever on the stack
			sourceBuf.append(smalltalkSourceToConvertASmalltalkVariableOnStackToTypeAndLeaveThatOnStack(m.getDeclaringClass())); // convert the receiver to java - PROBABLY WANT RECIEVER TYPE
		}
		
		sourceBuf.append(smalltalkSourceToCovertArgumentsAndPutOnStack(m.getParameterTypes()));
		String invoke = methodIsStatic?"invokeStatic":"invokeVirtual";
		sourceBuf.append("  JVM "+ invoke+ ": '" + fullyQualifiedJvmClassNameOfJavaClass(m.getDeclaringClass()) + "' method: '" + m.getName() + "' matching: '" + jvmMethodDescriptorForMethod(m) + "'.\n");


		boolean methodIsVoid = m.getReturnType().getName().equals("void");
		if (!methodIsVoid)
		{

			Method smalltalkObjectForJavaValueMethod = smalltalkObjectForJavaValueMethodForJavaType(m.getReturnType());// 'smalltalkObjectForJavaValue'

			sourceBuf.append("  JVM invokeStatic: 'st/redline/compiler/SmalltalkGeneratorOfAdaptorOfAJavaClass' method:'" + smalltalkObjectForJavaValueMethod.getName() + "' matching: '(" + javaArgsSignature(smalltalkObjectForJavaValueMethod.getParameterTypes()) + ")Lst/redline/core/PrimObject;';\n");
		}
		return sourceBuf.toString();
	}
	*/



	private static String jvmMethodDescriptorForMethod(String classname, String methodName)
	{
		return jvmMethodDescriptorForMethod(onlyPublicMethodNamed(classname, methodName));
	}

	private static String jvmMethodDescriptorForMethod(Method m)
	{
		return jvmMethodDescriptorFor(m.getReturnType(),m.getParameterTypes());
	}
	private static String jvmMethodDescriptorFor(Class<?> returnType, Class<?>[] parameterTypes)
	{
		StringBuffer jvmMethodDescriptorBuf = new StringBuffer();
		jvmMethodDescriptorBuf.append("(");
		for (Class<?> parameterType : parameterTypes)
		{
			jvmMethodDescriptorBuf.append(jvmParameterDescriptorForClass(parameterType));
		}
		jvmMethodDescriptorBuf.append(")");
		jvmMethodDescriptorBuf.append(jvmParameterDescriptorForClass(returnType));
		return jvmMethodDescriptorBuf.toString();
	}
	private static String javaArgsSignature(Class<?>[] parameterTypes)
	{
		String javaArgsSignature = "";
		for (Class<?> parameterType : parameterTypes)
		{
			javaArgsSignature = javaArgsSignature + jvmParameterDescriptorForClass(parameterType);
		}
		return javaArgsSignature;
	}

	private static String jvmParameterDescriptorForClass(Class<?> parameterTypeC)
	{
		String parameterType = parameterTypeC.getName();
		if (parameterType.contains("["))
		{
			return parameterType.replace("class ", "").replace(".", "/");
		}
		if (parameterType.indexOf('.') != -1)
		{
			return "L" + parameterType.replace(".", "/") + ";";
		}
		if (!PRIMITIVE_TO_SIGNATURE_TYPE.containsKey(parameterType))
		{
			throw new IllegalStateException("jvmParameterDescriptorForClass does not cope with class " + parameterTypeC.getName());
		}
		return PRIMITIVE_TO_SIGNATURE_TYPE.get(parameterType);
	}

	private static String methodSymbolComponentForType(Class<?> parameterTypeC)
	{
		String parameterType = parameterTypeC.getName();
		if (parameterType.startsWith("[L"))
		{
			if (parameterType.indexOf('.') != -1)
				return "ArrayOf" + parameterType.substring(parameterType.lastIndexOf('.') + 1).replace(";", "");
			else
				return "ArrayOf" + PRIMITIVE_TO_SIGNATURE_TYPE.get(parameterType);
		}

		if (parameterType.indexOf('.') != -1)
			return parameterType.substring(parameterType.lastIndexOf('.') + 1);
		return PRIMITIVE_TO_SIGNATURE_TYPE.get(parameterType);
	}

	private static String stringAfterLastOrEmptyString(String source, String delimiter)
	{
		int indexOfBeginOfDelimiter = source.lastIndexOf(delimiter);
		if (indexOfBeginOfDelimiter == -1)
		{
			return "";
		}
		return source.substring(indexOfBeginOfDelimiter + delimiter.length());
	}

	private static Class<?> classForName(String className)
	{
		try
		{
			return SmalltalkEnvironment.classLoader().loadClass(className);
		}
		catch (ClassNotFoundException e)
		{
			throw new IllegalStateException(className + " failed to be recognized as a valid java class");
		}
	}
	public static List<Method> methodsNamed(Class<?>c,String methodName)
	{
		List<Method> found = new ArrayList<Method>();
		for (Method m : c.getMethods())
		{
			if (m.getName().equals(methodName))
			{
				found.add(m);
			}
		}
		
		return found;
	}
	static Method methodNamed(Class<?> c, String methodName, Class<?>[] parameterTypes)
	{
		try
		{
			return c.getMethod(methodName, parameterTypes);
		}
		catch (NoSuchMethodException e)
		{
			throw new RedlineException(e);
		}
		catch (SecurityException e)
		{
			throw new RedlineException(e);
		}
	}

	
	public static Method onlyPublicMethodNamed(String className, String methodName)
	{
		return onlyPublicMethodNamed(classForName(className), methodName);
	}

	public static Method onlyPublicMethodNamed(Class<?> c, String methodName)
	{

		Method found = null;
		for (Method m : c.getMethods())
		{
			if (m.getName().equals(methodName))
			{
				if (found != null)
				{
					throw new IllegalStateException("did not expected to find more than one method" + methodName + " in class " + c.getName());
				}
				found = m;
			}
		}
		if (found == null)
		{
			throw new IllegalStateException("expected to find method" + methodName + " in class " + c.getName());
		}
		return found;
	}
	
	static Constructor<?> constructorToCallFor(Class<?> classOfConstructor,Object ... args)
	{
		Constructor<?>[] allPublicConstructors = classOfConstructor.getConstructors();
		List<Constructor<?>> constructorsMatchingArgs = new ArrayList<Constructor<?>>();
		loopthroughconstructors:
		for(Constructor<?> c : allPublicConstructors)
		{

			
			Class<?>[] pt = c.getParameterTypes();
			if(  c.getParameterTypes().length == args.length)
			{
				for(int i = 0; i< pt.length;i++)
				{
					if (!pt[i].isAssignableFrom(args[i].getClass()))
					{
						continue loopthroughconstructors  ;
					}	
				}
				constructorsMatchingArgs.add(c);
			}
			
		}
		if (constructorsMatchingArgs.isEmpty()) 
		{ 
			String msg = "failed to find constructor in class=" + classOfConstructor.getName()  +" with " + args.length + " args callable for arguments types";
			for(int i = 0; i < args.length;i++)
			{
				msg= msg + " " +args[i].getClass();
			}
			
			throw new IllegalStateException(msg );
		}
		if (constructorsMatchingArgs.size()> 1) { throw new IllegalStateException("dont currently cope with finding more than 1 possible constructor "  );}
		return constructorsMatchingArgs.get(0);
	}
	
	static Map<Method,ArrayList<Object>> selectorsKeyedbyRecieverClassMethodNameAndArgumentClasses = new HashMap<Method,ArrayList<Object>>(); // not used yet
	static Method methodToCallFor(Object receiver, String methodName,Object ... args)
	{
		Class<?> recieverClass = receiver.getClass();
		Method[] allPublicMethods = recieverClass.getMethods();
		List<Method> methodsMatchingNameAndArgs = new ArrayList<Method>();
		loopthroughmethods:
		for(Method m : allPublicMethods)
		{

			if (m.getName().equals(methodName))
			{
				Class<?>[] pt = m.getParameterTypes();
				//System.out.println("methodName="+ methodName + " m.getParameterTypes().length=" + m.getParameterTypes().length+ " args.length="+args.length);
				if(  m.getParameterTypes().length == args.length)
				{
					for(int i = 0; i< pt.length;i++)
					{
						if (!pt[i].isAssignableFrom(args[i].getClass()))
						{
							System.out.println("methodName="+ methodName + " pt[i]="+pt[i] +" is not isAssignableFrom args[i].getClass()=" + args[i].getClass());
							continue loopthroughmethods  ;
						}	
					}
					methodsMatchingNameAndArgs.add(m);
				}
			}
		}
		if (methodsMatchingNameAndArgs.isEmpty()) 
		{ 
			String msg = "failed to find selector for methodnamed=" + methodName  +" with " + args.length + " args callable for arguments types";
			for(int i = 0; i < args.length;i++)
			{
				msg= msg + " " +args[i].getClass();
			}
			
			throw new IllegalStateException(msg );
		}
		if (methodsMatchingNameAndArgs.size()> 1) { throw new IllegalStateException("dont currently cope with finding more than 1 possible method for methodnamed " + methodName  );}
		return methodsMatchingNameAndArgs.get(0);
	}
	public static Object invokeRightJavaConstructorAllArgs(Object  argsWithClass[])
	{
		Class<?> classOfObjectToCreate = (Class<?>) argsWithClass[argsWithClass.length-1];
		Object[] args = Arrays.copyOf(argsWithClass, argsWithClass.length-1);
		return invokeRightJavaConstructor(classOfObjectToCreate,args);
	
	}
	public static Object invokeRightJavaConstructorForClassNamedWithArgsInList(String classNameOfObjectToCreate,List<Object>  args)
	{
		//System.out.println("in invokeRightJavaConstructorForClassNamedWithArgsInList");
		Object rtn =  invokeRightJavaConstructor(classForName(classNameOfObjectToCreate), args.toArray());
		//System.out.println("end invokeRightJavaConstructorForClassNamedWithArgsInList");
		return rtn;
	}
	public static Object invokeRightJavaConstructorArgsInList(Class<?> classOfObjectToCreate,List<Object>  args)
	{
		return invokeRightJavaConstructor(classOfObjectToCreate, args.toArray());
	}
	public static Object invokeRightJavaConstructor(Class<?> classOfObjectToCreate,Object ... args)
	{
		Constructor<?> c = constructorToCallFor(classOfObjectToCreate,args);
		try
		{
			Object rtn = c.newInstance(args);
			return rtn;
		}
		catch (Exception e)
		{
			throw new RedlineException(e);
		}
		
	}
	public static Object invokeRightJavaMethodAllArgs(Object  argsWithReceiverAndMethodName[])
	{
		Object receiver = argsWithReceiverAndMethodName[argsWithReceiverAndMethodName.length-2];
		String methodName = (String) argsWithReceiverAndMethodName[argsWithReceiverAndMethodName.length-1];
		Object[] args = Arrays.copyOf(argsWithReceiverAndMethodName, argsWithReceiverAndMethodName.length-2);
		return invokeRightJavaMethod(receiver,methodName,args);
	
	}
	public static Object invokeRightJavaMethodArgsInList(Object receiver, String methodName,List<Object>  args)
	{
		return invokeRightJavaMethod(receiver,methodName, args.toArray());
	}
	public static Object invokeRightJavaMethod(Object receiver, String methodName,Object ... args)
	{
		Method m = methodToCallFor(receiver,methodName,args);
		try
		{
			System.out.println("invoking methodname="+ m.getName() + " on reiever=" + receiver + " with args=" + args);
			Object rtn = m.invoke(receiver, args);
			return rtn;
		}
		catch (Exception e)
		{
			throw new RedlineException(e);
		}
		
	}
	
	public static String smalltalkSelectorForMethodThatWrapsRightJavaConstructorForClassNamed(String classOfObjectToCreate, Object ... args)
	{
		return smalltalkSelectorForMethodThatWrapsRightJavaConstructorFor(classForName(classOfObjectToCreate), args);
	}
	public static String smalltalkSelectorForMethodThatWrapsRightJavaConstructorFor(Class<?> classOfObjectToCreate, Object ... args)
	{
		Constructor<?> c = constructorToCallFor(classOfObjectToCreate,args);
		return methodNameForMethodAdaptingIndividualConstructor(c);
	}
	public static Object smalltalkSelectorForMethodThatWrapsRightJavaMethodAllArgs(Object[]  argsWithReceiverAndMethodName)
	{
		Object receiver = argsWithReceiverAndMethodName[argsWithReceiverAndMethodName.length-2];
		String methodName = (String) argsWithReceiverAndMethodName[argsWithReceiverAndMethodName.length-1];
		Object[] args = Arrays.copyOf(argsWithReceiverAndMethodName, argsWithReceiverAndMethodName.length-2);
		return smalltalkSelectorForMethodThatWrapsRightJavaMethodFor(receiver,methodName,args);
	
	}
	
	public static String smalltalkSelectorForMethodThatWrapsRightJavaMethodFor(Object receiver, String methodName,Object ... args)
	{
		Method m = methodToCallFor(receiver,methodName,args);
		return methodNameForMethodAdaptingIndividualMethod(m);
	}
	
	public static String smalltalkCodeToDump(String smalltalkExpression, String typeCurrentlyOnStack)
	{ // DOES not work yet
		Method dumpMethod = onlyPublicMethodNamed(SmalltalkGeneratorOfAdaptorOfAJavaClass.class, "dumpItemOnStackWithoutAlteringStack") ;
		return "[|a|\n a := " + smalltalkExpression + ".\n" +
				smalltalkToCallStaticJavaMethod("", dumpMethod, "JVM atTemp: 0.") +
		 " ] value. \n" +
		"  JVM checkcast: '"+ typeCurrentlyOnStack + "'. \n";	
		
	}
    
	static public Object dumpItemOnStackWithoutAlteringStack(Object x)
	{
		System.out.println("\n*********dump " + x);
		return x;
	}
    
	static public Object[] convertToArrayOfObjects(Object toConvert)
	{
		if (Object[].class.isAssignableFrom(toConvert.getClass())) {
            return (Object[]) toConvert;
        }

		if(Collection.class.isAssignableFrom(toConvert.getClass()))
		{
			Collection c = (Collection) toConvert;
			Object[] converted = new Object[c.size()];
			int i=0;
			for(Object e : c)
			{
				converted[i] = e;
				i++;
			}
			return converted;
		}
        throw new RedlineException("should not happen: " + toConvert + " " + toConvert.getClass().getName());
	}
}
