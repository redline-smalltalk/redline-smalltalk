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

import st.redline.bootstrap.Bootstrapper;

import java.io.File;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Provides an entry point for Redline Smalltalk.
 */
public class Smalltalk extends ClassLoader {

	protected static final String REDLINE_PACKAGE = "redline";
	public static final String CURRENT_FILE = "CURRENT_FILE";

	private static final String NIL = "nil";
	private static final String TRUE = "true";
	private static final String FALSE = "false";
	private static final String ARRAY = "Array";
	private static final String NEW_SELECTOR = "new";

	private final Environment environment;
	private final Interpreter interpreter;
	private final Map<String, RObject> cachedObjects;
	private final ClassFinder classFinder;
	private final Map<String, String> internedSymbols;

	private RObject trueInstance;
	private RObject falseInstance;
	private RObject nilInstance;
	private boolean verboseOn;

	public static Smalltalk with(Environment environment) {
		if (environment == null)
			throw new MissingArgumentException();
		return new Smalltalk(environment, currentClassLoader());
	}

	public Smalltalk(Environment environment, ClassLoader parentClassLoader) {
		super(parentClassLoader);
		this.environment = environment;
		interpreter = new Interpreter(this);
		cachedObjects = new HashMap<String, RObject>();
		classFinder = new ClassFinder(this);
		internedSymbols = new HashMap<String, String>();
		initialize();
		bootstrap();
	}

	private void initialize() {
		verboseOn = verboseRequested();
		Thread.currentThread().setContextClassLoader(this);
	}

	private static ClassLoader currentClassLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	public static Smalltalk instance() {
		return (Smalltalk) currentClassLoader();
	}

	private void bootstrap() {
		new Bootstrapper(this).bootstrap();
	}

	public boolean verboseOn() {
		return verboseOn;
	}

	private boolean verboseRequested() {
		return commandLine().verboseRequested();
	}

	public Object evaluate(SourceFile sourceFile) {
		trackFile(sourceFile);
		try {
			return interpreter().interpret(sourceFile);
		} finally {
			untrackFile();
		}
	}

	protected void evaluate(File sourceFile) {
		evaluate(new SourceFile(sourceFile.getAbsolutePath()));
	}

	private void untrackFile() {
		environment.remove(CURRENT_FILE);
	}

	private void trackFile(SourceFile sourceFile) {
		environment.put(CURRENT_FILE, sourceFile);
	}

	protected Interpreter interpreter() {
		return interpreter;
	}

	public PrintWriter standardOutput() {
		return environment.standardOutput();
	}

	public PrintWriter errorOutput() {
		return environment.errorOutput();
	}

	public List<String> sourcePaths() {
		return commandLine().sourcePaths();
	}

	public CommandLine commandLine() {
		return environment.commandLine();
	}

	public Environment environment() {
		return environment;
	}

	public Object defineClass(byte[] classBytes, boolean instantiate) {
		try {
			if (instantiate)
				return defineClass(null, classBytes, 0, classBytes.length).newInstance();
			return defineClass(null, classBytes, 0, classBytes.length);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public RObject createBlock(String blockClassName) {
		System.out.println("createBlock() " + blockClassName);
		try {
			return (RBlock) loadClass(blockClassName).newInstance();
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}

	public RObject numberFromPrimitive(String value) {
		return createObjectWithPrimitiveValue("Number", valueAsNumber(value));
	}

	private Number valueAsNumber(String value) {
		return new Integer(value);
	}

	public RObject stringFromPrimitive(String value) {
		return createObjectWithPrimitiveValue("String", value);
	}

	public RObject characterFromPrimitive(String value) {
		return createObjectWithPrimitiveValue("Character", value);
	}

	public RObject symbolFromPrimitive(String value) {
		return createObjectWithPrimitiveValue("Symbol", internedSymbol(value));
	}

	private RObject createObjectWithPrimitiveValue(String objectName, Object primitiveValue) {
		RObject anInstance = newInstanceOf(objectName);
		anInstance.primitiveValue(primitiveValue);
		return anInstance;
	}

	private String internedSymbol(String symbol) {
		if (internedSymbols.containsKey(symbol))
			return internedSymbols.get(symbol);
		internedSymbols.put(symbol, symbol);
		return symbol;
	}

	public RObject primitiveAt(String objectName) {
		System.out.println("primitiveAt() " + objectName);
		if (isCachedObject(objectName))
			return cachedObject(objectName);
		String packageAndClassName = REDLINE_PACKAGE + "." + objectName;
		if (isCachedObject(packageAndClassName))
			return cachedObject(packageAndClassName);
		resolveClassObject(objectName);
		return cachedObject(objectName);
	}

	private boolean isCachedObject(String className) {
		return cachedObjects.containsKey(className);
	}

	private RObject cachedObject(String name) {
		RObject object = cachedObjects.get(name);
		if (object == null)
			object = cachedObjects.get(NIL);
		return object;
	}

	public RObject cachedObject0(String name) {
		return cachedObjects.get(name);
	}

	protected boolean resolveClassObject(String name) {
		try {
			System.out.println("resolveClassObject() " + name);
			loadClass(name).newInstance();
			return true;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}

	public Class<?> loadClass(String name) throws ClassNotFoundException {
		System.out.println("loadClass() " + name);
		return super.loadClass(name);
	}

	protected Class<?> findClass(String name) throws ClassNotFoundException {
		System.out.println("findClass() " + name);
		Class<?> cls = classFinder.find(name);
		if (cls != null)
			return cls;
		return super.findClass(name);
	}

	public void primitiveAtPut(String name, RObject object) {
		System.out.println("primitiveAtPut() " + name + " " + object);
		cachedObjects.put(name, object);
	}

	public RObject nilInstance() {
		if (nilInstance != null)
			return nilInstance;
		nilInstance = primitiveAt(NIL);
		return nilInstance;
	}

	public RObject trueInstance() {
		if (trueInstance != null)
			return trueInstance;
		trueInstance = newInstanceOf(TRUE);
		return trueInstance;
	}

	public RObject falseInstance() {
		if (falseInstance != null)
			return falseInstance;
		falseInstance = newInstanceOf(FALSE);
		return falseInstance;
	}

	public RObject arrayInstance() {
		return newInstanceOf(ARRAY);
	}

	private RObject lookupMustHaveObject(String name) {
		RObject object = primitiveAt(name);
		if (object != null)
			return object;
		throw new IllegalStateException("Can't find required object '" + name + "'.");
	}

	private RObject newInstanceOf(String className) {
		RObject aClass = lookupMustHaveObject(className);
		return RObject.send(aClass, NEW_SELECTOR, null);
	}
}
