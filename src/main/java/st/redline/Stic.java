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

public class Stic {

	public static void main(String[] args) throws Exception {
		new Stic().invokeWith(args[0], args);
	}

	public Stic() {
		initializeClassLoader();
	}

	private void initializeClassLoader() {
		Thread.currentThread().setContextClassLoader(createClassLoader());
	}

	private ClassLoader createClassLoader() {
		return new SmalltalkClassLoader(Thread.currentThread().getContextClassLoader());
	}

	private ClassLoader classLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	public void invokeWith(String className, String[] args) throws Exception {
		ProtoObject.primitiveMain(createClassInstance(className), args);
	}

	private ProtoObject createClassInstance(String className) throws Exception {
		return (ProtoObject) loadClass(className).newInstance();
	}

	private Class loadClass(String className) throws Exception {
		return Class.forName(className, true, classLoader());
	}
}
