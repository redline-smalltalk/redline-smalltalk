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

import java.io.PrintWriter;

public class Stic {

	public static void main(String[] args) throws Exception {
		invokeWith(args);
	}

	public static ProtoObject invokeWith(String[] args) throws Exception {
		CommandLine commandLine = createCommandLineWith(args);
		if (commandLine.haveNoArguments()) {
			commandLine.printHelp(new PrintWriter(System.out));
			return null;
		}
		return new Stic(commandLine).invoke((String) commandLine.arguments().get(0));
	}

	public static CommandLine createCommandLineWith(String[] args) {
		return new CommandLine(args);
	}

	public Stic(CommandLine commandLine) {
		initializeClassLoader(commandLine);
		bootstrap();
	}

	private void bootstrap() {
		classLoader().bootstrap();
	}

	private void initializeClassLoader(CommandLine commandLine) {
		Thread.currentThread().setContextClassLoader(createClassLoader(commandLine));
	}

	private ClassLoader createClassLoader(CommandLine commandLine) {
		return new SmalltalkClassLoader(Thread.currentThread().getContextClassLoader(), commandLine);
	}

	private SmalltalkClassLoader classLoader() {
		return SmalltalkClassLoader.instance();
	}

	public ProtoObject invoke(String className) throws Exception {
		ProtoObject root = protoObjectInstance();
		root.name("<root>");
		return createClassInstance(root, className);
	}

	private ProtoObject createClassInstance(ProtoObject root, String className) throws Exception {
		return ProtoObject.primitiveResolveObject(root, className);
	}

	private ProtoObject protoObjectInstance() throws ClassNotFoundException, IllegalAccessException, InstantiationException {
		return (ProtoObject) classLoader().loadClass("st.redline.ProtoObject").newInstance();
	}
}
