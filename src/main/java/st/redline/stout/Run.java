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
package st.redline.stout;

import org.mortbay.jetty.Handler;
import org.mortbay.jetty.Request;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.AbstractHandler;
import st.redline.CommandLine;
import st.redline.ProtoObject;
import st.redline.Stic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.channels.NonWritableChannelException;
import java.util.Hashtable;
import java.util.Map;

public class Run {

	private static Server server;
	private static Stic stic;
	private static ProtoObject httpServletRequest;
	private static ProtoObject httpServletResponse;
	private static ProtoObject requestSymbol;
	private static ProtoObject forwardSymbol;
	private static ProtoObject includeSymbol;
	private static ProtoObject errorSymbol;
	private static Map<String, ProtoObject> httpVerbMap = new Hashtable<String, ProtoObject>();

	public static void main(String args[]) throws Exception {
		CommandLine commandLine = Stic.createCommandLineWith(args);
		if (commandLine.haveNoArguments()) {
			commandLine.printHelp(new PrintWriter(System.out));
			return;
		}
		startSmalltalk(commandLine);
		startServer(commandLine);
	}

	private static void startSmalltalk(CommandLine commandLine) throws Exception {
		stic = new Stic(commandLine);
		httpServletRequest = stic.invoke("st.redline.stout.HttpServletRequest");
		httpServletResponse = stic.invoke("st.redline.stout.HttpServletResponse");
		requestSymbol = ProtoObject.primitiveSymbol(httpServletRequest, "Request");
		forwardSymbol = ProtoObject.primitiveSymbol(httpServletRequest, "Forward");
		includeSymbol = ProtoObject.primitiveSymbol(httpServletRequest, "Include");
		errorSymbol = ProtoObject.primitiveSymbol(httpServletRequest, "Error");
		httpVerbMap.put("GET", ProtoObject.primitiveSymbol(httpServletRequest, "GET"));
		httpVerbMap.put("PUT", ProtoObject.primitiveSymbol(httpServletRequest, "PUT"));
		httpVerbMap.put("POST", ProtoObject.primitiveSymbol(httpServletRequest, "POST"));
		httpVerbMap.put("OPTIONS", ProtoObject.primitiveSymbol(httpServletRequest, "OPTIONS"));
		httpVerbMap.put("DELETE", ProtoObject.primitiveSymbol(httpServletRequest, "DELETE"));
		httpVerbMap.put("TRACE", ProtoObject.primitiveSymbol(httpServletRequest, "TRACE"));
		httpVerbMap.put("HEAD", ProtoObject.primitiveSymbol(httpServletRequest, "HEAD"));
	}

	private static void startServer(CommandLine commandLine) throws Exception {
		server = new Server(8080);
		server.setHandler(initialHandler(commandLine));
		server.start();
	}

	private static ProtoObject object(CommandLine commandLine) throws Exception {
		return ProtoObject.primitiveSend(stic.invoke((String) commandLine.arguments().get(0)), "new", null);
	}

	private static Handler initialHandler(final CommandLine commandLine) throws Exception {
		return new AbstractHandler() {
			public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
				throws IOException, ServletException {
				Handler handler;
				try {
					handler = continuingHandler(object(commandLine));
				} catch (Exception e) {
					throw new ServletException(e);
				}
				server.setHandler(handler);
				handler.handle(target, request, response, dispatch);
			}
		};
	}

	private static Handler continuingHandler(final ProtoObject receiver) {
		return new AbstractHandler() {
			public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
				throws IOException, ServletException {
				ProtoObject.primitiveSend(receiver,
						method(request.getMethod()),
						string(target),
						request(request),
						response(response),
						dispatch(dispatch),
						"handle:on:with:and:and:",
						null);
			}

			private ProtoObject dispatch(int dispatch) {
				switch (dispatch) {
					case 1:
						return requestSymbol;
					case 2:
						return forwardSymbol;
					case 4:
						return includeSymbol;
					case 8:
						return errorSymbol;
					default:
						throw new IllegalStateException("Dispatch value of " + dispatch + " not understood.");
				}
			}

			private ProtoObject request(HttpServletRequest request) {
				return ProtoObject.primitiveNewWith(httpServletRequest, request);
			}

			private ProtoObject response(HttpServletResponse response) {
				return ProtoObject.primitiveNewWith(httpServletResponse, response);
			}

			private ProtoObject method(String value) {
				return httpVerbMap.get(value);
			}

			private ProtoObject string(String value) {
				return ProtoObject.primitiveString(receiver, value);
			}
		};
	}
}
