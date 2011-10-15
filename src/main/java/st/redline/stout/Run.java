/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import org.mortbay.jetty.Handler;
import org.mortbay.jetty.Request;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.AbstractHandler;
import st.redline.CommandLine;
import st.redline.Primitives;
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
		requestSymbol = Primitives.symbol(httpServletRequest, "Request");
		forwardSymbol = Primitives.symbol(httpServletRequest, "Forward");
		includeSymbol = Primitives.symbol(httpServletRequest, "Include");
		errorSymbol = Primitives.symbol(httpServletRequest, "Error");
		httpVerbMap.put("GET", Primitives.symbol(httpServletRequest, "GET"));
		httpVerbMap.put("PUT", Primitives.symbol(httpServletRequest, "PUT"));
		httpVerbMap.put("POST", Primitives.symbol(httpServletRequest, "POST"));
		httpVerbMap.put("OPTIONS", Primitives.symbol(httpServletRequest, "OPTIONS"));
		httpVerbMap.put("DELETE", Primitives.symbol(httpServletRequest, "DELETE"));
		httpVerbMap.put("TRACE", Primitives.symbol(httpServletRequest, "TRACE"));
		httpVerbMap.put("HEAD", Primitives.symbol(httpServletRequest, "HEAD"));
	}

	private static void startServer(CommandLine commandLine) throws Exception {
		server = new Server(8080);
		server.setHandler(initialHandler(commandLine));
		server.start();
		server.join();
	}

	private static ProtoObject object(CommandLine commandLine) throws Exception {
		return null; // Primitives.send(stic.invoke((String) commandLine.arguments().get(0)), "new", null);
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
//				Primitives.send(receiver,
//						method(request.getMethod()),
//						string(target),
//						request(request),
//						response(response),
//						dispatch(dispatch),
//						"handle:on:with:and:and:",
//						null);
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
				return Primitives.newWith(httpServletRequest, request);
			}

			private ProtoObject response(HttpServletResponse response) {
				return Primitives.newWith(httpServletResponse, response);
			}

			private ProtoObject method(String value) {
				return httpVerbMap.get(value);
			}

			private ProtoObject string(String value) {
				return Primitives.string(receiver, value);
			}
		};
	}
}
