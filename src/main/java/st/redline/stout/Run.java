/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import org.mortbay.jetty.Handler;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.AbstractHandler;
import st.redline.CommandLine;
import st.redline.PrimObject;
import st.redline.Stic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Hashtable;
import java.util.Map;

public class Run {

	private static Server server;
	private static Stic stic;
	private static PrimObject httpServletRequest;
	private static PrimObject httpServletResponse;
	private static PrimObject requestSymbol;
	private static PrimObject forwardSymbol;
	private static PrimObject includeSymbol;
	private static PrimObject errorSymbol;
	private static Map<String, PrimObject> httpVerbMap = new Hashtable<String, PrimObject>();

	public static void main(String args[]) throws Exception {
		CommandLine commandLine = Stic.createCommandLineWith(args);
		if (commandLine.haveNoArguments()) {
			commandLine.printHelp(new PrintWriter(System.out));
			return;
		}
		startSmalltalk(commandLine);
		startServer(commandLine);
	}

	public static RouterRegistry createRouterRegistry() {
	    return new RouterRegistryImpl(new RouterFactoryImpl(new RequestPathSpecificationFactoryImpl()));
	}

	private static void startSmalltalk(CommandLine commandLine) throws Exception {
		stic = new Stic(commandLine);
		httpServletRequest = stic.invoke("st.redline.stout.HttpServletRequest");
		httpServletResponse = stic.invoke("st.redline.stout.HttpServletResponse");
		requestSymbol = symbol("Request");
		forwardSymbol = symbol("Forward");
		includeSymbol = symbol("Include");
		errorSymbol = symbol("Error");
		httpVerbMap.put("GET", symbol("GET"));
		httpVerbMap.put("PUT", symbol("PUT"));
		httpVerbMap.put("POST", symbol("POST"));
		httpVerbMap.put("OPTIONS", symbol("OPTIONS"));
		httpVerbMap.put("DELETE", symbol("DELETE"));
		httpVerbMap.put("TRACE", symbol("TRACE"));
		httpVerbMap.put("HEAD", symbol("HEAD"));
	}

    private static PrimObject symbol(String value) {
        return PrimObject.symbol(value);
    }

    private static void startServer(CommandLine commandLine) throws Exception {
		server = new Server(8080);
		server.setHandler(initialHandler(commandLine));
		server.start();
		server.join();
	}

	private static PrimObject object(CommandLine commandLine) throws Exception {
		return stic.invoke((String) commandLine.arguments().get(0)).perform("new");
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

	private static Handler continuingHandler(final PrimObject receiver) {
		return new AbstractHandler() {
			public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
                    throws IOException, ServletException {
				try {
                    System.out.println("continuingHandler() " + receiver + " " + request.getMethod() + " " + target);
                    receiver.perform(
                            method(request.getMethod()),
                            string(target),
                            request(request),
                            response(response),
                              dispatch(dispatch),
                              "handle:on:with:and:and:");
				} catch (ClassNotFoundException e) {
					throw new ServletException(e);
				}
			}

			private PrimObject dispatch(int dispatch) {
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

			private PrimObject request(HttpServletRequest request) throws ClassNotFoundException {
				return newWith(httpServletRequest, request);
			}

			private PrimObject response(HttpServletResponse response) throws ClassNotFoundException {
				return newWith(httpServletResponse, response);
			}

            private PrimObject newWith(PrimObject receiver, Object value) {
                return receiver.perform("new").with(value);
            }

			private PrimObject method(String value) {
				return httpVerbMap.get(value);
			}

			private PrimObject string(String value) throws ClassNotFoundException {
				return PrimObject.string(value);
			}
		};
	}
}
