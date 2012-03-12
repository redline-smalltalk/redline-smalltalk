/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import org.mortbay.jetty.Handler;
import org.mortbay.jetty.Request;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.AbstractHandler;

import st.redline.CommandLine;
import st.redline.PrimObject;
import st.redline.PrimObjectMetaclass;
import st.redline.Stic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class Run extends Stic {

	static Server server;

	public static void main(String[] args) throws Exception {
		startServer(args);
	}

	public Run(CommandLine commandLine) throws Exception {
		super(commandLine);
	}

	static void startServer(String[] args) throws Exception {
		server = new Server(8080);
		server.setHandler(initialHandler(args));
		server.start();
		server.join();
	}

	static Handler initialHandler(final String[] args) throws Exception {
		return new AbstractHandler() {
			public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
					throws IOException, ServletException {
				Handler handler;
				try {
					handler = continuingHandler(objectFrom(args));
				} catch (Exception e) {
					throw new ServletException(e);
				}
				server.setHandler(handler);
				handler.handle(target, request, response, dispatch);
			}

			private PrimObject objectFrom(String[] args) throws Exception {
				PrimObjectMetaclass eigenClass = (PrimObjectMetaclass) invokeWith(Run.class, args);
				return PrimObject.CLASSES.get(eigenClass.getClass().getName()).perform("new");
			}
		};
	}

	static Handler continuingHandler(final PrimObject receiver) {
		return new AbstractHandler() {
			public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
					throws IOException, ServletException {
				try {
					System.out.println("continuingHandler() " + receiver + " " + request.getMethod() + " " + target);
					receiver.perform("ping");
				} catch (Exception e) {
					throw new ServletException(e);
				}
			}
		};
	}
}
