/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.antlr.runtime.CharStream;
import org.antlr.runtime.Lexer;
import org.antlr.runtime.RecognizerSharedState;

/**

 * This is the super class for the lexer. It is extended by the lexer class
 * generated from TLexer.g.
 *
 * Do not place code and declarations in the lexer .g files, use
 * a superclass like this and place all the support methods and
 * error overrides etc in the super class. This way you will keep
 * the lexer grammar clean and hunky dory.
 *
 * @author Jim Idle - Temporal Wave LLC (jimi@idle.ws)
 * @author James Ladd - Redline
 */
public abstract class AbstractPreProcLexer
    extends Lexer

{
    /**
     * Default constructor for the lexer, when you do not yet know what
     * the character stream to be provided is.
     */
    public AbstractPreProcLexer() {
    }

    /**
     * Create a new instance of the lexer using the given character stream as
     * the input to lex into tokens.
     *
     * @param input A valid character stream that contains the ruleSrc code you
     *              wish to compile (or lex at least)
     */
    public AbstractPreProcLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }

    /**
     * Internal constructor for ANTLR - do not use.
     *
     * @param input The character stream we are going to lex
     * @param state The shared state object, shared between all lexer components
     */
    public AbstractPreProcLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);
    }

    /**
     * Used internally by the lexer to build the pre-processed output stream
     */
    StringBuilder sb;
    
    /**
     * Name of the input file
     */
    String methodName;

	boolean haveMethods = false;

	public void resetMethods() {
		haveMethods = false;
	}

	public void haveMethods() {
		haveMethods = true;
	}

	public boolean firstMethod() {
		return !haveMethods;
	}

    /**
     * Initialize the pre-processor according to the input size.
     * 
     */
    public void initPreProc(String name) {
            
        // Assume an expansion of 20%, which is just a guess, but is
        // probably enough for most pre-processing situations.
        //
        sb = new StringBuilder( (int)(input.size() * 1.20));   
        
        // See if we have the file name?
        //
        methodName = name; // input.getSourceName();
        
//        if  (methodName == null || methodName.isEmpty()) {
//
//            methodName = "unknownMethod";
//        }
//        else {
//
//            // Massage the name
//            //
//            methodName = methodName.substring(methodName.lastIndexOf(File.separatorChar)+1); // Remove paths
//
//            if (methodName.lastIndexOf('.') != -1) {
//
//                methodName = methodName.substring(0, methodName.lastIndexOf('.'));
//            }
//        }
    }
   
    /**
     * Provide a reference to the internal StringBuilder in case it needs
     * to be manipulated outside the pre-processor.
     * 
     * @return A reference to the StringBuilder that is building the output
     *         stream.
     */
    public StringBuilder getPreProcBuilder() {
        return sb;
    }
    
    /**
     * Provide the pre-processed input in String form.
     * 
     * @return 
     */
    public String getOutput() {
	    if (haveMethods)
		    sb.append("]");
        return sb.toString();
    }
}

