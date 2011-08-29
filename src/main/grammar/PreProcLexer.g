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
lexer grammar PreProcLexer;

options {

   language=Java;  // Default

   // Tell ANTLR to make the generated lexer class extend the
   // the named class, which is where any supporting code and 
   // variables will be placed.
   //
   superClass = AbstractPreProcLexer;
}

// What package should the generated source exist in?
//
@header {

    package st.redline.compiler;
}


// A preprocessing line only kciks in if there is a + or - in column
// position 0. The gated semantic predicate (?=>) ensures that we
// don't select this rule unless we are at column 0
//
PREPROC
        : {getCharPositionInLine() == 0}?=>
        (
            ('import:')=>       // We must see all of this, or we ignore...
            
                'import:' 
                {
                    resetMethods();         // Reset as nothing needs closing on this one.
                    sb.append("self import:"); 
                }
                        
                (c=~('\n'|'\r') { sb.append((char)$c); })* 
                
                ('\r' { sb.append("\r"); } )? 
                '\n'  // All is good - proper line ending
                      {
                        sb.append("\n");
                      }

          |  v = ( '+'| '-')       // Only these are predicted.
            {
                if (!firstMethod()) {
                    sb.append("] ");   // DONT append this if this is first +/- seen.
                }
                sb.append(methodName);

                if ($v == '+') {
                    sb.append(" class");
                }
                sb.append(" >> ");
                haveMethods();
            }

            (' '| '\t')* // Eat any whitespace

            (
                (
                    // We can have:

                    // A single WORD, which is termed a unarySelector
                    // A word, a colon and a value, which is a keywordSelector
                    // A special binary character, then a word, which is a binary selector
                    // 
                    (
                        w1=WORD        // A single word, but this may be a keywordSelector

                            ( (' '| '\t')=>(' '| '\t'))*  // Eat any whitespace

                            (
                                  ':'     // Should be a keywordSelector

                                  ( (' '| '\t')=>(' '| '\t'))*  // Eat any whitespace

                                  (
                                      (WORD)=> w2=WORD     // This is a valid keywordSelector

                                      {
                                        // Write out the parts of the keyword Selector
                                        //
                                        sb.append($w1.text);        // First element of the selector
                                        sb.append(": ");
                                        sb.append($w2.text);        // Second element of the selector
                                        sb.append(' ');
                                      }

                                    | // *** ERROR - Malformed keyword selector ***
                                      // 
                                      {
                                        System.err.println("Malformed keyword selector '" + $w1.text + " at line " + $line);
                                      }
                                  )

                              | // Just a unarySelector only
                                //
                                {
                                    sb.append($w1.text);
                                }
                            )
                       | b=BINARY

                            ( (' '| '\t')=>(' '| '\t'))*  // Eat any whitespace

                            (
                                  (WORD)=> w1=WORD        // Valid binary selector

                                  {
                                        // Write out the parts of the binary selector
                                        //
                                        sb.append($b.text);
                                        sb.append(" ");
                                        sb.append($w1.text);
                                  }

                                | // *** ERROR - Malformed binary selector ***
                                      {
                                        System.err.println("Malformed binary selector '" + $b.text + " at line " + $line);
                                      }
                            )
                    )

                    ( (' '| '\t')=>(' '| '\t'))*  // Eat any whitespace

                )+  // May repeat one or more times

                    {
                        // Closing info
                        //                                        
                        sb.append(" [");
                    }

                (
                      ('\r' { sb.append("\r"); } )? 
                      '\n'  // All is good - proper line ending
                      {
                        sb.append("\n");
                      }

                    | // *** ERROR - Malformed selectors at line end
                    {
                        System.err.println("Malformed selector(s) at end of line " + $line);

                        // Could do an input consume until LA(1) == \r or \n here
                        //
                    }
                )

              | // *** Error, + or - without any valid selectors ***
                  {
                    System.err.println("Malformed pre-processor directive at line " + $line);
                  }
            )
        )
        ;

fragment
WORD  
    : // ASCII Words but may become significant if we are in
      // a pre-processing line mode
      //
     ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')+
    ;

fragment
BINARY
    : ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',')+ 
    ;

// --------------------------------------------------------------------------
// Elements that are just added to the output
//

WS  :   ( 
              ' '
            | '\t'
        )+
        { sb.append($text); }
    ;

NL  :   ( '\n' | '\r')+ { sb.append($text); } 
    ;

OTHER
    : . { sb.append($text); } 
    ;
