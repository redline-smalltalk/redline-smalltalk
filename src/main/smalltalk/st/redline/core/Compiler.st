" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Compiler.

+ evaluate: source
    "evaluate the Smalltalk 'source' and answer the result of the evaluated source."
    JVM arg: 0;
        invokeStatic: 'st/redline/core/Evaluator' method: 'evaluate' matching: '(Lst/redline/core/PrimObject;)Lst/redline/core/PrimObject;'.
