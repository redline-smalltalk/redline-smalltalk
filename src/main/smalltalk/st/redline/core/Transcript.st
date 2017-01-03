"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Transcript.

"category: streaming"

+ show: anObject
    self stdout: anObject.

+ stdout: aString
    JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
        argLoad: 0;
        invokeVirtual: 'java/io/PrintStream' method: 'print' matching: '(Ljava/lang/Object;)V';
        aload: 1.

+ cr
    JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
        invokeVirtual: 'java/io/PrintStream' method: 'println' matching: '()V';
        aload: 1.
