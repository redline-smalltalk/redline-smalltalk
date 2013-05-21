"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Transcript.

"categtory: streaming"

+ show: anObject
    self stdout: anObject.

+ cr
    JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
        invokeVirtual: 'java/io/PrintStream' method: 'println' matching: '()V'.

+ stdout: string
    JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
        arg: 0;
        invokeVirtual: 'java/io/PrintStream' method: 'print' matching: '(Ljava/lang/Object;)V'.
