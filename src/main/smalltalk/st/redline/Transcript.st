" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Transcript.

+ show: anObject
  self stdout: anObject.

+ cr
    JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
        invokeVirtual: 'java/io/PrintStream' method: 'println' returning: '()V'.

+ stdout: string
    JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
        aload: 3;
        invokeVirtual: 'java/io/PrintStream' method: 'print' returning: '(Ljava/lang/Object;)V'.
