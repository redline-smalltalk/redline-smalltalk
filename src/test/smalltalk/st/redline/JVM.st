
"This is an example of direct bytecode manipulation from Smalltalk."

JVM getstatic: 'java/lang/system' named: 'out' as: 'Ljava/io/PrintStream;';
    load: 'hello smalltalk';
    invokeVirtual: 'java/io/PrintStream' method: 'println' returning: '(Ljava/lang/String;)V'.
    aload: 0.

