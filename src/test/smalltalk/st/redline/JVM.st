
"This is an example of direct bytecode manipulation from Smalltalk."

JVM getStatic: 'java/lang/System' named: 'out' as: 'Ljava/io/PrintStream;';
    ldc: 'hello Smalltalk JVM bytecode manipulation!';
    invokeVirtual: 'java/io/PrintStream' method: 'println' returning: '(Ljava/lang/String;)V'.
