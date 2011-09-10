
Object < #Transcript

+ show: anObject
  self stdout: anObject

+ cr
  <fieldInsn: GETSTATIC, 'java/lang/System', 'out', 'Ljava/io/PrintStream;'>.
  <methodInsn: INVOKEVIRTUAL, 'java/io/PrintStream', 'println', '()V'>.
  <varInsn: ALOAD, 1>.

+ stdout: anObject
  "load System.out"
  <fieldInsn: GETSTATIC, 'java/lang/System', 'out', 'Ljava/io/PrintStream;'>.
  "Load anObject onto stack. 0 = this method, 1 = receiver, 2 = class method was found in, 3 = first argument."
  <varInsn: ALOAD, 3>.
  "Invoke print on System.out"
  <methodInsn: INVOKEVIRTUAL, 'java/io/PrintStream', 'print', '(Ljava/lang/Object;)V'>.
  "Load receiver as Smalltalk methods always return a value."
  <varInsn: ALOAD, 1>.

