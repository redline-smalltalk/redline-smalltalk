
Object < #Transcript

+ show: anObject
  self stdout: anObject

+ stdout: anObject
  "load System.out"
  <fieldInsn: GETSTATIC, 'java/lang/System', 'out', 'Ljava/io/PrintStream;'>.
  "Load anObject onto stack. 1 = receiver, 2 = class method found in, 3 = first argument."
  <varInsn: ALOAD, 3>.
  "Invoke print on System.out"
  <methodInsn: INVOKEVIRTUAL, 'java/io/PrintStream', 'print', '(Ljava/lang/Object;)V'>.
  "Load receiver as Smalltalk methods always return a value."
  <varInsn: ALOAD, 1>.

