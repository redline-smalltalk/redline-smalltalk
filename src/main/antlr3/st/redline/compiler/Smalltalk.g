/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
grammar Smalltalk;

options {
	language = Java;
}

@header {
	package st.redline.compiler;
}

@lexer::header {
	package st.redline.compiler;
}

program returns [Program n] 
	:	temporaries? statements methods {$n = new Program($temporaries.n, $statements.n, $methods.n);}
	;

methods returns [Methods n]
	:	{$n = new Methods();}
		( method {$n.add($method.n);} )*
	;
	
method returns [Method n]
	:	o = NAME c= 'class'? '>>'  messagePattern '[' primitive? temporaries? statements  ']' {$n = MethodFactory.create($o.text, $c.text, $messagePattern.n, $primitive.n, $temporaries.n, $statements.n);}
	;

messagePattern returns [MessagePattern n]
	:	unarySelector {$n = new UnarySelectorMessagePattern($unarySelector.n);}
	|	binarySelector variableName {$n = new BinarySelectorMessagePattern($binarySelector.n, $variableName.n);}
	|	( keyword variableName {if ($n != null) ((KeywordMessagePattern)$n).add(new KeywordAndVariableName($keyword.n, $variableName.n)); else $n = new KeywordMessagePattern(new KeywordAndVariableName($keyword.n, $variableName.n));} )+ 
	;

primitive returns [Primitive n]
	:	'<primitive:' DIGITS '>' {$n = new Primitive($DIGITS.text, $DIGITS.line);}
	;

temporaries	 returns [Temporaries n]
	:	{$n = new Temporaries();}
		'|' ( variableName {$n.add(new Temporary($variableName.n));} )* '|'
	;

statements returns [Statements n]
	:	( nonEmptyStatements {$n = $nonEmptyStatements.n;} )?
	;

nonEmptyStatements returns [Statements n]
	:	'^' expression {$n = new Statements(new AnswerExpression($expression.n));} '.'?
	|	expression ( '.' statements )? {$n = new Statements($expression.n, $statements.n);}
	;

expression returns [Expression n]
	:	variableName (':=' | '_') e = expression {$n = new AssignmentExpression($variableName.n, $e.n);}
	|	simpleExpression {$n = $simpleExpression.n;}
	|	jvmExpression  {$n = $jvmExpression.n;}
	;

jvmExpression returns [JvmExpression n]
	:	visitInsn {$n = $visitInsn.n;}
	;

visitInsn returns [JvmExpression n]
	:	'<insn:' jvmOpcode '>' {$n = new JvmInsn($jvmOpcode.n);}
	|	'<ldcInsn:' stringConstant '>' {$n = new JvmLdcInsn($stringConstant.n);}
	|	'<varInsn:' jvmOpcode ',' DIGITS '>' {$n = new JvmVarInsn($jvmOpcode.n, $DIGITS.text);}
	|	'<methodInsn:' jvmOpcode ','  o=stringConstant ',' a=stringConstant ',' d=stringConstant '>' {$n = new JvmMethodInsn($jvmOpcode.n, $o.n, $a.n, $d.n);}
	|	'<typeInsn:' jvmOpcode ',' stringConstant '>' {$n = new JvmTypeInsn($jvmOpcode.n, $stringConstant.n);}
	|	'<iincInsn:' v=DIGITS ',' i=DIGITS '>' {$n = new JvmIincInsn($v.text, $i.text, $v.line);}
	|	'<intInsn:' jvmOpcode ',' DIGITS '>' {$n = new JvmIntInsn($jvmOpcode.n, $DIGITS.text);}
	|	'<fieldInsn:' jvmOpcode ',' x=stringConstant ',' y=stringConstant ',' z=stringConstant '>'  {$n = new JvmFieldInsn($jvmOpcode.n, $x.n, $y.n, $z.n);}
	|	l = '<loadJavaValue>' {$n = new JvmLoadJavaValue($l.line);}
	|	l = '<storeJavaValue>' {$n = new JvmStoreJavaValue($l.line);}
	;

jvmOpcode returns [JvmOpcode n]
	:	    c = 'ACC_PUBLIC' {$n = new JvmOpcode(1, $c.line);}
	|    c = 'ACC_PRIVATE' {$n = new JvmOpcode(2, $c.line);}
	|    c = 'ACC_PROTECTED' {$n = new JvmOpcode(4, $c.line);}
	|    c = 'ACC_STATIC' {$n = new JvmOpcode(8, $c.line);}
	|    c = 'ACC_FINAL' {$n = new JvmOpcode(16, $c.line);}
	|    c = 'ACC_SUPER' {$n = new JvmOpcode(32, $c.line);}
	|    c = 'ACC_SYNCHRONIZED' {$n = new JvmOpcode(32, $c.line);}
	|    c = 'ACC_VOLATILE' {$n = new JvmOpcode(64, $c.line);}
	|    c = 'ACC_BRIDGE' {$n = new JvmOpcode(64, $c.line);}
	|    c = 'ACC_VARARGS' {$n = new JvmOpcode(128, $c.line);}
	|    c = 'ACC_TRANSIENT' {$n = new JvmOpcode(128, $c.line);}
	|    c = 'ACC_NATIVE' {$n = new JvmOpcode(256, $c.line);}
	|    c = 'ACC_INTERFACE' {$n = new JvmOpcode(512, $c.line);}
	|    c = 'ACC_ABSTRACT' {$n = new JvmOpcode(1024, $c.line);}
	|    c = 'ACC_STRICT' {$n = new JvmOpcode(2048, $c.line);}
	|    c = 'ACC_SYNTHETIC' {$n = new JvmOpcode(4096, $c.line);}
	|    c = 'ACC_ANNOTATION' {$n = new JvmOpcode(8192, $c.line);}
	|    c = 'ACC_ENUM' {$n = new JvmOpcode(16384, $c.line);}
	|    c = 'ACC_DEPRECATED' {$n = new JvmOpcode(131072, $c.line);}
	|    c = 'T_BOOLEAN' {$n = new JvmOpcode(4, $c.line);}
	|    c = 'T_CHAR' {$n = new JvmOpcode(5, $c.line);}
	|    c = 'T_FLOAT' {$n = new JvmOpcode(6, $c.line);}
	|    c = 'T_DOUBLE' {$n = new JvmOpcode(7, $c.line);}
	|    c = 'T_BYTE' {$n = new JvmOpcode(8, $c.line);}
	|    c = 'T_SHORT' {$n = new JvmOpcode(9, $c.line);}
	|    c = 'T_INT' {$n = new JvmOpcode(10, $c.line);}
	|    c = 'T_LONG' {$n = new JvmOpcode(11, $c.line);}
	|    c = 'F_NEW' {$n = new JvmOpcode(-1, $c.line);}
	|    c = 'F_FULL' {$n = new JvmOpcode(0, $c.line);}
	|    c = 'F_APPEND' {$n = new JvmOpcode(1, $c.line);}
	|    c = 'F_CHOP' {$n = new JvmOpcode(2, $c.line);}
	|    c = 'F_SAME' {$n = new JvmOpcode(3, $c.line);}
	|    c = 'F_SAME1' {$n = new JvmOpcode(4, $c.line);}
	|    c = 'NOP' {$n = new JvmOpcode(0, $c.line);}
	|    c = 'ACONST_NULL' {$n = new JvmOpcode(1, $c.line);}
	|    c = 'ICONST_M1' {$n = new JvmOpcode(2, $c.line);}
	|    c = 'ICONST_0' {$n = new JvmOpcode(3, $c.line);}
	|    c = 'ICONST_1' {$n = new JvmOpcode(4, $c.line);}
	|    c = 'ICONST_2' {$n = new JvmOpcode(5, $c.line);}
	|    c = 'ICONST_3' {$n = new JvmOpcode(6, $c.line);}
	|    c = 'ICONST_4' {$n = new JvmOpcode(7, $c.line);}
	|    c = 'ICONST_5' {$n = new JvmOpcode(8, $c.line);}
	|    c = 'LCONST_0' {$n = new JvmOpcode(9, $c.line);}
	|    c = 'LCONST_1' {$n = new JvmOpcode(10, $c.line);}
	|    c = 'FCONST_0' {$n = new JvmOpcode(11, $c.line);}
	|    c = 'FCONST_1' {$n = new JvmOpcode(12, $c.line);}
	|    c = 'FCONST_2' {$n = new JvmOpcode(13, $c.line);}
	|    c = 'DCONST_0' {$n = new JvmOpcode(14, $c.line);}
	|    c = 'DCONST_1' {$n = new JvmOpcode(15, $c.line);}
	|    c = 'BIPUSH' {$n = new JvmOpcode(16, $c.line);}
	|    c = 'SIPUSH' {$n = new JvmOpcode(17, $c.line);}
	|    c = 'LDC' {$n = new JvmOpcode(18, $c.line);}
	|    c = 'ILOAD' {$n = new JvmOpcode(21, $c.line);}
	|    c = 'LLOAD' {$n = new JvmOpcode(22, $c.line);}
	|    c = 'FLOAD' {$n = new JvmOpcode(23, $c.line);}
	|    c = 'DLOAD' {$n = new JvmOpcode(24, $c.line);}
	|    c = 'ALOAD' {$n = new JvmOpcode(25, $c.line);}
	|    c = 'IALOAD' {$n = new JvmOpcode(46, $c.line);}
	|    c = 'LALOAD' {$n = new JvmOpcode(47, $c.line);}
	|    c = 'FALOAD' {$n = new JvmOpcode(48, $c.line);}
	|    c = 'DALOAD' {$n = new JvmOpcode(49, $c.line);}
	|    c = 'AALOAD' {$n = new JvmOpcode(50, $c.line);}
	|    c = 'BALOAD' {$n = new JvmOpcode(51, $c.line);}
	|    c = 'CALOAD' {$n = new JvmOpcode(52, $c.line);}
	|    c = 'SALOAD' {$n = new JvmOpcode(53, $c.line);}
	|    c = 'ISTORE' {$n = new JvmOpcode(54, $c.line);}
	|    c = 'LSTORE' {$n = new JvmOpcode(55, $c.line);}
	|    c = 'FSTORE' {$n = new JvmOpcode(56, $c.line);}
	|    c = 'DSTORE' {$n = new JvmOpcode(57, $c.line);}
	|    c = 'ASTORE' {$n = new JvmOpcode(58, $c.line);}
	|    c = 'IASTORE' {$n = new JvmOpcode(79, $c.line);}
	|    c = 'LASTORE' {$n = new JvmOpcode(80, $c.line);}
	|    c = 'FASTORE' {$n = new JvmOpcode(81, $c.line);}
	|    c = 'DASTORE' {$n = new JvmOpcode(82, $c.line);}
	|    c = 'AASTORE' {$n = new JvmOpcode(83, $c.line);}
	|    c = 'BASTORE' {$n = new JvmOpcode(84, $c.line);}
	|    c = 'CASTORE' {$n = new JvmOpcode(85, $c.line);}
	|    c = 'SASTORE' {$n = new JvmOpcode(86, $c.line);}
	|    c = 'POP' {$n = new JvmOpcode(87, $c.line);}
	|    c = 'POP2' {$n = new JvmOpcode(88, $c.line);}
	|    c = 'DUP' {$n = new JvmOpcode(89, $c.line);}
	|    c = 'DUP_X1' {$n = new JvmOpcode(90, $c.line);}
	|    c = 'DUP_X2' {$n = new JvmOpcode(91, $c.line);}
	|    c = 'DUP2' {$n = new JvmOpcode(92, $c.line);}
	|    c = 'DUP2_X1' {$n = new JvmOpcode(93, $c.line);}
	|    c = 'DUP2_X2' {$n = new JvmOpcode(94, $c.line);}
	|    c = 'SWAP' {$n = new JvmOpcode(95, $c.line);}
	|    c = 'IADD' {$n = new JvmOpcode(96, $c.line);}
	|    c = 'LADD' {$n = new JvmOpcode(97, $c.line);}
	|    c = 'FADD' {$n = new JvmOpcode(98, $c.line);}
	|    c = 'DADD' {$n = new JvmOpcode(99, $c.line);}
	|    c = 'ISUB' {$n = new JvmOpcode(100, $c.line);}
	|    c = 'LSUB' {$n = new JvmOpcode(101, $c.line);}
	|    c = 'FSUB' {$n = new JvmOpcode(102, $c.line);}
	|    c = 'DSUB' {$n = new JvmOpcode(103, $c.line);}
	|    c = 'IMUL' {$n = new JvmOpcode(104, $c.line);}
	|    c = 'LMUL' {$n = new JvmOpcode(105, $c.line);}
	|    c = 'FMUL' {$n = new JvmOpcode(106, $c.line);}
	|    c = 'DMUL' {$n = new JvmOpcode(107, $c.line);}
	|    c = 'IDIV' {$n = new JvmOpcode(108, $c.line);}
	|    c = 'LDIV' {$n = new JvmOpcode(109, $c.line);}
	|    c = 'FDIV' {$n = new JvmOpcode(110, $c.line);}
	|    c = 'DDIV' {$n = new JvmOpcode(111, $c.line);}
	|    c = 'IREM' {$n = new JvmOpcode(112, $c.line);}
	|    c = 'LREM' {$n = new JvmOpcode(113, $c.line);}
	|    c = 'FREM' {$n = new JvmOpcode(114, $c.line);}
	|    c = 'DREM' {$n = new JvmOpcode(115, $c.line);}
	|    c = 'INEG' {$n = new JvmOpcode(116, $c.line);}
	|    c = 'LNEG' {$n = new JvmOpcode(117, $c.line);}
	|    c = 'FNEG' {$n = new JvmOpcode(118, $c.line);}
	|    c = 'DNEG' {$n = new JvmOpcode(119, $c.line);}
	|    c = 'ISHL' {$n = new JvmOpcode(120, $c.line);}
	|    c = 'LSHL' {$n = new JvmOpcode(121, $c.line);}
	|    c = 'ISHR' {$n = new JvmOpcode(122, $c.line);}
	|    c = 'LSHR' {$n = new JvmOpcode(123, $c.line);}
	|    c = 'IUSHR' {$n = new JvmOpcode(124, $c.line);}
	|    c = 'LUSHR' {$n = new JvmOpcode(125, $c.line);}
	|    c = 'IAND' {$n = new JvmOpcode(126, $c.line);}
	|    c = 'LAND' {$n = new JvmOpcode(127, $c.line);}
	|    c = 'IOR' {$n = new JvmOpcode(128, $c.line);}
	|    c = 'LOR' {$n = new JvmOpcode(129, $c.line);}
	|    c = 'IXOR' {$n = new JvmOpcode(130, $c.line);}
	|    c = 'LXOR' {$n = new JvmOpcode(131, $c.line);}
	|    c = 'IINC' {$n = new JvmOpcode(132, $c.line);}
	|    c = 'I2L' {$n = new JvmOpcode(133, $c.line);}
	|    c = 'I2F' {$n = new JvmOpcode(134, $c.line);}
	|    c = 'I2D' {$n = new JvmOpcode(135, $c.line);}
	|    c = 'L2I' {$n = new JvmOpcode(136, $c.line);}
	|    c = 'L2F' {$n = new JvmOpcode(137, $c.line);}
	|    c = 'L2D' {$n = new JvmOpcode(138, $c.line);}
	|    c = 'F2I' {$n = new JvmOpcode(139, $c.line);}
	|    c = 'F2L' {$n = new JvmOpcode(140, $c.line);}
	|    c = 'F2D' {$n = new JvmOpcode(141, $c.line);}
	|    c = 'D2I' {$n = new JvmOpcode(142, $c.line);}
	|    c = 'D2L' {$n = new JvmOpcode(143, $c.line);}
	|    c = 'D2F' {$n = new JvmOpcode(144, $c.line);}
	|    c = 'I2B' {$n = new JvmOpcode(145, $c.line);}
	|    c = 'I2C' {$n = new JvmOpcode(146, $c.line);}
	|    c = 'I2S' {$n = new JvmOpcode(147, $c.line);}
	|    c = 'LCMP' {$n = new JvmOpcode(148, $c.line);}
	|    c = 'FCMPL' {$n = new JvmOpcode(149, $c.line);}
	|    c = 'FCMPG' {$n = new JvmOpcode(150, $c.line);}
	|    c = 'DCMPL' {$n = new JvmOpcode(151, $c.line);}
	|    c = 'DCMPG' {$n = new JvmOpcode(152, $c.line);}
	|    c = 'IFEQ' {$n = new JvmOpcode(153, $c.line);}
	|    c = 'IFNE' {$n = new JvmOpcode(154, $c.line);}
	|    c = 'IFLT' {$n = new JvmOpcode(155, $c.line);}
	|    c = 'IFGE' {$n = new JvmOpcode(156, $c.line);}
	|    c = 'IFGT' {$n = new JvmOpcode(157, $c.line);}
	|    c = 'IFLE' {$n = new JvmOpcode(158, $c.line);}
	|    c = 'IF_ICMPEQ' {$n = new JvmOpcode(159, $c.line);}
	|    c = 'IF_ICMPNE' {$n = new JvmOpcode(160, $c.line);}
	|    c = 'IF_ICMPLT' {$n = new JvmOpcode(161, $c.line);}
	|    c = 'IF_ICMPGE' {$n = new JvmOpcode(162, $c.line);}
	|    c = 'IF_ICMPGT' {$n = new JvmOpcode(163, $c.line);}
	|    c = 'IF_ICMPLE' {$n = new JvmOpcode(164, $c.line);}
	|    c = 'IF_ACMPEQ' {$n = new JvmOpcode(165, $c.line);}
	|    c = 'IF_ACMPNE' {$n = new JvmOpcode(166, $c.line);}
	|    c = 'GOTO' {$n = new JvmOpcode(167, $c.line);}
	|    c = 'JSR' {$n = new JvmOpcode(168, $c.line);}
	|    c = 'RET' {$n = new JvmOpcode(169, $c.line);}
	|    c = 'TABLESWITCH' {$n = new JvmOpcode(170, $c.line);}
	|    c = 'LOOKUPSWITCH' {$n = new JvmOpcode(171, $c.line);}
	|    c = 'IRETURN' {$n = new JvmOpcode(172, $c.line);}
	|    c = 'LRETURN' {$n = new JvmOpcode(173, $c.line);}
	|    c = 'FRETURN' {$n = new JvmOpcode(174, $c.line);}
	|    c = 'DRETURN' {$n = new JvmOpcode(175, $c.line);}
	|    c = 'ARETURN' {$n = new JvmOpcode(176, $c.line);}
	|    c = 'RETURN' {$n = new JvmOpcode(177, $c.line);}
	|    c = 'GETSTATIC' {$n = new JvmOpcode(178, $c.line);}
	|    c = 'PUTSTATIC' {$n = new JvmOpcode(179, $c.line);}
	|    c = 'GETFIELD' {$n = new JvmOpcode(180, $c.line);}
	|    c = 'PUTFIELD' {$n = new JvmOpcode(181, $c.line);}
	|    c = 'INVOKEVIRTUAL' {$n = new JvmOpcode(182, $c.line);}
	|    c = 'INVOKESPECIAL' {$n = new JvmOpcode(183, $c.line);}
	|    c = 'INVOKESTATIC' {$n = new JvmOpcode(184, $c.line);}
	|    c = 'INVOKEINTERFACE' {$n = new JvmOpcode(185, $c.line);}
	|    c = 'INVOKEDYNAMIC' {$n = new JvmOpcode(186, $c.line);}
	|    c = 'NEW' {$n = new JvmOpcode(187, $c.line);}
	|    c = 'NEWARRAY' {$n = new JvmOpcode(188, $c.line);}
	|    c = 'ANEWARRAY' {$n = new JvmOpcode(189, $c.line);}
	|    c = 'ARRAYLENGTH' {$n = new JvmOpcode(190, $c.line);}
	|    c = 'ATHROW' {$n = new JvmOpcode(191, $c.line);}
	|    c = 'CHECKCAST' {$n = new JvmOpcode(192, $c.line);}
	|    c = 'INSTANCEOF' {$n = new JvmOpcode(193, $c.line);}
	|    c = 'MONITORENTER' {$n = new JvmOpcode(194, $c.line);}
	|    c = 'MONITOREXIT' {$n = new JvmOpcode(195, $c.line);}
	|    c = 'MULTIANEWARRAY' {$n = new JvmOpcode(197, $c.line);}
	|    c = 'IFNULL' {$n = new JvmOpcode(198, $c.line);}
	|    c = 'IFNONNULL' {$n = new JvmOpcode(199, $c.line);}
	;

simpleExpression returns [SimpleExpression n]
	:	{$n = new SimpleExpression();}
		primary {$n.add($primary.n);} ( messageExpression {$n.add($messageExpression.n);} ( ';' messageElement {$n.add($messageElement.n);} )* )?
	;

messageElement returns [MessageElement n]
	:	unarySelector {$n = new UnarySelectorMessageElement($unarySelector.n);}
	|	binarySelector unaryObjectDescription {$n = new BinarySelectorMessageElement($binarySelector.n, $unaryObjectDescription.n);}
	|	( keyword binaryObjectDescription {if ($n != null) ((KeywordMessageElement)$n).add($keyword.n, $binaryObjectDescription.n); else $n = new KeywordMessageElement($keyword.n, $binaryObjectDescription.n);} )+
	;

messageExpression returns [MessageExpression n]
	:	unaryExpression {$n = $unaryExpression.n;}
	|	binaryExpression {$n = $binaryExpression.n;}
	|	keywordExpression {$n = $keywordExpression.n;}
	;

unaryExpression returns [UnaryExpression n]
	:	{$n = new UnaryExpression();}
		( unarySelector {$n.add($unarySelector.n);} )+ (binaryExpression {$n.add($binaryExpression.n);} | keywordExpression {$n.add($keywordExpression.n);})?
	;

binaryExpression returns [BinaryExpression n]
	:	{$n = new BinaryExpression();}
		( binarySelector unaryObjectDescription {$n.add($binarySelector.n, $unaryObjectDescription.n);} )+ (keywordExpression {$n.add($keywordExpression.n);})?
	;

keywordExpression returns [KeywordExpression n]
	:	{$n = new KeywordExpression();}
		( keyword binaryObjectDescription {$n.add($keyword.n, $binaryObjectDescription.n);} )+
	;

unaryObjectDescription returns [UnaryObjectDescription n]
	:	primary {$n = new UnaryObjectDescription($primary.n);} ( unarySelector {$n.add($unarySelector.n);} )*
	;

binaryObjectDescription returns [BinaryObjectDescription n]
	:	primary {$n = new BinaryObjectDescription($primary.n);} ( unarySelector {$n.add($unarySelector.n);} )* ( binarySelector unaryObjectDescription {$n.add($binarySelector.n, $unaryObjectDescription.n);} )*
	;

primary returns [Primary n]
	:	literal {$n = $literal.n;}
	|	variableName {$n = $variableName.n;}
	|	block {$n = $block.n;}
	|	'(' expression {$n = new PrimaryExpression($expression.n);} ')'
	|	'{' statements {$n = new PrimaryStatements($statements.n);} '}'
	;

literal returns [Literal n]
	:	numberConstant {$n = new LiteralNumber($numberConstant.n);}
	|	characterConstant {$n = new LiteralCharacter($characterConstant.n);}
	|	stringConstant {$n = new LiteralString($stringConstant.n);}
	|	stringChunk {$n = new LiteralString($stringChunk.n);}
	|	'#' ( symbol {$n = new LiteralSymbol($symbol.n);} | array {$n = new LiteralArray($array.n);} )
	|	('self' | 'super' | 'true' | 'false' | 'nil' | 'Smalltalk' ) => reservedWordLiteral {$n = $reservedWordLiteral.n;}
	;

reservedWordLiteral returns [Literal n]
	:	l = 'self' {$n = new SelfReservedWord($l.line);}
	|	l = 'super' {$n = new SuperReservedWord($l.line);}
	|	l = 'true' {$n = new TrueReservedWord($l.line);}
	|	l = 'false' {$n = new FalseReservedWord($l.line);}
	| 	l = 'nil' {$n = new NilReservedWord($l.line);}
	| 	l = 'Smalltalk' {$n = new SmalltalkReservedWord($l.line);}	
	;
	
block returns [Block n]
	:	{$n = new Block();}
		'[' ((':' variableName {$n.add($variableName.n);})+ '|')? ( temporaries {$n.add($temporaries.n);} )? statements {$n.add($statements.n);} ']'
	;

array returns [Array n]
	:	{$n = new Array();}
		'(' ( arrayConstantElement {$n.add($arrayConstantElement.n);})* ')'
	;

arrayConstantElement returns [VisitableNode n]
 	:	numberConstant {$n = $numberConstant.n;}
	|	characterConstant {$n = $characterConstant.n;}
	|	stringConstant {$n = $stringConstant.n;}
	|	symbol {$n = $symbol.n;}
	|	array {$n = $array.n;}
	;

symbol returns [Symbol n]
	:	identifier {$n = new Symbol($identifier.n);}
	|	binarySelector {$n = new Symbol($binarySelector.n);}
	|	keyword {$n = new Symbol($keyword.n);}
	;

unarySelector returns [UnarySelector n] 
	:	NAME {$n = new UnarySelector($NAME.text, $NAME.line);}
	;

binarySelector returns [BinarySelector n]
	:	BINARY_SYMBOL {$n = new BinarySelector($BINARY_SYMBOL.text, $BINARY_SYMBOL.line);}
	;

variableName returns [VariableName n]
	:	NAME {$n = new VariableName($NAME.text, $NAME.line);}
	;

keyword returns [Keyword n]
	:	KEYWORD {$n = new Keyword($KEYWORD.text, $KEYWORD.line);}
	;

identifier returns [Identifier n]
	:	NAME {$n = new Identifier($NAME.text, $NAME.line);}
	;

characterConstant returns [CharacterConstant n]
	:	CHARACTER {$n = new CharacterConstant($CHARACTER.text, $CHARACTER.line);}
	;

stringConstant returns [StringConstant n]
	:	STRING_LITERAL {$n = new StringConstant($STRING_LITERAL.text, $STRING_LITERAL.line);}
	;

stringChunk returns [StringChunk n]
	:	STRING_CHUNK {$n = new StringChunk($STRING_CHUNK.text, $STRING_CHUNK.line);}
	;

numberConstant returns [NumberConstant n]
	:	DIGITS {$n = new NumberConstant($DIGITS.text, $DIGITS.line);}
	;

DIGITS: ('0'..'9')+;
NAME: ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')*;
KEYWORD: NAME ':';
STRING_LITERAL: '\'' .* '\'';
STRING_CHUNK: '`' .* '`';
BINARY_SYMBOL: ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',') ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',')*;
CHARACTER: '$' . ;
COMMENT: '"' .* '"' {$channel = HIDDEN;};
WHITESPACE: (' ' | '\t' | '\n' | '\r' | '\f' )+ {$channel = HIDDEN;};

