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
*/
grammar Smalltalk;

options {
	language = Java;
	backtrack = true;
}

@header {
	package st.redline.smalltalk.interpreter;
}

@lexer::header {
	package st.redline.smalltalk.interpreter;
}

program returns [Program n]
	: 	{ $n = new Program(); }
		(chunk {$n.add($chunk.n);})* EOF
	; 

chunk returns [Chunk n]
	:	(temporaries)? sequence EOF {$n = new SequenceChunk($sequence.n, $temporaries.n);}
	|	(temporaries)? sequence '!' {$n = new SequenceChunk($sequence.n, $temporaries.n);}
	|	'!' sequence '!' {$n = new DirectiveChunk($sequence.n);}
	|	method CHUNK_END {$n = new MethodChunk($method.n); }
	; 

method returns [Method n]
	:	methodPattern (temporaries)? (pragmas)? sequence {$n = new Method($methodPattern.n, $temporaries.n, $pragmas.n, $sequence.n);}
	;
	
methodPattern returns [MethodPattern n]
	:	NAME {$n = new UnaryMethodPattern($NAME.text, $NAME.line);}
	|	BINARY_SYMBOL variable {$n = new BinaryMethodPattern($BINARY_SYMBOL.text, $BINARY_SYMBOL.line, $variable.n);}	
	|	keywordMethodPattern {$n = $keywordMethodPattern.n;}
	;

keywordMethodPattern returns [KeywordMethodPattern n]
	:	{$n = new KeywordMethodPattern();}
		(KEYWORD variable {$n.add($KEYWORD.text, $KEYWORD.line, $variable.n);})*
	;

temporaries returns [Temporaries n]
	:	'||' {$n = new Temporaries();}
	|	{$n = new Temporaries();}
		'|' ( NAME {$n.add(new Temporary($NAME.text, $NAME.line));} )+ '|'
	;

pragmas returns [Pragmas n]
	:	'<' pragmaMessage {$n = $pragmaMessage.n;} '>' 
	;
			
sequence returns [Sequence n]
	:	statements {$n = new Sequence($statements.n);}
	;

statements returns [Statements n]
	:	statementList '.'? {$n = new Statements($statementList.n);}
	|	statementList '.'  {$n = new Statements($statementList.n);} '^' expression {$n.answer($expression.n);} '.'?
	|	'^' expression {$n = new Statements($expression.n);} '.'?
	;
	
statementList returns [StatementList n]
	:	{ $n = new StatementList(); }	
		e1 = expression {$n.add($e1.n);} ( '.' e2 = expression {$n.add($e2.n);} )*
	;
	
expression returns [Expression n]
	:	assignment {$n = new Expression($assignment.n);}
	|	cascade {$n = new Expression($cascade.n);}
	|	primary {$n = new Expression($primary.n);}
	;

assignment returns [Assignment n]
	:	variable ASSIGNMENT expression {$n = new Assignment($variable.n, $expression.n);}
	;
	
cascade returns [Cascade n]
	:	messageSend {$n = new Cascade($messageSend.n);} (';' message {$n.add($message.n);})*
	;

message returns [Message n]
	:	unaryMessage {$n = new Message($unaryMessage.n);}
	|	binaryMessage {$n = new Message($binaryMessage.n);}
	|	keywordMessage {$n = new Message($keywordMessage.n);}
	;
	
messageSend returns [MessageSend n]
	:	keywordMessageSend {$n = new MessageSend($keywordMessageSend.n);}
	|	binaryMessageSend {$n = new MessageSend($binaryMessageSend.n);}	
	|	unaryMessageSend {$n = new MessageSend($unaryMessageSend.n);}
	;

keywordMessageSend returns [KeywordMessageSend n]
	:	primary keywordMessage {$n = new KeywordMessageSend($primary.n, $keywordMessage.n);}
	;
	
keywordMessage returns [KeywordMessage n]
	:	k1 = KEYWORD a1 = keywordArgument {$n = new KeywordMessage($k1.text, $k1.line, $a1.n);} ( k2 = KEYWORD a2 = keywordArgument {$n.add($k2.text, $k2.line, $a2.n);} )*
	;

keywordArgument returns [KeywordArgument n]
	:	primary {$n = new KeywordArgument($primary.n);}
	;

binaryMessageSend returns [BinaryMessageSend n]
	:	unaryMessageSend {$n = new BinaryMessageSend($unaryMessageSend.n);} ( binaryMessage {$n.add($binaryMessage.n);} )+
	|	primary {$n = new BinaryMessageSend($primary.n);} ( binaryMessage {$n.add($binaryMessage.n);} )+
	;

binaryMessage returns [BinaryMessage n]
	:	BINARY_SYMBOL binaryArgument {$n = new BinaryMessage($BINARY_SYMBOL.text, $BINARY_SYMBOL.line, $binaryArgument.n);}
	;

binaryArgument returns [BinaryArgument n]
	:	unaryMessageSend {$n = new BinaryArgument($unaryMessageSend.n);}
	|	primary {$n = new BinaryArgument($primary.n);}
	;
		
unaryMessageSend returns [UnaryMessageSend n]
	:	primary {$n = new UnaryMessageSend($primary.n);} ( unaryMessage {$n.add($unaryMessage.n);} )+
	;

unaryMessage returns [UnaryMessage n]
	:	NAME {$n = new UnaryMessage($NAME.text, $NAME.line);}
	;

primary returns [Primary n]
	:	l = '(' expression {$n = new PrimaryExpression($expression.n, $l.line);} ')'
	|	variable {$n = $variable.n;}
	|	block {$n = $block.n;}
	|	literal {$n = $literal.n;}
	|	array {$n = $array.n;}
	;

block returns [Block n]
	:	l = '[' {$n = new Block($l.line, null);} ']'
	|	l = '[' sequence {$n = new Block($l.line, $sequence.n);} ']'
	;
	
array returns [Array n]
	:	l = '{' statementList {$n = new Array($statementList.n, $l.line);} '}'
	;
		
variable returns [Variable n]
	:	NAME {$n = new Variable($NAME.text, $NAME.line);}
	;

literal returns [Literal n]
	:	l = 'self' {$n = new Self($l.text, $l.line);}
	|	l = 'true' {$n = new True($l.text, $l.line);}
	|	l = 'false' {$n = new False($l.text, $l.line);}
	|	l = 'nil' {$n = new Nil($l.text, $l.line);}
	|	l = DECIMAL_NUMBER {$n = new LiteralNumber($l.text, $l.line);}	
	|	l = RADIX_NUMBER {$n = new LiteralNumber($l.text, $l.line);}
	|	l = SCALED_NUMBER {$n = new LiteralNumber($l.text, $l.line);}
	|	l = EXPONENT_NUMBER {$n = new LiteralNumber($l.text, $l.line);}
	|	l = NEGATIVE_NUMBER {$n = new LiteralNumber($l.text, $l.line);}
	|	CHARACTER {$n = new StCharacter($CHARACTER.text, $CHARACTER.line);}
	|	STRING {$n = new StString($STRING.text, $STRING.line);}
 	|	'#' NAME {$n = new Symbol($NAME.text, $NAME.line);}
 	|	'#' STRING {$n = new Symbol($STRING.text, $STRING.line);}
 	|	'#' BINARY_SYMBOL {$n = new Symbol($BINARY_SYMBOL.text, $BINARY_SYMBOL.line);}
 	|	'#' KEYWORD {$n = new Symbol($KEYWORD.text, $KEYWORD.line);}
 	|	'#' MULTI_KEYWORD {$n = new Symbol($MULTI_KEYWORD.text, $MULTI_KEYWORD.line);} 
 	|	'#' ALTERNATE_KEYWORD {$n = new Symbol($ALTERNATE_KEYWORD.text, $ALTERNATE_KEYWORD.line);}
 	|	l = '#:' {$n = new Symbol("':'", $l.line);}
 	|	l = '#' '(' literalArray {$literalArray.n.line($l.line); $n = $literalArray.n;} ')' 
	;

literalArray returns [LiteralArray n]
	:	{$n = new LiteralArray(); }
		( nestedArrayLiteral {$n.add($nestedArrayLiteral.n);} )*
	;

nestedArrayLiteral returns [Literal n, LiteralArray outside, LiteralArray inside]
	:	arrayLiteral {$n = $arrayLiteral.n; }
	|	{$n = new LiteralArray(); }
		'(' ( arrayLiteral {((LiteralArray)$n).add($arrayLiteral.n);} )* ')'
	|	{$n = new LiteralArray();}
		'(' ( a1 = arrayLiteral {$outside.add($a1.n);} )* '(' ( a2 = arrayLiteral {$inside.add($a2.n);} )* ')' {$outside.add($inside);} ( a3 = arrayLiteral {$outside.add($a3.n);} )* ')'
		{((LiteralArray)$n).add($outside);}
	;

arrayLiteral returns [ArrayLiteral n]
	:	NAME {$n = new ArrayLiteral($NAME.text, $NAME.line);}
 	|	BINARY_SYMBOL {$n = new ArrayLiteral($BINARY_SYMBOL.text, $BINARY_SYMBOL.line);}
 	|	KEYWORD {$n = new ArrayLiteral($KEYWORD.text, $KEYWORD.line);}
 	|	MULTI_KEYWORD {$n = new ArrayLiteral($MULTI_KEYWORD.text, $MULTI_KEYWORD.line);} 
 	|	ALTERNATE_KEYWORD {$n = new ArrayLiteral($ALTERNATE_KEYWORD.text, $ALTERNATE_KEYWORD.line);}
	;

pragmaMessage returns [PragmaMessage n]
	:	primitive {$n = new PragmaMessage($primitive.n);}
	;
	
primitive returns [Primitive n]
	:	l = 'primitive:' STRING {$n = new PrimitiveString($STRING.text, $l.line);}
	|	l = 'primitive:' DECIMAL_NUMBER {$n = new PrimitiveNumber($DECIMAL_NUMBER.text, $l.line);}
	|	l = 'primitive:' s1 = STRING 'module:' s2 = STRING {$n = new PrimitiveModule($s1.text, $s2.text, $l.line);}
	;

DECIMAL_NUMBER:	('0'..'9')+ ('.' ('0'..'9')+)?;
RADIX_NUMBER: ('0'..'9')+ 'r' ('0'..'9' | 'A'..'Z')+ ('.' ('0'..'9' | 'A'..'Z')+)?;
SCALED_NUMBER: DECIMAL_NUMBER 's' ('0'..'9')+;
EXPONENT_NUMBER: (DECIMAL_NUMBER | RADIX_NUMBER) 'e' '-'? ('0'..'9')+;
NEGATIVE_NUMBER: '-' DECIMAL_NUMBER | '-' RADIX_NUMBER | '-' SCALED_NUMBER | '-' EXPONENT_NUMBER;
NAME: ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')*;
KEYWORD: NAME ':';
MULTI_KEYWORD: NAME ':' (NAME ':')+;
ALTERNATE_KEYWORD: ':' NAME ':' (NAME ':')*;
WHITESPACE: (' ' | '\t' | '\n' | '\r' | '\f' )+ {$channel = HIDDEN;};
COMMENT: '"' .* '"' {$channel = HIDDEN;};
STRING: '\'' .* '\'';
BINARY_SYMBOL: ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',') ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',')*;
CHARACTER: '$' . ;
CHUNK_END: '! !' ;
ASSIGNMENT: ':=' | '_';

