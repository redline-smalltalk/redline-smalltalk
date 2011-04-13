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
}

@header {
	package st.redline.smalltalk.interpreter;
}

@lexer::header {
	package st.redline.smalltalk.interpreter;
}

program returns [Program n] 
	:	temporaries? statements methods {$n = new Program($temporaries.n, $statements.n, $methods.n);}
	;

methods returns [Methods n]
	:	{$n = new Methods();}
		( method {$n.add($method.n);} )*
	;
	
method returns [Method n]
	: 	(i = 'def' | c = 'cdef') messagePattern temporaries? statements {$n = MethodFactory.create($i.text, $c.text, $messagePattern.n, $temporaries.n, $statements.n);}
	;

messagePattern returns [MessagePattern n]
	:	unarySelector {$n = new UnarySelectorMessagePattern($unarySelector.n);}
	|	binarySelector variableName {$n = new BinarySelectorMessagePattern($binarySelector.n, $variableName.n);}
	|	( keyword variableName {if ($n != null) ((KeywordMessagePattern)$n).add(new KeywordAndVariableName($keyword.n, $variableName.n)); else $n = new KeywordMessagePattern(new KeywordAndVariableName($keyword.n, $variableName.n));} )+ 
	;

temporaries	 returns [Temporaries n]
	:	{$n = new Temporaries();}
		'|' ( variableName {$n.add(new Temporary($variableName.n));} )* '|'
	;

statements returns [Statements n]
	: 	( nonEmptyStatements {$n = $nonEmptyStatements.n;} )?
	;

nonEmptyStatements returns [Statements n]
	:	'^' expression {$n = new Statements(null);} '.'?
	|	expression ( '.' statements )?
	;

expression
	:	variableName (':=' | '_') expression
	|	simpleExpression
	;

simpleExpression
	:	primary (messageExpression (';' messageElement)*)?
	;

messageElement
	:	unarySelector | binarySelector unaryObjectDescription | (keyword binaryObjectDescription)+
	;

messageExpression
	:	unaryExpression
	|	binaryExpression
	|	keywordExpression
	;

unaryExpression
	:	unarySelector+ (binaryExpression | keywordExpression)?
	;

binaryExpression
	:	(binarySelector unaryObjectDescription)+ keywordExpression?
	;

keywordExpression
	:	(keyword binaryObjectDescription)+
	;

unaryObjectDescription
	:	primary unarySelector*
	;

binaryObjectDescription
	:	primary unarySelector* (binarySelector unaryObjectDescription)*
	;

primary 	
	:	literal
	|	variableName
	|	block
	|	'(' expression ')'
	|	'{' statements '}'
	;

literal 
	:	numberConstant
	|	characterConstant
	|	stringConstant
	|	'#' ( symbol | array )
	;

block 
	:	'[' ((':' variableName)+ '|')? temporaries? statements ']'
	;

array 
	:	'(' arrayConstantElement* ')'
	;

arrayConstantElement
 	:	numberConstant
	|	characterConstant
	|	stringConstant
	|	symbol
	|	array
	;

symbol 
	:	 identifier | binarySelector | keyword
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

identifier 
	:	NAME
	;

characterConstant
	:	CHARACTER
	;

stringConstant
	:	STRING_LITERAL
	;

numberConstant 
	:	DIGITS
	;

DIGITS: ('0'..'9')+;
NAME: ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')*;
KEYWORD: NAME ':';
STRING_LITERAL: '\'' .* '\'';
BINARY_SYMBOL: ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',') ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',')*;
CHARACTER: '$' . ;
COMMENT: '"' .* '"' {$channel = HIDDEN;};
WHITESPACE: (' ' | '\t' | '\n' | '\r' | '\f' )+ {$channel = HIDDEN;};

