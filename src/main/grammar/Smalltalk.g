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
	:	temporaries? statements methods {$n = new Program();}
	;

methods	
	:	method*
	;
	
method 
	: 	('\t+' | '\t-') messagePattern temporaries? statements
	;

messagePattern
	:	unarySelector
	|	binarySelector variableName
	|	(keyword variableName)+
	;

temporaries
	:  '\t'* '|' variableName* '|'
	;

statements
	: 	'\t'* nonEmptyStatements?
	;

nonEmptyStatements
	:	'^' expression '.'?
	|	expression ('.' statements)?
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

unarySelector 
	:	NAME
	;

binarySelector 
	:	BINARY_SYMBOL
	;

variableName 
	:	NAME
	;

keyword 
	:	KEYWORD
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
WHITESPACE: (' ' | '\n' | '\r' | '\f' )+ {$channel = HIDDEN;};

