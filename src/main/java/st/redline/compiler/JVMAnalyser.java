/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.objectweb.asm.Opcodes;

import java.util.HashMap;
import java.util.Map;

public class JVMAnalyser implements AnalyserDelegate, Opcodes {

    final static Map<String, Builder> builders = new HashMap<String, Builder>();
    static {
        builders.put("getStatic:named:as:", new VisitFieldInsnBuilder(GETSTATIC));
        builders.put("ldc:", new VisitLdcInsnBuilder());
        builders.put("aload:", new VisitVarInsnBuilder());
	    builders.put("invokeVirtual:method:returning:", new VisitMethodInsnBuilder(INVOKEVIRTUAL));
	    builders.put("invokeSpecial:method:returning:", new VisitMethodInsnBuilder(INVOKESPECIAL));
	    builders.put("invokeInterface:method:returning:", new VisitMethodInsnBuilder(INVOKEINTERFACE));
	    builders.put("new:", new VisitTypeInsnBuilder(NEW));
	    builders.put("checkcast:", new VisitTypeInsnBuilder(CHECKCAST));
	    // commands added by Redline - not true JVM instructions.
        builders.put("arg:", new VisitArgumentFetchBuilder());
    }

    protected final Analyser analyser;
	protected final ClassBytecodeWriter writer;
	private final boolean verbose;

	private int nesting;
    Builder builder;

    JVMAnalyser(Analyser analyser, ClassBytecodeWriter classBytecodeWriter, boolean verbose) {
		this.analyser = analyser;
		this.writer = classBytecodeWriter;
		this.verbose = verbose;
		this.nesting = 1;
	}

	public byte[] classBytes() {
		throw new IllegalStateException("Should not be calling this on JVMAnalyser.");
	}

	public void visitBegin(Program program) {
	}

	public void visitEnd(Program program) {
	}

	public void visitBegin(Temporaries temporaries) {
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visitBegin(Statements statements) {
	}

	public void visitEnd(Statements statements) {
	}

	public void visitBegin(AnswerStatement answerStatement) {
	}

	public void visitEnd(AnswerStatement answerStatement) {
	}

	public void visitBegin(SimpleExpression simpleExpression) {
		nesting++;
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		nesting--;
		if (nesting == 0) {
			analyser.previousDelegate();
		}
	}

	public void visitBegin(Cascade cascade) {
	}

	public void visitEnd(Cascade cascade) {
	}

	public void visitBegin(UnaryExpression unaryExpression) {
	}

	public void visitEnd(UnaryExpression unaryExpression) {
	}

	public void visitBegin(BinaryExpression binaryExpression) {
	}

	public void visitEnd(BinaryExpression binaryExpression) {
	}

	public void visitBegin(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
        if (!builders.containsKey(selector))
            throw new IllegalStateException("Builder not found for keyword: '" + selector + "'.");
        builder = builders.get(selector).create();
	}

	public void visitEnd(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
        builder.writeUsing(writer);
	}

	public void visitBegin(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
        if (!builders.containsKey(selector))
            throw new IllegalStateException("Builder not found for keyword: '" + selector + "'.");
        builder = builders.get(selector).create();
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
        builder.writeUsing(writer);
	}

	public void visitBegin(AssignmentExpression assignmentExpression) {
	}

	public void visitEnd(AssignmentExpression assignmentExpression) {
	}

	public void visitBegin(Array array) {
	}

	public void visitEnd(Array array) {
	}

	public void visitBegin(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
	}

	public void visitBegin(Block block, int line) {
	}

	public void visitEnd(Block block, int line) {
	}

	public void visitBegin(BlockArguments blockArguments, int argumentCount) {
	}

	public void visitEnd(BlockArguments blockArguments, int argumentCount) {
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(Temporary temporary, String value, int line) {
	}

	public void visit(PrimaryExpression primaryExpression, int line) {
	}

	public void visit(PrimaryStatements primaryStatements, int line) {
	}

	public void visit(Identifier identifier, String value, int line) {
	}

	public void visit(Number number, String value, int index, boolean insideArray, int line) {
		builder.addArgument(Integer.valueOf(value));
	}

	public void visit(BlockArgument blockArgument, String value, int line) {
	}

	public void visit(Self self, int line) {
		writer.pushReceiver();
	}

	public void visit(Super aSuper, int line) {
	}

	public void visit(True aTrue, int line) {
		writer.pushObjectStaticField("TRUE");
	}

	public void visit(False aFalse, int line) {
		writer.pushObjectStaticField("FALSE");
	}

	public void visit(Nil nil, int line) {
		writer.pushObjectStaticField("NIL");
	}

	public void visit(JVM jvm, int line) {
		writer.visitLine(line);
	}

	public void visit(ArrayConstant arrayConstant, int line) {
	}

	public void visit(UnarySelector unarySelector, String selector, int line) {
		writer.visitLine(line);
		writer.visitInsn(selector.toUpperCase());
	}

	public void visit(BinarySelector binarySelector, String selector, int line) {
	}

	public void visit(CharacterConstant characterConstant, String value, int index, boolean insideArray, int line) {
	}

	public void visit(StringConstant stringConstant, String value, int index, boolean insideArray, int line) {
		builder.addArgument(value);
	}

	public void visit(Symbol symbol, String value, int index, boolean insideArray, int line) {
	}

	public void visit(SymbolConstant symbolConstant, String value, int line) {
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		writer.visitLine(line);
		writer.visitInsn(value.toUpperCase());
	}

	public void visit(Primitive primitive, String keyword, int line, String digits) {
	}

	public boolean skipBlockVisit(Block block) {
		throw new IllegalStateException("Should not be calling this on JVMAnalyser.");
	}

    static abstract class Builder {

        int opcode;
        int argumentCount;
        Object[] arguments;
	    int offset = 0;

        abstract Builder create();
        abstract void writeUsing(ClassBytecodeWriter writer);

        public Builder(int opcode, int argumentCount) {
            this.opcode = opcode;
            this.argumentCount = argumentCount;
            this.arguments = new Object[argumentCount];
        }
        
	    void addArgument(String argument) {
		    arguments[offset] = argument;
		    offset++;
	    }

	    void addArgument(Integer argument) {
		    arguments[offset] = argument;
		    offset++;
	    }

        String string(int index) {
            return String.valueOf(arguments[index]);
        }

	    Integer number(int index) {
		    return Integer.valueOf(string(index));
	    }
    }

    static class VisitFieldInsnBuilder extends Builder {
        VisitFieldInsnBuilder(int opcode) { super(opcode, 3); }
        Builder create() { return new VisitFieldInsnBuilder(opcode); }
        void writeUsing(ClassBytecodeWriter writer) {
	        writer.visitFieldInsn(opcode, string(0), string(1), string(2));
        }
    }

	static class VisitMethodInsnBuilder extends Builder {
		VisitMethodInsnBuilder(int opcode) { super(opcode, 3); }
		Builder create() { return new VisitMethodInsnBuilder(opcode); }
		void writeUsing(ClassBytecodeWriter writer) {
			writer.visitMethodInsn(opcode, string(0), string(1), string(2));
		}
	}

	static class VisitLdcInsnBuilder extends Builder {
		VisitLdcInsnBuilder() { super(0, 1); }
		Builder create() { return new VisitLdcInsnBuilder(); }
		void writeUsing(ClassBytecodeWriter writer) {
			if (arguments[0] instanceof java.lang.String)
				writer.visitLdcInsn(string(0));
			else if (arguments[0] instanceof Integer)
				writer.pushNumber(number(0));
			else
				throw new IllegalArgumentException("Unsupported Ldc type: " + arguments[0]);
		}
	}

	static class VisitVarInsnBuilder extends Builder {
		VisitVarInsnBuilder() { super(0, 1); }
		Builder create() { return new VisitVarInsnBuilder(); }
		void writeUsing(ClassBytecodeWriter writer) {
			if (arguments[0] instanceof Integer)
				writer.visitVarInsn(ALOAD, number(0));
			else
				throw new IllegalArgumentException("Unsupported Var type: " + arguments[0]);
		}
	}

	static class VisitTypeInsnBuilder extends Builder {
		VisitTypeInsnBuilder(int opcode) { super(opcode, 1); }
		Builder create() { return new VisitTypeInsnBuilder(opcode); }
		void writeUsing(ClassBytecodeWriter writer) {
			writer.visitTypeInsn(opcode, string(0));
		}
	}

    // this builder provides access to method arguments
    static class VisitArgumentFetchBuilder extends Builder {
        VisitArgumentFetchBuilder() { super(0, 1); }
        Builder create() { return new VisitArgumentFetchBuilder(); }
        void writeUsing(ClassBytecodeWriter writer) {
            if (arguments[0] instanceof Integer)
                writer.pushArgument(number(0));
            else
                throw new IllegalArgumentException("Unsupported Arg type: " + arguments[0]);
        }
    }
}
