/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import st.redline.classloader.SmalltalkClassLoader;
import st.redline.classloader.Source;
import st.redline.compiler.ast.*;
import st.redline.compiler.ast.Number;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class ProgramAnalyser implements AnalyserDelegate {

    protected final Source source;
    protected final SmalltalkClassLoader smalltalkClassLoader;
    protected final AnalyserFactory analyserFactory;
    protected final BytecodeWriterFactory bytecodeWriterFactory;
    protected final Analyser analyser;
    protected final ClassBytecodeWriter writer;

    private int argumentsIndex = 0;
    private Map<String, Integer> argumentsRegistry;
    private int temporariesIndex;
    private HashMap<String, Integer> temporariesRegistry;
    private boolean sendToSuper = false;
    private boolean hasBlockWithAnswerExpression = false;

    public ProgramAnalyser(Analyser analyser, SmalltalkClassLoader smalltalkClassLoader, AnalyserFactory analyserFactory,
                           BytecodeWriterFactory bytecodeWriterFactory, Source source) {
        this(analyser, smalltalkClassLoader, analyserFactory, bytecodeWriterFactory, source, source.className());
    }

    public ProgramAnalyser(Analyser analyser, SmalltalkClassLoader smalltalkClassLoader, AnalyserFactory analyserFactory,
                           BytecodeWriterFactory bytecodeWriterFactory, Source source, String className) {
        this.analyser = analyser;
        this.smalltalkClassLoader = smalltalkClassLoader;
        this.analyserFactory = analyserFactory;
        this.source = source;
        this.bytecodeWriterFactory = bytecodeWriterFactory;
        this.writer = createClassBytecodeWriter(className);
    }

    protected ClassBytecodeWriter createClassBytecodeWriter(String className) {
        String packageName =  source.packageName();
        return bytecodeWriterFactory.createClassBytecodeWriter(className, packageName);
    }

    public byte[] classBytes() {
        return writer.contents();
    }

    public void visitBegin(Program program) {
        writer.openClass();
    }

    public void visitEnd(Program program) {
        writer.closeClass();
    }

    public void visitBegin(ReferencedClasses referencedClasses) {
    }

    public void visitEnd(ReferencedClasses referencedClasses) {
    }

    public void visitBegin(Statements statements) {
    }

    public void visitEnd(Statements statements) {
    }

    public void visitBegin(Temporaries temporaries) {
        initializeTemporariesRegistration();
        writer.invokeContextTemporariesInit(temporaries.size());
    }

    void initializeTemporariesRegistration() {
        temporariesIndex = 0;
        temporariesRegistry = new HashMap<String, Integer>();
    }

    public void visitEnd(Temporaries temporaries) {
    }

    public void visitBegin(AnswerStatement answerStatements) {
    }

    public void visitEnd(AnswerStatement answerStatements) {
    }

    public void visitBegin(SimpleExpression simpleExpression) {
        hasBlockWithAnswerExpression = simpleExpression.hasBlockWithAnswerExpression();
//        System.out.println("simpleExpression.hasBlockWithAnswerExpression() " + hasBlockWithAnswerExpression);
        if (hasBlockWithAnswerExpression) {
            String blockReturnType = blockReturnType();
            writer.setupTryForBlockReturn(simpleExpression, blockReturnType);
            createBlockReturnType(blockReturnType);
        }
    }

    private void createBlockReturnType(String blockReturnType) {
        new BlockReturnTypeCreator(blockReturnType).create(smalltalkClassLoader);
    }

    private String blockReturnType() {
        return source.packageName() + File.separator + source.className() + "$MAnswer";
    }

    public void visitEnd(SimpleExpression simpleExpression) {
        if (hasBlockWithAnswerExpression)
            writer.setupCatchForBlockReturn(simpleExpression, blockReturnType());
        if (simpleExpression.isResultDuplicatedOnStack())
            writer.pushDuplicate();
        if (!simpleExpression.isResultLeftOnStack())
            writer.pop();
    }

    public void visitBegin(Cascade cascade) {
        writer.pushDuplicate();
    }

    public void visitEnd(Cascade cascade) {
        writer.pop();
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
    }

    public void visitEnd(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
        invokeObjectPerform(selector, argumentCount, line);
    }

    public void visitBegin(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
    }

    public void visitEnd(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
        invokeObjectPerform(selector, argumentCount, line);
    }

    public void visitBegin(AssignmentExpression assignmentExpression) {
    }

    public void visitEnd(AssignmentExpression assignmentExpression) {
    }

    public void visitBegin(Array array) {
        writer.invokeObjectArray(array.size());
    }

    public void visitEnd(Array array) {
    }

    public void visitBegin(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
    }

    public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
    }

    public void visitBegin(Block block, int line) {
        // NOTE: When asking a keyword expression if it contains a block with an answer expression,
        // we should also take that opportunity to mark the block node as being a method block or
        // not, then when we create the block, pass this into the block object.
        // This flag will be used during createBlockInstance().
        String blockClassName = createBlockName();
        String fullBlockClassName = createFullBlockName(blockClassName);
        block.analyser(createBlockAnalyser(blockClassName, block));
        block.blockReturnType(blockReturnType());
        block.outerTemporariesRegistry(temporariesRegistry);
        block.outerArgumentsRegistry(argumentsRegistry);
        smalltalkClassLoader.registerBlockToBeCompiled(block, fullBlockClassName);
        writer.invokeObjectCompileBlock(fullBlockClassName, line);
    }

    protected Analyser createBlockAnalyser(String blockClassName, Block block) {
        Analyser analyserDelegator = analyserFactory.createAnalyser(block, source);
        BlockAnalyser blockAnalyser = analyserFactory.createBlockAnalyser(analyserDelegator, smalltalkClassLoader,
                                        blockClassName, source, block);
        analyserDelegator.currentDelegate(blockAnalyser);
        return analyserDelegator;
    }

    protected String createBlockName() {
        smalltalkClassLoader.BLOCK_NUMBER++;
        return source.className() + "$M" + smalltalkClassLoader.BLOCK_NUMBER;
    }

    protected String createFullBlockName(String blockClassName) {
        String packageName = source.packageName();
        return packageName + (packageName.equals("") ? "" : File.separator) + blockClassName;
    }

    public void visitEnd(Block block, int line) {
    }

    public void visitBegin(BlockArguments blockArguments, int argumentCount) {
        initializeBlockArgumentsRegistration();
    }

    public void visitEnd(BlockArguments blockArguments, int argumentCount) {
    }

    public void visit(BinaryObjectDescription binaryObjectDescription) {
    }

    public void visit(UnaryObjectDescription unaryObjectDescription) {
    }

    public void visit(ReferencedClass referencedClass, String value) {
        // Check is referenced class is in same package and import if it is to save Developer importing classes in
        // same package. If not in same package we ignore as class will be resolved later.
        if (source.isNextTo(value))
            writer.addClassToImports(source.packageName() + File.separator + value);
    }

    public void visit(Temporary temporary, String value, int line) {
        if (temporariesRegistry.containsKey(value))
            System.err.println("WARNING: Temporary '" + value + "' defined twice. Line: " + line);
        temporariesRegistry.put(value, temporariesIndex++);
    }

    public void visit(PrimaryExpression primaryExpression, int line) {
    }

    public void visit(PrimaryStatements primaryStatements, int line) {
    }

    public void visit(Identifier identifier, String value, int line) {
        if (identifier.isOnLoadSideOfExpression()) {
            if (isArgument(value))
                writer.pushArgument(argumentsRegistry.get(value));
            else if (isTemporary(value))
                writer.pushTemporary(temporariesRegistry.get(value));
            else
                writer.invokeVariableAt(value, line);
        } else {
            if (isArgument(value))
                throw new RuntimeException("Can't store into an argument, only temporaries and variables.");
            else if (isTemporary(value))
                writer.storeTemporary(temporariesRegistry.get(value));
            else
                writer.invokeVariablePutAt(value, line);
        }
    }

    private boolean isTemporary(String name) {
        return temporariesRegistry != null && temporariesRegistry.containsKey(name);
    }

    private boolean isArgument(String name) {
        return argumentsRegistry != null && argumentsRegistry.containsKey(name);
    }

    public void visit(Number number, String value, int index, boolean insideArray, int line) {
        writer.invokeObjectNumber(value, line);
        if (insideArray)
            writer.invokeArrayPutAt(index, line);
    }

    public void visit(BlockArgument blockArgument, String value, int line) {
        if (argumentsRegistry.containsKey(value))
            System.err.println("WARNING: Block argument '" + value + "' defined twice. Line: " + line);
        argumentsRegistry.put(value, argumentsIndex++);
    }

    public void visit(Self self, int line) {
        writer.visitLine(line);
        writer.pushReceiver();
    }

    public void visit(Super aSuper, int line) {
        sendToSuper = true;
        writer.pushReceiver();
        writer.pushContext();
    }

    public void visit(True aTrue, int line) {
        pushClassLoaderField("TRUE", line);
    }

    public void visit(False aFalse, int line) {
        pushClassLoaderField("FALSE", line);
    }

    public void pushClassLoaderField(String field, int line) {
        writer.visitLine(line);
        writer.pushClassLoaderField(field);
    }

    public void visit(Nil nil, int line) {
        pushClassLoaderField("NIL", line);
    }

    public void visit(JVM jvm, int line) {
        writer.visitLine(line);
        AnalyserDelegate jvmAnalyser = analyserFactory.createJVMAnalyser(analyser, writer);
        analyser.currentDelegate(jvmAnalyser);
    }

    public void visit(ArrayConstant arrayConstant, int line) {
        writer.visitLine(line);
    }

    public void visit(UnarySelector unarySelector, String selector, int line) {
        invokeObjectPerform(selector, 0, line);
    }

    public void visit(BinarySelector binarySelector, String selector, int line) {
        invokeObjectPerform(selector, 1, line);
    }

    public void visit(CharacterConstant characterConstant, String value, int index, boolean insideArray, int line) {
        writer.invokeObjectCharacter(value, line);
        if (insideArray)
            writer.invokeArrayPutAt(index, line);
    }

    public void visit(StringConstant stringConstant, String value, int index, boolean insideArray, int line) {
        writer.invokeObjectString(value, line);
        if (insideArray)
            writer.invokeArrayPutAt(index, line);
    }

    public void visit(Symbol symbol, String value, int index, boolean insideArray, int line) {
        writer.invokeObjectSymbol(value, line);
        if (insideArray)
            writer.invokeArrayPutAt(index, line);
    }

    public void visit(SymbolConstant symbolConstant, String value, int line) {
        writer.invokeObjectSymbol(value, line);
    }

    public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
        invokeObjectPerform(value, 0, line);
    }

    public void visit(Primitive primitive, String keyword, int line, String digits) {
        writer.invokePrimitive(line, digits);
    }

    public boolean skipBlockVisit(Block block) {
        return true;
    }

    private void invokeObjectPerform(String selector, int argumentCount, int line) {
        writer.visitLine(line);
        writer.invokeObjectPerform(selector, argumentCount, sendToSuper);
        sendToSuper = false;
    }

    private void initializeBlockArgumentsRegistration() {
        argumentsIndex = 0;
        argumentsRegistry = new HashMap<String, Integer>();
    }
}
