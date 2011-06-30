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

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline.bootstrap;

import st.redline.RObject;
import st.redline.Smalltalk;

import static st.redline.RObject.createClassWith;

public class Bootstrapper {

	protected static final String FULL_SUBCLASSING_SELECTOR = "subclass:instanceVariableNames:classVariableNames:classInstanceVariableNames:poolDictionaries:category:";
	protected static final String NO_CATEGORY_SUBCLASSING_SELECTOR = "subclass:instanceVariableNames:classVariableNames:classInstanceVariableNames:poolDictionaries:";
	protected static final String SHORT_SUBCLASSING_SELECTOR = "subclass:";
	protected static final String CLASSBUILDER_SELECTOR = "superclass:subclass:instanceVariableNames:classVariableNames:classInstanceVariableNames:poolDictionaries:category:";
	protected static final String ADDINSTVAR_SELECTOR = "addInstVarNamed:";
	protected static final String ADDCLASSVAR_SELECTOR = "addClassVarNamed:";
	protected static final String ADDCLASSINSTVAR_SELECTOR = "addClassInstVarNamed:";
	protected static final String ADDSHAREDPOOL_SELECTOR = "addSharedPool:";
	protected static final String CATEGORY_SELECTOR = "category:";
	protected static final String METACLASS = "Metaclass";
	protected static final String PROTOOBJECT = "ProtoObject";
	protected static final String OBJECT = "Object";
	protected static final String CLASS = "Class";
	protected static final String CLASS_DESCRIPTION = "ClassDescription";
	protected static final String UNDEFINED_OBJECT = "UndefinedObject";
	protected static final String NIL = "nil";
	protected static final String BEHAVIOR = "Behavior";
	protected static final String COLLECTION = "Collection";
	protected static final String SEQUENCEABLE_COLLECTION = "SequenceableCollection";
	protected static final String ARRAYED_COLLECTION = "ArrayedCollection";
	protected static final String STRING = "String";
	protected static final String SYMBOL = "Symbol";
	protected static final String CLASS_BUILDER = "ClassBuilder";
	protected static final String NEW_SELECTOR = "new";
	protected static final String COMPILE_INSTANCE_METHOD_SELECTOR = "compileInstanceMethod:at:fromSource:";
	protected static final String COMPILE_CLASS_METHOD_SELECTOR = "compileClassMethod:at:fromSource:";
	protected static final String PRIMITIVE_SOURCE_PATH_SELECTOR = "primitiveSourceFile:";
	protected static final String INITIALIZE_CLASSINSTVARS_SELECTOR = "primitiveInitializeClassInstanceVariables";

	private final Smalltalk smalltalk;

	public Bootstrapper(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public void bootstrap() {
		createClasses();
		fixupReferences();
		registerNil(makeNil());
		addMethods();
	}

	private void addMethods() {
		string().cls().methodAtPut(NEW_SELECTOR, new PrimitiveNewMethod());
		classBuilder().cls().methodAtPut(NEW_SELECTOR, new PrimitiveNewMethod());
		classBuilder().cls().methodAtPut(CLASSBUILDER_SELECTOR, new ClassBuilderBuildSubclassMethod());
		undefinedObject().cls().methodAtPut(NEW_SELECTOR, new PrimitiveNewNotAllowedMethod());
		classClass().methodAtPut(FULL_SUBCLASSING_SELECTOR, new PrimitiveFullSubclassMethod(this));
		classClass().methodAtPut(NO_CATEGORY_SUBCLASSING_SELECTOR, new PrimitiveNoCategorySubclassMethod());
		classClass().methodAtPut(SHORT_SUBCLASSING_SELECTOR, new PrimitiveShortSubclassMethod());
		classClass().methodAtPut(ADDINSTVAR_SELECTOR, new AddInstVarNamedMethod());
		classClass().methodAtPut(ADDCLASSVAR_SELECTOR, new AddClassVarNamedMethod());
		classClass().methodAtPut(ADDCLASSINSTVAR_SELECTOR, new AddClassInstVarNamedMethod());
		classClass().methodAtPut(ADDSHAREDPOOL_SELECTOR, new AddSharedPoolMethod());
		classClass().methodAtPut(CATEGORY_SELECTOR, new SetCategoryMethod());
		classClass().methodAtPut(COMPILE_INSTANCE_METHOD_SELECTOR, new CompileMethod(false, smalltalk));
		classClass().methodAtPut(COMPILE_CLASS_METHOD_SELECTOR, new CompileMethod(true, smalltalk));
		classClass().methodAtPut(PRIMITIVE_SOURCE_PATH_SELECTOR, new PrimitiveSourcePathMethod());
		classClass().methodAtPut(INITIALIZE_CLASSINSTVARS_SELECTOR, new InitializeClassInstanceVariables());
	}

	private void registerNil(RObject nil) {
		smalltalk.primitiveAtPut(NIL, nil);
	}

	private RObject makeNil() {
		return RObject.createInstanceWith(undefinedObject());
	}

	private void fixupReferences() {
		protoObject().cls(protoObjectMetaclass());
		object().cls(objectMetaclass());
		behavior().cls(behaviorMetaclass());
		classDescription().cls(classDescriptionMetaclass());
		classClass().cls(classClassMetaclass());
		undefinedObject().cls(undefinedObjectMetaclass());
		metaclass().cls(metaclassMetaclass());
		collection().cls(collectionMetaclass());
		sequenceableCollection().cls(sequenceableCollectionMetaclass());
		arrayedCollection().cls(arrayedCollectionMetaclass());
		string().cls(stringMetaclass());
		symbol().cls(symbolMetaclass());
		classBuilder().cls(classBuilderMetaclass());
	}

	private void createClasses() {
		protoObject(createProtoObject());
		object(createObject());
		behavior(createBehavior());
		classDescription(createClassDescription());
		classClass(createClassClass());
		metaclass(createMetaclass());
		undefinedObject(createUndefinedObject());
		collection(createCollection());
		sequenceableCollection(createSequenceableCollection());
		arrayedCollection(createArrayedCollection());
		string(createString());
		symbol(createSymbol());
		classBuilder(createClassBuilder());
	}

	private RObject classBuilderMetaclass() {
		return metaclassInstance(object().cls());
	}

	private void classBuilder(RObject object) {
		smalltalk.primitiveAtPut(CLASS_BUILDER, object);
	}

	public RObject classBuilder() {
		return smalltalk.primitiveAt(CLASS_BUILDER);
	}

	private RObject createClassBuilder() {
		return createClass(CLASS_BUILDER, object());
	}

	private RObject symbolMetaclass() {
		return metaclassInstance(string().cls());
	}

	private void symbol(RObject object) {
		smalltalk.primitiveAtPut(SYMBOL, object);
	}

	private RObject symbol() {
		return smalltalk.primitiveAt(SYMBOL);
	}

	private RObject createSymbol() {
		return createClass(SYMBOL, string());
	}

	private RObject stringMetaclass() {
		return metaclassInstance(arrayedCollection().cls());
	}

	private void string(RObject object) {
		smalltalk.primitiveAtPut(STRING, object);
	}

	private RObject string() {
		return smalltalk.primitiveAt(STRING);
	}

	private RObject createString() {
		return createClass(STRING, arrayedCollection());
	}

	private RObject arrayedCollectionMetaclass() {
		return metaclassInstance(sequenceableCollection().cls());
	}

	private void arrayedCollection(RObject object) {
		smalltalk.primitiveAtPut(ARRAYED_COLLECTION, object);
	}

	private RObject arrayedCollection() {
		return smalltalk.primitiveAt(ARRAYED_COLLECTION);
	}

	private RObject createArrayedCollection() {
		return createClass(ARRAYED_COLLECTION, sequenceableCollection());
	}

	private RObject collectionMetaclass() {
		return metaclassInstance(object().cls());
	}

	private void collection(RObject object) {
		smalltalk.primitiveAtPut(COLLECTION, object);
	}

	private RObject collection() {
		return smalltalk.primitiveAt(COLLECTION);
	}

	private RObject createCollection() {
		return createClass(COLLECTION, object());
	}

	private RObject sequenceableCollectionMetaclass() {
		return metaclassInstance(collection().cls());
	}

	private void sequenceableCollection(RObject object) {
		smalltalk.primitiveAtPut(SEQUENCEABLE_COLLECTION, object);
	}

	private RObject sequenceableCollection() {
		return smalltalk.primitiveAt(SEQUENCEABLE_COLLECTION);
	}

	private RObject createSequenceableCollection() {
		return createClass(SEQUENCEABLE_COLLECTION, collection());
	}

	private RObject undefinedObjectMetaclass() {
		return metaclassInstance(object().cls());
	}

	private RObject classClassMetaclass() {
		return metaclassInstance(classDescription().cls());
	}

	private RObject classDescriptionMetaclass() {
		return metaclassInstance(behavior().cls());
	}

	private RObject behaviorMetaclass() {
		return metaclassInstance(object().cls());
	}

	private RObject metaclassMetaclass() {
		return metaclassInstance(classDescription().cls());
	}

	private RObject objectMetaclass() {
		return metaclassInstance(protoObject().cls());
	}

	private RObject protoObjectMetaclass() {
		return metaclassInstance(classClass());
	}

	public RObject metaclassInstance(RObject superclass) {
		if (superclass == null)
			throw new IllegalStateException("Metaclass superclass cannot be null.");
		return createClassWith(metaclass(), superclass);
	}

	private void object(RObject object) {
		smalltalk.primitiveAtPut(OBJECT, object);
	}

	private RObject object() {
		return smalltalk.primitiveAt(OBJECT);
	}

	private RObject createObject() {
		return createClass(OBJECT, protoObject());
	}

	private void behavior(RObject behavior) {
		smalltalk.primitiveAtPut(BEHAVIOR, behavior);
	}

	private RObject behavior() {
		return smalltalk.primitiveAt(BEHAVIOR);
	}

	private RObject createBehavior() {
		return createClass(BEHAVIOR, object());
	}

	private void classDescription(RObject classDescription) {
		smalltalk.primitiveAtPut(CLASS_DESCRIPTION, classDescription);
	}

	private RObject classDescription() {
		return smalltalk.primitiveAt(CLASS_DESCRIPTION);
	}

	private RObject createClassDescription() {
		return createClass(CLASS_DESCRIPTION, behavior());
	}

	private RObject createClassClass() {
		return createClass(CLASS, classDescription());
	}

	public RObject createClass(String name, RObject superclass) {
		// System.out.println("createClass() " + name);
		RObject cls = createClassWith(null, superclass);
		cls.bootstrapped(true);
		cls.primitiveName(name);
		return cls;
	}

	private RObject classClass() {
		return smalltalk.primitiveAt(CLASS);
	}

	private void classClass(RObject cls) {
		smalltalk.primitiveAtPut(CLASS, cls);
	}

	private void metaclass(RObject metaclass) {
		smalltalk.primitiveAtPut(METACLASS, metaclass);
	}

	private RObject metaclass() {
		return smalltalk.primitiveAt(METACLASS);
	}

	private RObject createMetaclass() {
		return createClass(METACLASS, classDescription());
	}

	private void protoObject(RObject protoObject) {
		smalltalk.primitiveAtPut(PROTOOBJECT, protoObject);
	}

	private RObject protoObject() {
		return smalltalk.primitiveAt(PROTOOBJECT);
	}

	private RObject createProtoObject() {
		return createClass(PROTOOBJECT, null);
	}

	private void undefinedObject(RObject undefinedObject) {
		smalltalk.primitiveAtPut(UNDEFINED_OBJECT, undefinedObject);
	}

	private RObject undefinedObject() {
		return smalltalk.primitiveAt(UNDEFINED_OBJECT);
	}

	private RObject createUndefinedObject() {
		return createClass(UNDEFINED_OBJECT, object());
	}
}
