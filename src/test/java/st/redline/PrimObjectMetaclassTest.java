package st.redline;

import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PrimObjectMetaclassTest {

    @Test
    public void shouldExpandAttributesWhenNewClassVariableAdded() {
        PrimObject newElement = PrimObject.BOOTSTRAPPING ? PrimObject.PRIM_NIL : PrimObject.NIL;
        PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
        int before = metaclass.attributes.length;
        int currentNextVariableIndex = metaclass.nextVariableIndex;
        metaclass.addClassVariableNamed("newvar");
        assertTrue(metaclass.hasClassVariableNamed("newvar"));
        assertEquals(before + 1, metaclass.attributes.length);
        assertEquals(newElement, metaclass.attributes[metaclass.attributes.length - 1]);
        assertEquals(newElement, metaclass.attributes[currentNextVariableIndex]);
    }

    @Test (expected = IllegalStateException.class)
    public void shouldNotBeAbleToAddClassVariableTwice() {
        String name = "var1";
        PrimObject newvar = mock(PrimObject.class);
        PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
        metaclass.addClassVariableNamed(name);
        metaclass.variableAtPut(name, newvar);
        assertEquals(newvar, metaclass.variableAt(name));
        metaclass.addClassVariableNamed(name);
    }

    @Test
    public void shouldSetAndGetClassVariables() {
        String name = "setget";
        PrimObject newvar = mock(PrimObject.class);
        PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
        metaclass.addClassVariableNamed(name);
        metaclass.variableAtPut(name, newvar);
        assertEquals(newvar, metaclass.variableAt(name));
    }

	@Test
	public void shouldLookInGlobalImportsMapWhenFindingPackageForClassName() {
		Map<String, String> imports = mock(Map.class);
		PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
		PrimObjectMetaclass.IMPORTS = imports;
		metaclass.packageFor("ClassName");
		verify(imports).get("ClassName");
	}

	@Test
	public void shouldLookInImportsMapInSuperclassChainWhenFindingPackageForClassName() {
		PrimObjectMetaclass superclass = mock(PrimObjectMetaclass.class);
		when(superclass.packageFor("ClassName")).thenReturn("com.foo.ClassName");
		PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
		metaclass.superclass(superclass);
		assertEquals("com.foo.ClassName", metaclass.packageFor("ClassName"));
	}

	@Test
	public void shouldLookInImportsMapWhenFindingPackageForClassName() {
		Map<String, String> imports = mock(Map.class);
		PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
		metaclass.imports = imports;
		metaclass.packageFor("ClassName");
		verify(imports).get("ClassName");
	}

	@Test
	public void shouldCreateMetaclassSubclass() {
		PrimObjectMetaclass superMeta = new PrimObjectMetaclass();
		PrimObjectMetaclass metaclass = PrimObjectMetaclass.basicSubclassOf(superMeta);
		assertEquals(metaclass.superclass(), superMeta);
	}

	@Test
	public void shouldCreateMetaclassInstance() {
		PrimObjectMetaclass metaclass = PrimObjectMetaclass.basicSubclassOf(null);
		PrimObjectMetaclass aClass = metaclass.basicCreate("aName", null, null, null, null, null);
		assertNotNull(aClass);
		assertEquals(metaclass, aClass.cls());
	}

	@Test
	public void shouldProvideAccessToName() {
		String name = "name";
		PrimObjectMetaclass metaclass = PrimObjectMetaclass.basicSubclassOf(null);
		PrimObjectMetaclass aClass = metaclass.basicCreate(name, null, null, null, null, null);
		assertNotNull(aClass.name());
		assertEquals(aClass.name().javaValue, name);
	}

	@Test
	public void shouldHaveBasicMetaclassAsClassOfMetaclass() {
		PrimObjectMetaclass metaclass = new PrimObjectMetaclass();
		assertEquals(metaclass.cls(), PrimObjectMetaclass.METACLASS);
	}

	@Test
	public void basicMetaClassShouldHaveClass() {
		assertNotNull(PrimObjectMetaclass.METACLASS.cls());
	}

	@Test
	public void shouldNotHaveSuperclass() {
		assertEquals(PrimObject.PRIM_NIL, new PrimObjectMetaclass().superclass());
	}
}
