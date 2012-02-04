package st.redline;

import org.junit.Test;

import java.util.Hashtable;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PrimObjectMetaclassTest {

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
