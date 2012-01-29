package st.redline;

import org.junit.Test;

import java.util.Hashtable;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class PrimObjectMetaclassTest {

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
	public void basicMetaClassShouldNotHaveClass() {
		assertEquals(null, PrimObjectMetaclass.METACLASS.cls());
	}
}
