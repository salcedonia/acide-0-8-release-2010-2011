package properties;

import java.util.Properties;
import java.util.HashMap;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import properties.exception.MissedPropertyException;

/**
 *
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
public class PropertiesManager {

	/**
	 * 
	 */
	private static final String CONFIGURATION_FILE = "./src/properties/main/configuration.properties";
	/**
	 * 
	 */
	private static HashMap<?, ?> _properties;
	/**
	 * 
	 */
	private static Properties _temporalProperties;

	/**
	 * 
	 */
	static {

		try {

			FileInputStream f = new FileInputStream(CONFIGURATION_FILE);
			_temporalProperties = new Properties();
			_temporalProperties.load(f);
			f.close();
			_properties = new HashMap(_temporalProperties);

		} catch (Exception e) {
			System.out.print(e);
		}
	}

	private PropertiesManager() {
	}

	/**
	 * 
	 * 
	 * @param name
	 * @return
	 * @throws MissedPropertyException
	 */
	public static String getProperty(String name)
			throws MissedPropertyException {

		String value = (String) _properties.get(name);

		if (value == null || value.contains("(Sin especificar)"))
			throw new MissedPropertyException(name);

		return value;
	}

	/**
	 * 
	 * 
	 * @param name
	 * @param value
	 */
	public static void setProperty(String name, String value) {
		try {
			_temporalProperties.setProperty(name, value);
			_temporalProperties.store(new FileOutputStream(CONFIGURATION_FILE),
					null);
			try {

				FileInputStream f = new FileInputStream(CONFIGURATION_FILE);
				_temporalProperties = new Properties();
				_temporalProperties.load(f);
				f.close();
				_properties = new HashMap(_temporalProperties);

			} catch (Exception e) {
				System.out.print(e);
			}
		} catch (Exception e) {

			System.out.print(e);
		}
	}
}