package properties;

import java.util.Properties;
import java.util.HashMap;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import properties.exception.MissedPropertyException;

/**
 * Properties Manager of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class PropertiesManager {

	/**
	 * Properties configuration file.
	 */
	private static final String CONFIGURATION_FILE = "./src/properties/main/configuration.xml";
	/**
	 * Properties to load.
	 */
	private static HashMap<Object, Object> _properties;
	/**
	 * Temporal properties if the loading fails.
	 */
	private static Properties _temporalProperties;

	static {

		try {

			FileInputStream f = new FileInputStream(CONFIGURATION_FILE);
			_temporalProperties = new Properties();
			_temporalProperties.loadFromXML(f);
			f.close();
			_properties = new HashMap<Object, Object>(_temporalProperties);
		} catch (Exception e) {
			System.err.println(e.getMessage());
		}
	}

	/**
	 * constructor of the class
	 */
	private PropertiesManager() {

	}

	/**
	 * Returns the property from the list with the specified name.
	 * 
	 * @param name
	 *            Property name.
	 * 
	 * @return The property from the list with the specified name.
	 * 
	 * @throws MissedPropertyException
	 */
	public static String getProperty(String name)
			throws MissedPropertyException {

		String value = (String) _properties.get(name);

		if (value == null)
			throw new MissedPropertyException(name);

		return value;
	}

	/**
	 * Set a new value to a property identified by a name given as a parameter.
	 * 
	 * @param name
	 *            Property name.
	 * @param value
	 *            New value to set.
	 */
	public static void setProperty(String name, String value) {
		try {
			_temporalProperties.setProperty(name, value);
			_temporalProperties.storeToXML(new FileOutputStream(
					CONFIGURATION_FILE), "ACIDE Configuration", "UTF-8");
			try {

				FileInputStream f = new FileInputStream(CONFIGURATION_FILE);
				_temporalProperties = new Properties();
				_temporalProperties.loadFromXML(f);
				f.close();
				_properties = new HashMap<Object, Object>(_temporalProperties);

			} catch (Exception e) {
			}
		} catch (Exception e) {
		}
	}
}