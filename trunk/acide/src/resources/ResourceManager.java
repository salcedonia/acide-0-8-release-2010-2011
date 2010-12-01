package resources;

import java.util.Properties;
import java.util.HashMap;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import operations.log.AcideLog;
import resources.exception.MissedPropertyException;


/************************************************************************																
 * Resource manager of ACIDE - A Configurable IDE. Handles all the resources 
 * of ACIDE - A Configurable IDE.								
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see Properties																												
 ***********************************************************************/
public class ResourceManager {

	/**
	 * Properties configuration file.
	 */
	private static final String CONFIGURATION_FILE = "./src/resources/configuration.xml";
	/**
	 * Unique resource manager class instance.
	 */
	private static ResourceManager _instance;
	/**
	 * Properties to load.
	 */
	private static HashMap<Object, Object> _properties;
	/**
	 * Temporal properties if the loading fails.
	 */
	private static Properties _temporalProperties;
	
	/**
	 * Creates a new resource manager. 
	 * 
	 * Loads the main configuration XML file. 
	 */
	public ResourceManager(){

		try {

			FileInputStream configurationFile = new FileInputStream(CONFIGURATION_FILE);
			_temporalProperties = new Properties();
			_temporalProperties.loadFromXML(configurationFile);
			configurationFile.close();
			_properties = new HashMap<Object, Object>(_temporalProperties);
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the resource manager class unique instance.
	 * 
	 * @return the resource manager class unique instance.
	 */
	public static ResourceManager getInstance(){
		
		if(_instance == null)
			_instance = new ResourceManager();
		return _instance;
	}
	
	/**
	 * Returns the property from the list with the specified name.
	 * 
	 * @param name
	 *            property name.
	 * 
	 * @return the property from the list with the specified name.
	 * 
	 * @throws MissedPropertyException if there is a problem with the 
	 * data loading process.
	 */
	public String getProperty(String name)
			throws MissedPropertyException {

		String value = (String) _properties.get(name);

		if (value == null)
			throw new MissedPropertyException(name);

		return value;
	}

	/**
	 * Sets a new value to a property identified by a name given as a parameter.
	 * 
	 * @param name
	 *            name of the property to modify.
	 * @param value
	 *            new value to set.
	 */
	public void setProperty(String name, String value) {
		try {
			
			_temporalProperties.setProperty(name, value);
			_temporalProperties.storeToXML(new FileOutputStream(
					CONFIGURATION_FILE), "ACIDE Configuration", "UTF-8");
			try {

				FileInputStream fileInputStream = new FileInputStream(CONFIGURATION_FILE);
				_temporalProperties = new Properties();
				_temporalProperties.loadFromXML(fileInputStream);
				fileInputStream.close();
				_properties = new HashMap<Object, Object>(_temporalProperties);

			} catch (Exception exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}