/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package resources;

import java.util.Properties;
import java.util.HashMap;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import operations.log.AcideLog;
import resources.exception.MissedPropertyException;


/**																
 * ACIDE - A Configurable IDE resource manager. 
 * 
 * Handles all the resources of ACIDE - A Configurable IDE.								
 *					
 * @version 0.8	
 * @see Properties																												
 */
public class AcideResourceManager {

	/**
	 * ACIDE - A Configurable IDE resource manager properties configuration file.
	 */
	private static final String CONFIGURATION_FILE = "./src/resources/configuration.xml";
	/**
	 * ACIDE - A Configurable IDE resource manager unique class instance.
	 */
	private static AcideResourceManager _instance;
	/**
	 * ACIDE - A Configurable IDE resource manager properties to load.
	 */
	private static HashMap<Object, Object> _properties;
	/**
	 * ACIDE - A Configurable IDE resource manager temporal properties if the loading fails.
	 */
	private static Properties _temporalProperties;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE resource manager. 
	 * 
	 * Loads the main configuration XML file. 
	 */
	public AcideResourceManager(){

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
	 * Returns the ACIDE - A Configurable IDE resource manager class unique instance.
	 * 
	 * @return the ACIDE - A Configurable IDE resource manager class unique instance.
	 */
	public static AcideResourceManager getInstance(){
		
		if(_instance == null)
			_instance = new AcideResourceManager();
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