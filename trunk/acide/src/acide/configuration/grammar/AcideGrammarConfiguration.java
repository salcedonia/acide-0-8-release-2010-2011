/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
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
package acide.configuration.grammar;

/**
 * ACIDE - A Configurable IDE grammar configuration.
 * 
 * @version 0.8
 */
public class AcideGrammarConfiguration {

	/**
	 * ACIDE - A Configurable IDE grammar configuration default file.
	 */
	public final static String DEFAULT_FILE = "./configuration/grammars/bytes.jar";
	/**
	 * ACIDE - A Configurable IDE grammar configuration default path.
	 */
	public final static String DEFAULT_PATH = "./configuration/grammars/";
	/**
	 * ACIDE - A Configurable IDE grammar configuration unique class instance.
	 */
	private static AcideGrammarConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE grammar configuration path.
	 */
	private String _path;

	/**
	 * Returns the ACIDE - A Configurable IDE grammar configuration unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar configuration unique class
	 *         instance.
	 */
	public static AcideGrammarConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideGrammarConfiguration();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar configuration.
	 */
	public AcideGrammarConfiguration() {

	}

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar configuration.
	 * 
	 * @param path
	 *            absolute configuration file path.
	 */
	public AcideGrammarConfiguration(String path) {
		_path = path;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar configuration path.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar configuration path.
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE grammar configuration
	 * path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar configuration name.
	 */
	public String getName() {

		// Gets the name
		int lastIndexOfSlash = _path.lastIndexOf("\\");
		if (lastIndexOfSlash == -1)
			lastIndexOfSlash = _path.lastIndexOf("/");
		return _path.substring(lastIndexOfSlash + 1, _path.length() - 4);
	}
}
