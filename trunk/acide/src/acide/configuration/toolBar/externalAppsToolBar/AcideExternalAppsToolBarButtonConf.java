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
package acide.configuration.toolBar.externalAppsToolBar;

/**
 * ACIDE - A Configurable IDE external applications tool bar button
 * configuration.
 * 
 * @version 0.8
 */
public class AcideExternalAppsToolBarButtonConf {

	/**
	 * ACIDE - A Configurable IDE external applications tool bar button
	 * configuration number of parameters.
	 */
	public static final int NUMBER_OF_PARAMETERS = 4;
	/**
	 * ACIDE - A Configurable IDE external applications tool bar button
	 * configuration name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE external applications tool bar button
	 * configuration path.
	 */
	private String _path;
	/**
	 * ACIDE - A Configurable IDE external applications tool bar button
	 * configuration hint text.
	 */
	private String _hintText;
	/**
	 * ACIDE - A Configurable IDE external applications tool bar button
	 * configuration icon path to display.
	 */
	private String _icon;

	/**
	 * Creates a new ACIDE - A Configurable IDE external applications tool bar
	 * button configuration.
	 */
	public AcideExternalAppsToolBarButtonConf() {

		// Sets the name by default
		_name = "";
		
		// Sets the path by default
		_path = "";
		
		// Sets the hint text by default
		_hintText = "";

		// Sets the icon by default
		_icon = "";
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE external applications tool bar
	 * button configuration.
	 * 
	 * @param name
	 *            button configuration name.
	 * @param path
	 *            button configuration path.
	 * @param hintText
	 *            button configuration hint text.
	 * @param icon
	 *            button configuration icon.
	 */
	public AcideExternalAppsToolBarButtonConf(String name, String path, String hintText,
			String icon) {

		// Sets the name
		_name = name;

		// Sets the path
		_path = path;
		
		// Sets the hint
		_hintText = hintText;

		// Sets the icon
		_icon = icon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * button configuration hint text.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         button configuration hint text.
	 */
	public String getHintText() {
		return _hintText;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar button configuration hint text.
	 * 
	 * @param hintText
	 *            new value to set.
	 */
	public void setHintText(String hintText) {
		_hintText = hintText;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * button configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         button configuration name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar button configuration name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * button configuration path.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         button configuration path.
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar button configuration path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * button configuration icon.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         button configuration icon.
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar button configuration icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications bar button
	 * configuration has icon flag.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         button configuration has icon flag.
	 */
	public boolean getHasIcon() {
		return !_icon.matches("");
	}
}
