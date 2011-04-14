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
package acide.configuration.toolBar;

import acide.configuration.toolBar.consolePanelToolBar.AcideConsolePanelToolBarConfiguration;
import acide.configuration.toolBar.externalAppsToolBar.AcideExternalAppsToolBarConfiguration;
import acide.configuration.toolBar.menuBarToolBar.AcideMenuBarToolBarConfiguration;

/**
 * ACIDE - A Configurable IDE tool bar configuration.
 * 
 * @version 0.8
 */
public class AcideToolBarConfiguration {

	/**
	 * ACIDE - A Configurable IDE tool bar configuration unique class instance.
	 */
	private static AcideToolBarConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar configuration.
	 */
	private AcideMenuBarToolBarConfiguration _menuBarToolBarConfiguration;
	/**
	 * ACIDE - A Configurable IDE console panel tool bar configuration.
	 */
	private AcideConsolePanelToolBarConfiguration _consolePanelToolBarConfiguration;
	/**
	 * ACIDE - A Configurable IDE external applications tool bar configuration.
	 */
	private AcideExternalAppsToolBarConfiguration _externalAppsToolBarConfiguration;

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar configuration unique
	 * class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar configuration unique
	 *         class instance.
	 */
	public static AcideToolBarConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideToolBarConfiguration();

		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar configuration.
	 */
	public AcideToolBarConfiguration() {

		// Creates the menu bar tool bar configuration
		_menuBarToolBarConfiguration = new AcideMenuBarToolBarConfiguration();

		// Creates the console panel tool bar configuration
		_consolePanelToolBarConfiguration = new AcideConsolePanelToolBarConfiguration();

		// Creates the menu bar tool bar configuration
		_externalAppsToolBarConfiguration = new AcideExternalAppsToolBarConfiguration();
	}

	/**
	 * Loads the ACIDE - A Configurable IDE tool bar configuration from the file
	 * given as a parameter.
	 * 
	 * @param filePath
	 *            file path.
	 */
	public void load(String filePath) {

		// Loads the menu bar tool bar configuration
		_menuBarToolBarConfiguration.load(filePath);
		
		// Loads the console panel tool bar configuration final list
		_consolePanelToolBarConfiguration.loadLists(filePath);
			
		// Loads the external applications tool bar configuration
		_externalAppsToolBarConfiguration.loadLists(filePath);
	}

	/**
	 * Saves the ACIDE - A Configurable IDE tool bar configuration into the file
	 * given as a parameter.
	 * 
	 * @param filePath
	 *            file path.
	 */
	public void save(String filePath) {

	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu bar tool bar configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE menu bar tool bar configuration.
	 */
	public AcideMenuBarToolBarConfiguration getMenuBarToolBarConfiguration() {
		return _menuBarToolBarConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu bar tool bar
	 * configuration.
	 * 
	 * @param menuBarToolBarConfiguration
	 *            new value to set.
	 */
	public void setMenuBarToolBarConfiguration(
			AcideMenuBarToolBarConfiguration menuBarToolBarConfiguration) {
		_menuBarToolBarConfiguration = menuBarToolBarConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar
	 *         configuration.
	 */
	public AcideConsolePanelToolBarConfiguration getConsolePanelToolBarConfiguration() {
		return _consolePanelToolBarConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * configuration.
	 * 
	 * @param consolePanelToolBarConfiguration
	 *            new value to set.
	 */
	public void setConsolePanelToolBarConfiguration(
			AcideConsolePanelToolBarConfiguration consolePanelToolBarConfiguration) {
		_consolePanelToolBarConfiguration = consolePanelToolBarConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         configuration.
	 */
	public AcideExternalAppsToolBarConfiguration getExternalAppsToolBarConfiguration() {
		return _externalAppsToolBarConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar configuration.
	 * 
	 * @param externalAppsToolBarConfiguration
	 *            new value to set.
	 */
	public void setExternalAppsToolBarConfiguration(
			AcideExternalAppsToolBarConfiguration externalAppsToolBarConfiguration) {
		_externalAppsToolBarConfiguration = externalAppsToolBarConfiguration;
	}
}
