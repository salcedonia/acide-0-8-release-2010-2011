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
package es.configuration.menu;

import java.util.ArrayList;
import java.util.ResourceBundle;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

import es.configuration.menu.exception.IncorrectMenuConfigurationFileFormatException;
import es.text.AcideTextFile;

/**
 * ACIDE - A Configurable IDE menu configuration.
 * 
 * @version 0.8
 */
public class AcideMenuConfiguration {

	/**
	 * ACIDE - A Configurable IDE menu configuration unique class instance;
	 */
	private static AcideMenuConfiguration _instance;

	/**
	 * ACIDE - A Configurable IDE menu configuration item list.
	 */
	private static ArrayList<AcideMenuItemInformation> _menuItemList;

	/**
	 * Creates a new ACIDE - A Configurable IDE menu configuration.
	 */
	public AcideMenuConfiguration() {

		// Creates the menu item list
		_menuItemList = new ArrayList<AcideMenuItemInformation>();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu configuration unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE menu configuration unique class
	 *         instance.
	 */
	public static AcideMenuConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideMenuConfiguration();
		return _instance;
	}

	/**
	 * Sets all the values to a value given as a parameter.
	 * 
	 * @param values
	 *            new values to set.
	 */
	public void setMenuElementList(
			ArrayList<AcideMenuItemInformation> menuElementList) {
		_menuItemList = menuElementList;
	}

	/**
	 * Saves the ACIDE - A Configurable IDE menu configuration into a
	 * configuration file.
	 * 
	 * @param fileName
	 *            file name.
	 * @param menuElementList
	 *            menu element list to save.
	 */
	public void saveMenuConfigurationFile(String fileName,
			ArrayList<AcideMenuItemInformation> menuElementList) {

		String fileContent = "";

		for (AcideMenuItemInformation menuElement : _menuItemList)
			fileContent += menuElement.getName() + " = "
					+ menuElement.getIsDisplayed() + "\n";

		AcideTextFile f = new AcideTextFile();
		f.write(fileName, fileContent);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE menu configuration from a file and
	 * stores the info in the menu item information array list.
	 * 
	 * @param filePath
	 *            file path.
	 * @return the new menu item list with the loaded values from the menu
	 *         configuration file given as a parameter.
	 */
	public ArrayList<AcideMenuItemInformation> loadMenuConfigurationFile(
			String filePath)
			throws IncorrectMenuConfigurationFileFormatException {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Clear the list
		_menuItemList.clear();

		// Loads the menu configuration file
		AcideTextFile f = new AcideTextFile();
		String fileContent = f.load(filePath);

		// Split the file content by lines
		String[] menuItems = fileContent.split("\n");

		// For each one of the lines, builds the menu item information
		// and adds it to the menu item list.
		for (int index = 0; index < menuItems.length; index++) {

			// Gets the equals symbol index
			int equalsSymbolIndex = menuItems[index].lastIndexOf(" = ");

			// If the format is incorrect
			if (equalsSymbolIndex == -1)
				throw new IncorrectMenuConfigurationFileFormatException(
						labels.getString("s533"));
			else {
				String name = menuItems[index].substring(0, equalsSymbolIndex);
				String isDisplayedString = menuItems[index].substring(
						equalsSymbolIndex + 3, menuItems[index].length());

				boolean isDisplayed = false;

				// Makes the String to boolean conversion
				if (isDisplayedString.matches("true"))
					isDisplayed = true;
				else if (isDisplayedString.matches("false"))
					isDisplayed = false;
				else
					throw new IncorrectMenuConfigurationFileFormatException(
							labels.getString("s533"));

				// Adds the new menu item information
				_menuItemList.add(new AcideMenuItemInformation(name, isDisplayed));
			}
		}

		// Returns the the new menu item list with the loaded values
		return _menuItemList;
	}

	/**
	 * Enables all the menu bar items.
	 */
	public void allMenuItemsEnabled() {

		for (AcideMenuItemInformation menuElement : _menuItemList)
			menuElement.setIsDisplayed(true);
	}

	/**
	 * Disables all the menu bar items <b>except the menu menu item option</b>,
	 * which has to be always displayed.
	 */
	public void allMenuItemsDisabled() {

		for (AcideMenuItemInformation menuItem : _menuItemList)
			if (!menuItem.getName().matches("menu"))
				menuItem.setIsDisplayed(false);
	}

	/**
	 * Returns the menu item list.
	 * 
	 * @return the menu item list.
	 */
	public ArrayList<AcideMenuItemInformation> getMenuItemList() {
		return _menuItemList;
	}

	/**
	 * Returns the menu item display flag of the menu item from the list
	 * specified by the parameter menuItemName.
	 * 
	 * @param menuItemName
	 *            menu item name to get its display flag.
	 * 
	 * @return the menu item display flag of the menu item from the list
	 *         specified by the parameter menuItemName.
	 */
	public boolean getIsDisplayed(String menuItemName) {

		for (AcideMenuItemInformation menuItem : _menuItemList)
			if (menuItem.getName().equals(menuItemName))
				return menuItem.getIsDisplayed();
		return false;
	}
}