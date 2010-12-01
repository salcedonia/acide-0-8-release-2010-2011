package es.configuration.menu;

import java.util.ArrayList;
import java.util.ResourceBundle;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

import es.configuration.menu.exception.IncorrectMenuConfigurationFileFormatException;
import es.text.TextFile;

/************************************************************************
 * Menu configuration of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
 ***********************************************************************/
public class MenuConfiguration {

	/**
	 * Menu configuration unique class instance;
	 */
	private static MenuConfiguration _instance;

	/**
	 * Menu item list.
	 */
	private static ArrayList<MenuItemInformation> _menuItemList;

	/**
	 * Creates a new menu configuration.
	 */
	public MenuConfiguration() {

		// Creates the menu item list
		_menuItemList = new ArrayList<MenuItemInformation>();
	}

	/**
	 * Returns the menu configuration unique class instance.
	 * 
	 * @return the menu configuration unique class instance.
	 */
	public static MenuConfiguration getInstance() {

		if (_instance == null)
			_instance = new MenuConfiguration();
		return _instance;
	}

	/**
	 * Sets all the values to a value given as a parameter.
	 * 
	 * @param values
	 *            new values to set.
	 */
	public void setMenuElementList(
			ArrayList<MenuItemInformation> menuElementList) {
		_menuItemList = menuElementList;
	}

	/**
	 * Saves the menu configuration into a configuration file.
	 * 
	 * @param fileName
	 *            file name.
	 * @param menuElementList
	 *            menu element list to save.
	 */
	public void saveMenuConfigurationFile(String fileName,
			ArrayList<MenuItemInformation> menuElementList) {

		String fileContent = "";

		for (MenuItemInformation menuElement : _menuItemList)
			fileContent += menuElement.getName() + " = "
					+ menuElement.getIsDisplayed() + "\n";

		TextFile f = new TextFile();
		f.save(fileName, fileContent);
	}

	/**
	 * Loads the menu configuration from a file and stores the info in the menu
	 * item information array list.
	 * 
	 * @param filePath
	 *            file path.
	 * @return the new menu item list with the loaded values from the menu
	 *         configuration file given as a parameter.
	 */
	public ArrayList<MenuItemInformation> loadMenuConfigurationFile(
			String filePath)
			throws IncorrectMenuConfigurationFileFormatException {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
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
		TextFile f = new TextFile();
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
				else
					if (isDisplayedString.matches("false"))
						isDisplayed = false;
					else
						throw new IncorrectMenuConfigurationFileFormatException(
								labels.getString("s533"));
				
				// Adds the new menu item information
				_menuItemList.add(new MenuItemInformation(name, isDisplayed));
			}
		}

		// Returns the the new menu item list with the loaded values
		return _menuItemList;
	}

	/**
	 * Enables all the menu bar items.
	 */
	public void allMenuItemsEnabled() {

		for (MenuItemInformation menuElement : _menuItemList)
			menuElement.setIsDisplayed(true);
	}

	/**
	 * Disables all the menu bar items <b>except the menu menu item option</b>,
	 * which has to be always displayed.
	 */
	public void allMenuItemsDisabled() {

		for (MenuItemInformation menuItem : _menuItemList)
			if (!menuItem.getName().matches("menu"))
				menuItem.setIsDisplayed(false);
	}

	/**
	 * Returns the menu item list.
	 * 
	 * @return the menu item list.
	 */
	public ArrayList<MenuItemInformation> getMenuItemList() {
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

		for (MenuItemInformation menuItem : _menuItemList)
			if (menuItem.getName().equals(menuItemName))
				return menuItem.getIsDisplayed();
		return false;
	}
}