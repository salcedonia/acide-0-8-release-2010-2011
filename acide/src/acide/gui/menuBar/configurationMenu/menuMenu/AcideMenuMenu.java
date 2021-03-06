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
package acide.gui.menuBar.configurationMenu.menuMenu;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideLoadMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideModifyMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideNewMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideSaveMenuAsMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideSaveMenuMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE menu menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideMenuMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE menu menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE menu menu new menu menu item name.
	 */
	public static final String NEW_MENU_NAME = "New Menu";
	/**
	 * ACIDE - A Configurable IDE menu menu new menu menu item name.
	 */
	public static final String LOAD_MENU_NAME = "Load Menu";
	/**
	 * ACIDE - A Configurable IDE menu menu modify menu menu item name.
	 */
	public static final String MODIFY_MENU_NAME = "Modify Menu";
	/**
	 * ACIDE - A Configurable IDE menu menu save menu menu item name.
	 */
	public static final String SAVE_MENU_NAME = "Save Menu";
	/**
	 * ACIDE - A Configurable IDE menu menu save menu as menu item name.
	 */
	public static final String SAVE_MENU_AS_NAME = "Save Menu As";
	/**
	 * ACIDE - A Configurable IDE menu menu new menu menu item.
	 */
	private JMenuItem _newMenuMenuItem;
	/**
	 * ACIDE - A Configurable IDE menu menu load menu menu item.
	 */
	private JMenuItem _loadMenuMenuItem;
	/**
	 * ACIDE - A Configurable IDE menu menu modify menu menu item.
	 */
	private JMenuItem _modifyMenuMenuItem;
	/**
	 * ACIDE - A Configurable IDE menu menu save menu menu item.
	 */
	private JMenuItem _saveMenuMenuItem;
	/**
	 * ACIDE - A Configurable IDE menu menu save menu as menu item.
	 */
	private JMenuItem _saveMenuAsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE menu menu.
	 */
	public AcideMenuMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the menu menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE menu menu.
	 */
	private void addComponents() {

		// Adds the new menu menu item
		add(_newMenuMenuItem);

		// Adds the load menu menu item
		add(_loadMenuMenuItem);

		// Adds the modify menu menu item
		add(_modifyMenuMenuItem);

		// Adds the save menu menu item
		add(_saveMenuMenuItem);

		// Adds the save menu as menu item
		add(_saveMenuAsMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu menu components.
	 */
	private void buildComponents() {

		// Creates the new menu menu item
		_newMenuMenuItem = new JMenuItem();

		// Sets the new menu menu item name
		_newMenuMenuItem.setName(NEW_MENU_NAME);

		// Creates the load menu menu item
		_loadMenuMenuItem = new JMenuItem();

		// Sets the load menu menu item name
		_loadMenuMenuItem.setName(LOAD_MENU_NAME);

		// Creates the modify menu menu item
		_modifyMenuMenuItem = new JMenuItem();

		// Sets the modify menu menu item name
		_modifyMenuMenuItem.setName(MODIFY_MENU_NAME);

		// Creates the save menu menu item
		_saveMenuMenuItem = new JMenuItem();

		// Sets the save menu menu item name
		_saveMenuMenuItem.setName(SAVE_MENU_NAME);

		// Disables the save menu menu item
		_saveMenuMenuItem.setEnabled(false);

		// Creates the save menu as menu item
		_saveMenuAsMenuItem = new JMenuItem();

		// Sets the save menu as menu item name
		_saveMenuAsMenuItem.setName(SAVE_MENU_AS_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE menu menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the new menu menu item text
		_newMenuMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s275"));

		// Sets the load menu menu item text
		_loadMenuMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s276"));

		// Sets the modify menu menu item text
		_modifyMenuMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s277"));

		// Sets the save menu menu item text
		_saveMenuMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s278"));

		// Sets the save menu as menu item text
		_saveMenuAsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s279"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE menu menu components visibility
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the new menu menu item to visible or not visible
		_newMenuMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(NEW_MENU_NAME));

		// Sets the load menu menu item to visible or not visible
		_loadMenuMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(LOAD_MENU_NAME));

		// Sets the modify menu menu item to visible or not visible
		_modifyMenuMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(MODIFY_MENU_NAME));

		// Sets the save menu menu item to visible or not visible
		_saveMenuMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_MENU_NAME));

		// Sets the save menu as menu item to visible or not visible
		_saveMenuAsMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_MENU_AS_NAME));
	}

	/**
	 * Set the ACIDE - A Configurable IDE menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the new menu menu item action listener
		_newMenuMenuItem.addActionListener(new AcideNewMenuMenuItemListener());

		// Sets the load menu menu item action listener
		_loadMenuMenuItem
				.addActionListener(new AcideLoadMenuMenuItemListener());

		// Sets the modify menu menu item action listener
		_modifyMenuMenuItem
				.addActionListener(new AcideModifyMenuMenuItemListener());

		// Sets the save menu menu item action listener
		_saveMenuMenuItem
				.addActionListener(new AcideSaveMenuMenuItemListener());

		// Sets the save menu as menu item action listener
		_saveMenuAsMenuItem
				.addActionListener(new AcideSaveMenuAsMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu new menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu new menu menu item.
	 */
	public JMenuItem getNewMenuMenuItem() {
		return _newMenuMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu load menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu load menu menu item.
	 */
	public JMenuItem getLoadMenuMenuItem() {
		return _loadMenuMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu modify menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu modify menu menu item.
	 */
	public JMenuItem getModifyMenuMenuItem() {
		return _modifyMenuMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu save menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu save menu menu item.
	 */
	public JMenuItem getSaveMenuMenuItem() {
		return _saveMenuMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu save menu as menu item
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu save menu as menu item
	 */
	public JMenuItem getSaveMenuAsMenuItem() {
		return _saveMenuAsMenuItem;
	}
}
