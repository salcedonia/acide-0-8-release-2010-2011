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
package acide.gui.menuBar.configurationMenu.menuMenu;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideLoadMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideModifyMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideNewMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideSaveAsMenuMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.listeners.AcideSaveMenuMenuItemListener;

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

		// NEW MENU MENU ITEM
		_newMenuMenuItem = new JMenuItem();
		
		// LOAD MENU MENU ITEM
		_loadMenuMenuItem = new JMenuItem();
		
		// MODIFY MENU MENU ITEM
		_modifyMenuMenuItem = new JMenuItem();
		
		// SAVE MENU MENU ITEM
		_saveMenuMenuItem = new JMenuItem();
		_saveMenuMenuItem.setEnabled(false);
		
		// SAVE MENU AS MENU ITEM
		_saveMenuAsMenuItem = new JMenuItem();

		// Sets the text of the menu menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE menu menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

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
		final ResourceBundle labels = language.getLabels();

		// NEW MENU MENU ITEM
		_newMenuMenuItem.setText(labels.getString("s275"));

		// LOAD MENU MENU ITEM
		_loadMenuMenuItem.setText(labels.getString("s276"));

		// MODIFY MENU MENU ITEM
		_modifyMenuMenuItem.setText(labels.getString("s277"));

		// SAVE MENU MENU ITEM
		_saveMenuMenuItem.setText(labels.getString("s278"));

		// SAVE MENU AS MENU ITEM
		_saveMenuAsMenuItem.setText(labels.getString("s279"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// NEW MENU MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_MENU_NAME))
			add(_newMenuMenuItem);

		// LOAD MENU MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_MENU_NAME))
			add(_loadMenuMenuItem);

		// MODIFY MENU MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_MENU_NAME))
			add(_modifyMenuMenuItem);

		// SAVE MENU MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_MENU_NAME))
			add(_saveMenuMenuItem);

		// SAVE MENU AS MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_MENU_AS_NAME))
			add(_saveMenuAsMenuItem);
	}

	/**
	 * Set the menu menu item listeners.
	 */
	public void setListeners() {

		// NEW MENU MENU ITEM
		_newMenuMenuItem.addActionListener(new AcideNewMenuMenuItemListener());

		// LOAD MENU MENU ITEM
		_loadMenuMenuItem.addActionListener(new AcideLoadMenuMenuItemListener());

		// MODIFY MENU MENU ITEM
		_modifyMenuMenuItem.addActionListener(new AcideModifyMenuMenuItemListener());

		// SAVE MENU MENU ITEM
		_saveMenuMenuItem.addActionListener(new AcideSaveMenuMenuItemListener());

		// SAVE MENU AS MENU ITEM
		_saveMenuAsMenuItem.addActionListener(new AcideSaveAsMenuMenuItemListener());
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
