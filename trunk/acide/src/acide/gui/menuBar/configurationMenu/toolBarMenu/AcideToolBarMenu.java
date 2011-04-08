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
package acide.gui.menuBar.configurationMenu.toolBarMenu;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.toolBarMenu.listeners.AcideLoadToolBarMenuItemListener;
import acide.gui.menuBar.configurationMenu.toolBarMenu.listeners.AcideModifyToolBarMenuItemListener;
import acide.gui.menuBar.configurationMenu.toolBarMenu.listeners.AcideNewToolBarMenuItemListener;
import acide.gui.menuBar.configurationMenu.toolBarMenu.listeners.AcideSaveAsToolBaMenuItemrListener;
import acide.gui.menuBar.configurationMenu.toolBarMenu.listeners.AcideSaveToolBarMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE tool bar menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideToolBarMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE tool bar menu tool bar menu class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar menu new tool bar menu item name.
	 */
	public static final String NEW_TOOLBAR_NAME = "New Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu load tool bar menu item name.
	 */
	public static final String LOAD_TOOLBAR_NAME = "Load Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu modify tool bar menu item name.
	 */
	public static final String MODIFY_TOOLBAR_NAME = "Modify Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar menu item name.
	 */
	public static final String SAVE_TOOLBAR_NAME = "Save Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar as menu item name.
	 */
	public static final String SAVE_TOOLBAR_AS_NAME = "Save Toolbar As";
	/**
	 * ACIDE - A Configurable IDE tool bar menu new tool bar menu item.
	 */
	private JMenuItem _newToolBarMenuItem;
	/**
	 * ACIDE - A Configurable IDE tool bar menu load tool bar menu item.
	 */
	private JMenuItem _loadToolBarMenuItem;
	/**
	 * ACIDE - A Configurable IDE tool bar menu modify tool bar menu item.
	 */
	private JMenuItem _modifyToolBarMenuItem;
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar menu item.
	 */
	private JMenuItem _saveToolBarMenuItem;
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar as menu item.
	 */
	private JMenuItem _saveToolBarAsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar menu.
	 */
	public AcideToolBarMenu() {

		// Creates the new tool bar menu item
		_newToolBarMenuItem = new JMenuItem();

		// Creates the load tool bar menu item
		_loadToolBarMenuItem = new JMenuItem();

		// Creates the modify tool bar menu item
		_modifyToolBarMenuItem = new JMenuItem();

		// Creates the save tool bar menu item
		_saveToolBarMenuItem = new JMenuItem();
		
		// Disables the save tool bar menu item
		_saveToolBarMenuItem.setEnabled(false);

		// Creates the save tool bar as menu item
		_saveToolBarAsMenuItem = new JMenuItem();

		// Sets the text of the tool bar menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE tool bar menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the new tool bar menu item text
		_newToolBarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s280"));

		// Sets the load tool bar menu item text
		_loadToolBarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s281"));

		// Sets the modify tool bar menu item text
		_modifyToolBarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s282"));

		// Sets the save tool bar menu item text
		_saveToolBarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s283"));

		// Sets the save tool bar as menu item text
		_saveToolBarAsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s284"));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE tool bar menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the new tool bar menu item action listener
		_newToolBarMenuItem
				.addActionListener(new AcideNewToolBarMenuItemListener());

		// Sets the load tool bar menu item action listener
		_loadToolBarMenuItem
				.addActionListener(new AcideLoadToolBarMenuItemListener());

		// Sets the modify tool bar menu item action listener
		_modifyToolBarMenuItem
				.addActionListener(new AcideModifyToolBarMenuItemListener());

		// Sets the save tool bar menu item action listener
		_saveToolBarMenuItem
				.addActionListener(new AcideSaveToolBarMenuItemListener());

		// Sets the save tool bar as menu item action listener
		_saveToolBarAsMenuItem
				.addActionListener(new AcideSaveAsToolBaMenuItemrListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE tool bar menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// Adds the new tool bar menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				NEW_TOOLBAR_NAME))
			add(_newToolBarMenuItem);

		// Adds the load tool bar menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				LOAD_TOOLBAR_NAME))
			add(_loadToolBarMenuItem);

		// Adds the modify tool bar menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				MODIFY_TOOLBAR_NAME))
			add(_modifyToolBarMenuItem);

		// Adds the save tool bar menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_TOOLBAR_NAME))
			add(_saveToolBarMenuItem);

		// Adds the save tool bar as menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_TOOLBAR_AS_NAME))
			add(_saveToolBarAsMenuItem);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu new tool bar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu new tool bar menu
	 *         item.
	 */
	public JMenuItem getNewToolBarMenuItem() {
		return _newToolBarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu load tool bar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu load tool bar menu
	 *         item.
	 */
	public JMenuItem getLoadToolBarMenuItem() {
		return _loadToolBarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu modify tool bar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu modify tool bar menu
	 *         item.
	 */
	public JMenuItem getModifyToolBarMenuItem() {
		return _modifyToolBarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu save tool bar menu
	 * item
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu save tool bar menu
	 *         item
	 */
	public JMenuItem getSaveToolBarMenuItem() {
		return _saveToolBarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu save tool bar as
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu save tool bar as
	 *         menu item.
	 */
	public JMenuItem getSaveToolBarAsMenuItem() {
		return _saveToolBarAsMenuItem;
	}
}