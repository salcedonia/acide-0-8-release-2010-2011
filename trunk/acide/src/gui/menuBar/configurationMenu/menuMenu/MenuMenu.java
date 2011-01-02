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
package gui.menuBar.configurationMenu.menuMenu;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.configurationMenu.menuMenu.listeners.LoadMenuMenuItemListener;
import gui.menuBar.configurationMenu.menuMenu.listeners.ModifyMenuMenuItemListener;
import gui.menuBar.configurationMenu.menuMenu.listeners.NewMenuMenuItemListener;
import gui.menuBar.configurationMenu.menuMenu.listeners.SaveAsMenuMenuItemListener;
import gui.menuBar.configurationMenu.menuMenu.listeners.SaveMenuMenuItemListener;

/**
 * ACIDE - A Configurable IDE menu menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class MenuMenu extends JMenu {

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
	private JMenuItem _newMenu;
	/**
	 * ACIDE - A Configurable IDE menu menu load menu menu item.
	 */
	private JMenuItem _loadMenu;
	/**
	 * ACIDE - A Configurable IDE menu menu modify menu menu item.
	 */
	private JMenuItem _modifyMenu;
	/**
	 * ACIDE - A Configurable IDE menu menu save menu menu item.
	 */
	private JMenuItem _saveMenu;
	/**
	 * ACIDE - A Configurable IDE menu menu save as menu menu item.
	 */
	private JMenuItem _saveAsMenu;

	/**
	 * Creates a new ACIDE - A Configurable IDE menu menu.
	 */
	public MenuMenu() {

		// MENU ITEM
		_newMenu = new JMenuItem();
		_loadMenu = new JMenuItem();
		_modifyMenu = new JMenuItem();
		_saveMenu = new JMenuItem();
		_saveMenu.setEnabled(false);
		_saveAsMenu = new JMenuItem();

		setLanguageLabels();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE menu menu language labels.
	 */
	public void setLanguageLabels() {

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

		// NEW
		_newMenu.setText(labels.getString("s275"));

		// LOAD
		_loadMenu.setText(labels.getString("s276"));

		// MODIFY
		_modifyMenu.setText(labels.getString("s277"));

		// SAVE
		_saveMenu.setText(labels.getString("s278"));

		// SAVE AS
		_saveAsMenu.setText(labels.getString("s279"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu menu.
	 */
	public void buildMenu() {

		removeAll();

		// NEW MENU MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_MENU_NAME))
			add(_newMenu);

		// LOAD MENU MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_MENU_NAME))
			add(_loadMenu);

		// MODIFY MENU MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_MENU_NAME))
			add(_modifyMenu);

		// SAVE MENU MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_MENU_NAME))
			add(_saveMenu);

		// SAVE AS MENU MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_MENU_AS_NAME))
			add(_saveAsMenu);
	}

	/**
	 * Set the menu menu item listeners.
	 */
	public void setListeners() {

		// NEW MENU
		_newMenu.addActionListener(new NewMenuMenuItemListener());

		// LOAD MENU
		_loadMenu.addActionListener(new LoadMenuMenuItemListener());

		// MODIFY MENU
		_modifyMenu.addActionListener(new ModifyMenuMenuItemListener());

		// SAVE MENU
		_saveMenu.addActionListener(new SaveMenuMenuItemListener());

		// SAVE AS MENU
		_saveAsMenu.addActionListener(new SaveAsMenuMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu load menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu load menu menu item.
	 */
	public JMenuItem getLoadMenu() {
		return _loadMenu;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu menu load menu
	 * menu item.
	 * 
	 * @param loadMenu
	 *            new value to set.
	 */
	public void setLoadMenu(JMenuItem loadMenu) {
		_loadMenu = loadMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu modify menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu modify menu menu item.
	 */
	public JMenuItem getModifyMenu() {
		return _modifyMenu;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu menu modify menu
	 * menu item.
	 * 
	 * @param modifyMenu
	 *            new value to set.
	 */
	public void setModifyMenu(JMenuItem modifyMenu) {
		_modifyMenu = modifyMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu new menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu new menu menu item.
	 */
	public JMenuItem getNewMenu() {
		return _newMenu;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu menu new menu
	 * menu item.
	 * 
	 * @param newMenu
	 *            new value to set.
	 */
	public void setNewMenu(JMenuItem newMenu) {
		_newMenu = newMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu save as menu menu item
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu save as menu menu item
	 */
	public JMenuItem getSaveAsMenu() {
		return _saveAsMenu;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu menu save as menu
	 * menu item.
	 * 
	 * @param saveAsMenu
	 *            new value to set.
	 */
	public void setSaveAsMenu(JMenuItem saveAsMenu) {
		_saveAsMenu = saveAsMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu menu save menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE menu menu save menu menu item.
	 */
	public JMenuItem getSaveMenu() {
		return _saveMenu;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu menu save menu
	 * menu item.
	 * 
	 * @param saveMenu
	 *            new value to set.
	 */
	public void setSaveMenu(JMenuItem saveMenu) {
		_saveMenu = saveMenu;
	}
}
