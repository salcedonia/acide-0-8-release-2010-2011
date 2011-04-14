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
package acide.gui.menuBar.configurationMenu.consoleMenu;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideConfigureMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideConsoleDisplayOptionsMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideExternalCommandMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE console menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideConsoleMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE console menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item name.
	 */
	public static final String CONFIGURE_NAME = "Configure";
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item name.
	 */
	public static final String EXTERNAL_COMMAND_NAME = "External Command";
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu item
	 * name.
	 */
	public static final String CONSOLE_DISPLAY_OPTIONS_NAME = "Console Display Options";
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item image icon.
	 */
	private final static ImageIcon CONFIGURE_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/configure.png");
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item image
	 * icon.
	 */
	private final static ImageIcon EXTERNAL_COMMAND_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/externalCommand.png");
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu item
	 * image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/consoleDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item.
	 */
	private JMenuItem _configureMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item.
	 */
	private JMenuItem _externalCommandMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu
	 * item.
	 */
	private JMenuItem _consoleDisplayOptionsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE console menu.
	 */
	public AcideConsoleMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the console menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE console menu.
	 */
	private void addComponents() {

		// Adds the configure menu item
		add(_configureMenuItem);

		// Adds the external command menu item
		add(_externalCommandMenuItem);

		// Adds the console display options menu item
		add(_consoleDisplayOptionsMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE components.
	 */
	private void buildComponents() {

		// Creates the configure menu item
		_configureMenuItem = new JMenuItem(CONFIGURE_IMAGE);

		// Sets the configure menu item name
		_configureMenuItem.setName(CONFIGURE_NAME);

		// Creates the external command menu item
		_externalCommandMenuItem = new JMenuItem(EXTERNAL_COMMAND_IMAGE);

		// Sets the external command menu item name
		_externalCommandMenuItem.setName(EXTERNAL_COMMAND_NAME);

		// Creates the console display options menu item
		_consoleDisplayOptionsMenuItem = new JMenuItem(
				CONSOLE_DISPLAY_OPTIONS_IMAGE);

		// Sets the console display options menu item
		_consoleDisplayOptionsMenuItem.setName(CONSOLE_DISPLAY_OPTIONS_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE console menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the configure menu item text
		_configureMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s333"));

		// Sets the external command menu item text
		_externalCommandMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s341"));

		// Sets the console display options menu item text
		_consoleDisplayOptionsMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s977"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE console menu components visibility
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the configure menu item to visible or not visible
		_configureMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CONFIGURE_NAME));

		// Sets the external command menu item to visible or not visible
		_externalCommandMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(EXTERNAL_COMMAND_NAME));

		// Sets the console display options menu item to visible or not visible
		_consoleDisplayOptionsMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(CONSOLE_DISPLAY_OPTIONS_NAME));
	}

	/**
	 * Sets ACIDE - A Configurable IDE console menu item listeners.
	 */
	public void setListeners() {

		// Sets the configure menu item action listener
		_configureMenuItem
				.addActionListener(new AcideConfigureMenuItemListener());

		// Sets the external command menu item action listener
		_externalCommandMenuItem
				.addActionListener(new AcideExternalCommandMenuItemListener());

		// Sets the console display options menu item action listener
		_consoleDisplayOptionsMenuItem
				.addActionListener(new AcideConsoleDisplayOptionsMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu external command menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu external command
	 *         menu. item
	 */
	public JMenuItem getExternalCommandMenuItem() {
		return _externalCommandMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu configure menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu configure menu item.
	 */
	public JMenuItem getConfigureMenuItem() {
		return _configureMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu console display
	 * options menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu console display
	 *         options menu item.
	 */
	public JMenuItem getConsoleDisplayOptionsMenuItem() {
		return _consoleDisplayOptionsMenuItem;
	}
}