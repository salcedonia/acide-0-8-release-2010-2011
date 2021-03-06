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
package acide.gui.menuBar.helpMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.helpMenu.listeners.AcideShowAboutUsMenuItemListener;
import acide.gui.menuBar.helpMenu.listeners.AcideShowHelpMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE help menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideHelpMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE help menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE help menu show help menu item name.
	 */
	public final static String SHOW_HELP_NAME = "Show Help";
	/**
	 * ACIDE - A Configurable IDE help menu show about us menu item name.
	 */
	public final static String SHOW_ABOUT_US_NAME = "Show About Us";
	/**
	 * ACIDE - A Configurable IDE help menu show help menu item image icon.
	 */
	private final static ImageIcon SHOW_HELP_IMAGE = new ImageIcon(
			"./resources/icons/menu/help/help.png");
	/**
	 * ACIDE - A Configurable IDE help menu show about us menu item image icon.
	 */
	private final static ImageIcon SHOW_ABOUT_US_IMAGE = new ImageIcon(
			"./resources/icons/menu/help/aboutUs.png");
	/**
	 * ACIDE - A Configurable IDE help menu show help menu item.
	 */
	private JMenuItem _showHelpMenuItem;
	/**
	 * ACIDE - A Configurable IDE help menu show about us menu item.
	 */
	private JMenuItem _showAboutUsMenuItem;
	/**
	 * ACIDE - A Configurable IDE help menu show help show about us separator.
	 */
	private JSeparator _showHelpShowAboutUsSeparator;

	/**
	 * Creates a new ACIDE - A Configurable IDE help menu.
	 */
	public AcideHelpMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the help menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE help menu.
	 */
	private void addComponents() {

		// Adds the show help menu item to the menu
		add(_showHelpMenuItem);

		// Adds the show help show about us separator to the menu
		add(_showHelpShowAboutUsSeparator);

		// Adds the show about us menu item to the menu
		add(_showAboutUsMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE help menu components.
	 */
	private void buildComponents() {

		// Creates the show help menu item
		_showHelpMenuItem = new JMenuItem(SHOW_HELP_IMAGE);

		// Sets the show help menu item name
		_showHelpMenuItem.setName(SHOW_HELP_NAME);

		// Creates the show help show about us separator
		_showHelpShowAboutUsSeparator = new JSeparator();

		// Creates the show about us menu item
		_showAboutUsMenuItem = new JMenuItem(SHOW_ABOUT_US_IMAGE);

		// Sets the show about us menu item name
		_showAboutUsMenuItem.setName(SHOW_ABOUT_US_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE help menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the show help menu item text
		_showHelpMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s38"));

		// Sets the show help menu item accelerator
		_showHelpMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));

		// Sets the show about us menu item text
		_showAboutUsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s39"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE help menu components visibility
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the show help menu item to visible or not visible
		_showHelpMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SHOW_HELP_NAME));

		// Sets the show help show about us to visible or not visible
		_showHelpShowAboutUsSeparator.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(SHOW_HELP_NAME)
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(
						SHOW_ABOUT_US_NAME));

		// Sets the show about us menu item to visible or not visible
		_showAboutUsMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SHOW_ABOUT_US_NAME));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE help menu item listeners.
	 */
	public void setListeners() {

		// Sets the show help menu item action listener
		_showAboutUsMenuItem
				.addActionListener(new AcideShowAboutUsMenuItemListener());

		// Sets the show about us menu item text
		_showHelpMenuItem
				.addActionListener(new AcideShowHelpMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE help menu show about us menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE help menu show about us menu item.
	 */
	public JMenuItem getShowAboutUsMenuItem() {
		return _showAboutUsMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE help menu show help menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE help menu show help menu item.
	 */
	public JMenuItem getShowHelpMenuItem() {
		return _showHelpMenuItem;
	}
}
