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
package acide.gui.menuBar.helpMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
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
	private final static ImageIcon SHOW_HELP_IMAGE = new ImageIcon("./resources/icons/menu/help/help.png");
	/**
	 * ACIDE - A Configurable IDE help menu show about us menu item image icon.
	 */
	private final static ImageIcon SHOW_ABOUT_US_IMAGE = new ImageIcon("./resources/icons/menu/help/aboutUs.png");
	/**
	 * ACIDE - A Configurable IDE help menu show help menu item.
	 */
	private JMenuItem _showHelpMenuItem;
	/**
	 * ACIDE - A Configurable IDE help menu show about us menu item.
	 */
	private JMenuItem _showAboutUsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE help menu.
	 */
	public AcideHelpMenu() {

		// SHOW HELP MENU ITEM
		_showHelpMenuItem = new JMenuItem(SHOW_HELP_IMAGE);
		
		// SHOW ABOUT US MENU ITEM
		_showAboutUsMenuItem = new JMenuItem(SHOW_ABOUT_US_IMAGE);

		// Sets the text of the help menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE help menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// SHOW HELP MENU ITEM
		_showHelpMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s38"));
		_showHelpMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));

		// SHOW ABOUT US MENU ITEM
		_showAboutUsMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s39"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE help menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// SHOW HELP MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_HELP_NAME))
			add(_showHelpMenuItem);
		
		// SEPARATOR
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_HELP_NAME)
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_ABOUT_US_NAME))
			addSeparator();
		
		// SHOW ABOUT US MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_ABOUT_US_NAME))
			add(_showAboutUsMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE help menu item listeners.
	 */
	public void setListeners() {

		// SHOW ABOUT US MENU ITEM
		_showAboutUsMenuItem.addActionListener(new AcideShowAboutUsMenuItemListener());

		// SHOW HELP MENU ITEM
		_showHelpMenuItem.addActionListener(new AcideShowHelpMenuItemListener());
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
