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
package acide.gui.menuBar.viewMenu;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.viewMenu.listeners.AcideShowAcideExplorerPanelMenuItemListener;
import acide.gui.menuBar.viewMenu.listeners.AcideShowAcideLogTabMenuItemListener;
import acide.gui.menuBar.viewMenu.listeners.AcideShowAcideConsolePanelMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE view menu.
 * 
 * @version 0.8
 */
public class AcideViewMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE view menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE view menu show log tab menu item name.
	 */
	public static final String SHOW_LOG_TAB_NAME = "Show Log Tab";
	/**
	 * ACIDE - A Configurable IDE view menu show explorer panel menu item name.
	 */
	public static final String SHOW_EXPLORER_PANEL_NAME = "Show Explorer Panel";
	/**
	 * ACIDE - A Configurable IDE view menu show console panel menu item name.
	 */
	public static final String SHOW_CONSOLE_PANEL_NAME = "Show Console Panel";
	/**
	 * ACIDE - A Configurable IDE view menu show log tab menu item icon.
	 */
	private final static ImageIcon SHOW_LOG_TAB_IMAGE = new ImageIcon(
			"./resources/icons/menu/view/showLogTab.png");
	/**
	 * ACIDE - A Configurable IDE view menu show explorer panel check box menu
	 * item image icon.
	 */
	private final static ImageIcon SHOW_EXPLORER_PANEL_IMAGE = new ImageIcon(
			"./resources/icons/menu/view/showExplorerPanel.png");
	/**
	 * ACIDE - A Configurable IDE view menu show console panel check box menu
	 * item image icon.
	 */
	private final static ImageIcon SHOW_CONSOLE_PANEL_IMAGE = new ImageIcon(
			"./resources/icons/menu/view/showConsolePanel.png");
	/**
	 * ACIDE - A Configurable IDE view menu show log tab menu item.
	 */
	private JMenuItem _showLogTabMenuItem;
	/**
	 * ACIDE - A Configurable IDE view menu show explorer panel check box menu
	 * item.
	 */
	private JCheckBoxMenuItem _showExplorerPanelCheckBoxMenuItem;
	/**
	 * ACIDE - A Configurable IDE view menu show console panel check box menu
	 * item.
	 */
	private JCheckBoxMenuItem _showConsolePanelCheckBoxMenuItem;
	/**
	 * ACIDE - A Configurable IDE view menu console size.
	 */
	private int _consoleSize;

	/**
	 * Creates a new ACIDE - A Configurable IDE view menu.
	 */
	public AcideViewMenu() {

		// SHOW LOG TAB MENU ITEM
		_showLogTabMenuItem = new JMenuItem(SHOW_LOG_TAB_IMAGE);

		// SHOW EXPLORER PANEL CHECK BOX MENU ITEM
		_showExplorerPanelCheckBoxMenuItem = new JCheckBoxMenuItem(
				SHOW_EXPLORER_PANEL_IMAGE);
		_showExplorerPanelCheckBoxMenuItem.setSelected(true);

		// SHOW CONSOLE PANEL CHECK BOX MENU ITEM
		_showConsolePanelCheckBoxMenuItem = new JCheckBoxMenuItem(
				SHOW_CONSOLE_PANEL_IMAGE);
		_showConsolePanelCheckBoxMenuItem.setSelected(true);

		// Sets the text of the view menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE view menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// SHOW LOG TAB MENU ITEM
		_showLogTabMenuItem.setText(labels.getString("s28"));
		_showLogTabMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_G, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// SHOW EXPLORER PANEL CHECK BOX MENU ITEM
		_showExplorerPanelCheckBoxMenuItem.setText(labels.getString("s221"));

		// SHOW CONSOLE PANEL CHECK BOX MENU ITEM
		_showConsolePanelCheckBoxMenuItem.setText(labels.getString("s223"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE view menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// SHOW LOG TAB MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SHOW_LOG_TAB_NAME))
			add(_showLogTabMenuItem);

		// SHOW EXPLORER PANEL CHECK BOX MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SHOW_EXPLORER_PANEL_NAME))
			add(_showExplorerPanelCheckBoxMenuItem);

		// SHOW CONSOLE PANEL CHECK BOX MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SHOW_CONSOLE_PANEL_NAME))
			add(_showConsolePanelCheckBoxMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE view menu menu item listeners.
	 */
	public void setListeners() {

		// SHOW LOG TAB MENU ITEM
		_showLogTabMenuItem
				.addActionListener(new AcideShowAcideLogTabMenuItemListener());

		// SHOW EXPLORER PANEL CHECK BOX MENU ITEM
		_showExplorerPanelCheckBoxMenuItem
				.addActionListener(new AcideShowAcideExplorerPanelMenuItemListener());

		// SHOW CONSOLE PANEL CHECK BOX MENU ITEM
		_showConsolePanelCheckBoxMenuItem
				.addActionListener(new AcideShowAcideConsolePanelMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE view menu show log tab menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu show log tab menu item.
	 */
	public JMenuItem getShowLogTabMenuItem() {
		return _showLogTabMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE view menu show explorer panel
	 * check box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu show explorer panel
	 *         check box menu item.
	 */
	public JCheckBoxMenuItem getShowExplorerPanelCheckBoxMenuItem() {
		return _showExplorerPanelCheckBoxMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE view menu show output panel check
	 * box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu show output panel check
	 *         box menu item.
	 */
	public JCheckBoxMenuItem getShowConsolePanelCheckBoxMenuItem() {
		return _showConsolePanelCheckBoxMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE view menu console size.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu console size.
	 */
	public int getConsoleSize() {
		return _consoleSize;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE view menu console
	 * size.
	 * 
	 * @param consoleSize
	 *            new value to set.
	 */
	public void setConsoleSize(int consoleSize) {
		_consoleSize = consoleSize;
	}
}