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

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.language.AcideLanguageManager;

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
	 * Creates a new ACIDE - A Configurable IDE view menu.
	 */
	public AcideViewMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the view menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE view menu.
	 */
	private void addComponents() {

		// Adds the show log tab menu item to the menu
		add(_showLogTabMenuItem);

		// Adds the show explorer panel check box menu item to the menu
		add(_showExplorerPanelCheckBoxMenuItem);

		// Adds the show console panel check box menu item to the menu
		add(_showConsolePanelCheckBoxMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE view menu components.
	 */
	private void buildComponents() {

		// Creates the show log tab menu item
		_showLogTabMenuItem = new JMenuItem(SHOW_LOG_TAB_IMAGE);

		// Sets the show log tab menu item name
		_showLogTabMenuItem.setName(SHOW_LOG_TAB_NAME);

		// Creates the show explorer panel check box menu item
		_showExplorerPanelCheckBoxMenuItem = new JCheckBoxMenuItem(
				SHOW_EXPLORER_PANEL_IMAGE);

		// Sets the show explorer panel check box menu item name
		_showExplorerPanelCheckBoxMenuItem.setName(SHOW_EXPLORER_PANEL_NAME);

		// Sets the show explorer panel check box menu item as not selected
		_showExplorerPanelCheckBoxMenuItem.setSelected(true);

		// Creates the show console panel check box menu item
		_showConsolePanelCheckBoxMenuItem = new JCheckBoxMenuItem(
				SHOW_CONSOLE_PANEL_IMAGE);

		// Sets the show console panel check box menu item name
		_showConsolePanelCheckBoxMenuItem.setName(SHOW_CONSOLE_PANEL_NAME);

		// Sets the show console panel check box menu item as not selected
		_showConsolePanelCheckBoxMenuItem.setSelected(true);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE view menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the show log tab menu item text
		_showLogTabMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s28"));

		// Sets the show log tab menu item accelerator
		_showLogTabMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_G, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the show explorer panel check box menu item text
		_showExplorerPanelCheckBoxMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s221"));

		// Sets the show console panel check box menu item text
		_showConsolePanelCheckBoxMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s223"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE view menu components visibiliy
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the show log tab menu item to visible or not visible
		_showLogTabMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SHOW_LOG_TAB_NAME));

		// Sets the show explorer panel check box menu item as visible or not
		// visible
		_showExplorerPanelCheckBoxMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(SHOW_EXPLORER_PANEL_NAME));

		// Sets the console panel check box menu item as visible or not visible
		_showConsolePanelCheckBoxMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(SHOW_CONSOLE_PANEL_NAME));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE view menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the show log tab menu item action listener
		_showLogTabMenuItem
				.addActionListener(new AcideShowAcideLogTabMenuItemListener());

		// Sets the show explorer panel check box menu item action listener
		_showExplorerPanelCheckBoxMenuItem
				.addActionListener(new AcideShowAcideExplorerPanelMenuItemListener());

		// Sets the show console panel check box menu item action listener
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
}