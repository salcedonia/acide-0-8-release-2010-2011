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
package gui.menuBar.viewMenu;

import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.viewMenu.listeners.ShowAcideExplorerPanelMenuItemListener;
import gui.menuBar.viewMenu.listeners.ShowAcideLogTabMenuItemListener;
import gui.menuBar.viewMenu.listeners.ShowAcideConsolePanelMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.log.AcideLog;

import language.AcideLanguageManager;

import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE view menu.
 *					
 * @version 0.8																														
 */
public class ViewMenu extends JMenu {

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
	private final static ImageIcon SHOW_LOG_TAB_IMAGE = new ImageIcon("./resources/icons/menu/view/showLogTab.png");
	/**
	 * ACIDE - A Configurable IDE view menu show explorer panel check box menu item image icon.
	 */
	private final static ImageIcon SHOW_EXPLORER_PANEL_IMAGE = new ImageIcon("./resources/icons/menu/view/showExplorerPanel.png");
	/**
	 * ACIDE - A Configurable IDE view menu show console panel check box menu item image icon.
	 */
	private final static ImageIcon SHOW_CONSOLE_PANEL_IMAGE = new ImageIcon("./resources/icons/menu/view/showConsolePanel.png");
	/**
	 * ACIDE - A Configurable IDE view menu show log tab menu item.
	 */
	private JMenuItem _showLogTab;
	/**
	 * ACIDE - A Configurable IDE view menu show explorer panel check box menu item.
	 */
	private JCheckBoxMenuItem _showExplorerPanel;
	/**
	 * ACIDE - A Configurable IDE view menu show console panel check box menu item.
	 */
	private JCheckBoxMenuItem _showConsolePanel;
	/**
	 * ACIDE - A Configurable IDE view menu console size.
	 */
	private int _consoleSize;

	/**
	 * Creates a new ACIDE - A Configurable IDE view menu.
	 */
	public ViewMenu(){
		
		// MENU ITEM
		_showLogTab = new JMenuItem(SHOW_LOG_TAB_IMAGE);
		_showExplorerPanel = new JCheckBoxMenuItem(SHOW_EXPLORER_PANEL_IMAGE);
		_showExplorerPanel.setSelected(true);		
		_showConsolePanel = new JCheckBoxMenuItem(SHOW_CONSOLE_PANEL_IMAGE);
		_showConsolePanel.setSelected(true);

		setLanguageLabels();
	}

	/**
	 * Sets the labels to display in the selected language.
	 */
	public void setLanguageLabels() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// SHOW LOG TAB
		_showLogTab.setText(labels.getString("s28"));
		_showLogTab.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// SHOW EXPLORER PANEL
		_showExplorerPanel.setText(labels.getString("s221"));
		
		// SHOW OUTPUT PANEL
		_showConsolePanel.setText(labels.getString("s223"));
	}
	
	/**
	 * Builds the ACIDE - A Configurable IDE view menu.
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_LOG_TAB_NAME))
			add(_showLogTab);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_EXPLORER_PANEL_NAME))
			add(_showExplorerPanel);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_CONSOLE_PANEL_NAME))
			add(_showConsolePanel);
	}
	
	/**
	 * Sets the ACIDE - A Configurable IDE view menu menu item listeners.
	 */
	public void setListeners(){
		
		// SHOW LOG 
		_showLogTab.addActionListener(new ShowAcideLogTabMenuItemListener());
		
		// SHOW BROWSER
		_showExplorerPanel.addActionListener(new ShowAcideExplorerPanelMenuItemListener());
		
		// SHOW SHELL WINDOWS
		_showConsolePanel.addActionListener(new ShowAcideConsolePanelMenuItemListener());
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE view menu show log tab menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu show log tab menu item.
	 */
	public JMenuItem getShowLogTab() {
		return _showLogTab;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE view menu show log tab menu item.
	 * 
	 * @param showLogTab new value to set.
	 */
	public void setShowLogTab(JMenuItem showLogTab) {
		_showLogTab = showLogTab;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE view menu show explorer panel check box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu show explorer panel check box menu item.
	 */
	public JCheckBoxMenuItem getShowExplorerPanel() {
		return _showExplorerPanel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE view menu show explorer panel check box menu item.
	 * 
	 * @param showExplorerPanel new value to set.
	 */
	public void setShowExplorerPanel(JCheckBoxMenuItem showExplorerPanel) {
		_showExplorerPanel = showExplorerPanel;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE view menu show output panel check box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu show output panel check box menu item.
	 */
	public JCheckBoxMenuItem getShowConsolePanel() {
		return _showConsolePanel;
	}

	/**
	 * Sets the ACIDE - A Configurable IDE view menu show console panel check box menu item.
	 * 
	 * @param showConsolePanel new value to set.
	 */
	public void setShowConsolePanel(JCheckBoxMenuItem showConsolePanel) {
		_showConsolePanel = showConsolePanel;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE view menu console size.
	 * 
	 * @return the ACIDE - A Configurable IDE view menu console size.
	 */
	public int getConsoleSize(){
		return _consoleSize;
	}
	
	/**
	 * Sets a new value to the ACIDE - A Configurable IDE view menu console size.
	 * 
	 * @param consoleSize new value to set.
	 */
	public void setConsoleSize(int consoleSize){
		_consoleSize = consoleSize;
	}
}