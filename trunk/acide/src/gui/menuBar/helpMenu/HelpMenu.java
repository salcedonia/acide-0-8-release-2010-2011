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
package gui.menuBar.helpMenu;

import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.helpMenu.listeners.ShowAboutUsMenuItemListener;
import gui.menuBar.helpMenu.listeners.ShowHelpMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.log.AcideLog;
import resources.AcideResourceManager;

import language.AcideLanguageManager;

/**																
 * ACIDE - A Configurable IDE help menu.											
 *					
 * @version 0.8	
 * @see JMenu																													
 */
public class HelpMenu extends JMenu {

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
	private JMenuItem _showHelp;
	/**
	 * ACIDE - A Configurable IDE help menu show about us menu item.
	 */
	private JMenuItem _showAboutUs;

	/**
	 * Creates a new ACIDE - A Configurable IDE help menu.
	 */
	public HelpMenu() {

		// MENU ITEM
		_showHelp = new JMenuItem(SHOW_HELP_IMAGE);
		_showAboutUs = new JMenuItem(SHOW_ABOUT_US_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE help menu language labels.
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

		// SHOW HELP
		_showHelp.setText(labels.getString("s38"));
		_showHelp.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));

		// SHOW ABOUT US
		_showAboutUs.setText(labels.getString("s39"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE help menu.
	 */
	public void buildMenu() {

		removeAll();

		// SHOW HELP
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_HELP_NAME))
			add(_showHelp);
		
		// SEPARATOR
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_HELP_NAME)
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_ABOUT_US_NAME))
			addSeparator();
		
		// SHOW ABOUT US
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SHOW_ABOUT_US_NAME))
			add(_showAboutUs);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE help menu item listeners.
	 */
	public void setListeners() {

		// SHOW ABOUT US
		_showAboutUs.addActionListener(new ShowAboutUsMenuItemListener());

		// SHOW HELP
		_showHelp.addActionListener(new ShowHelpMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE help menu show about us menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE help menu show about us menu item.
	 */
	public JMenuItem getShowAboutUs() {
		return _showAboutUs;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE help menu show about us menu item.
	 * 
	 * @param showAboutUs
	 *            new value to set.
	 */
	public void setShowAboutUs(JMenuItem showAboutUs) {
		_showAboutUs = showAboutUs;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE help menu show help menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE help menu show help menu item.
	 */
	public JMenuItem getShowHelp() {
		return _showHelp;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE help menu show help menu item.
	 * 
	 * @param showHelp
	 *            new value to set.
	 */
	public void setShowHelp(JMenuItem showHelp) {
		_showHelp = showHelp;
	}
}
