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
package acide.gui.menuBar.configurationMenu.fileEditor;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.fileEditor.listeners.AcideFileEditorDisplayOptionsMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideFileEditorMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE file editor menu serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor menu file editor display options
	 * menu item name.
	 */
	public static final String FILE_EDITOR_DISPLAY_OPTIONS_NAME = "File Editor Display Options";
	/**
	 * ACIDE - A Configurable IDE file editor menu file editor display options
	 * menu item image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/fileEditorDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu file editor display options
	 * menu item.
	 */
	private JMenuItem _fileEditorDisplayOptionsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor menu.
	 */
	public AcideFileEditorMenu() {

		// Creates the file editor display options menu item
		_fileEditorDisplayOptionsMenuItem = new JMenuItem(
				CONSOLE_DISPLAY_OPTIONS_IMAGE);

		// Sets the text of the file editor menu components
		setTextOfMenuComponets();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE file editor menu
	 * components with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponets() {

		// Sets the file editor display options menu item text
		_fileEditorDisplayOptionsMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s1041"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE file editor menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// Adds the file editor display options menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				FILE_EDITOR_DISPLAY_OPTIONS_NAME))
			add(_fileEditorDisplayOptionsMenuItem);
	}

	/**
	 * Sets ACIDE - A Configurable IDE file editor menu item listeners.
	 */
	public void setListeners() {

		// Sets the file editor display options menu item action listener
		_fileEditorDisplayOptionsMenuItem
				.addActionListener(new AcideFileEditorDisplayOptionsMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor menu file editor
	 * display options menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor menu file editor
	 *         display options menu item.
	 */
	public JMenuItem getFileEditorDisplayOptionsMenuItem() {
		return _fileEditorDisplayOptionsMenuItem;
	}
}
