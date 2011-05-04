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
package acide.gui.statusBarPanel.popup;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import acide.gui.statusBarPanel.popup.listeners.AcideCopyMenuItemAction;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE status bar popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 */
public class AcideStatusBarPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE status bar popup menu class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE status bar popup menu copy menu item.
	 */
	private JMenuItem _copyMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE status bar popup menu.
	 */
	public AcideStatusBarPopupMenu() {

		// Builds the components
		buildComponents();

		// Adds the components to the popup menu
		addComponents();

		// Sets the listeners for the popup menu components
		setListeners();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE status bar popup
	 * menu.
	 */
	private void addComponents() {

		// Adds the copy menu item to the popup menu
		add(_copyMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar popup menu components.
	 */
	private void buildComponents() {

		// Creates the copy menu item
		_copyMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s187"));
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE status bar popup
	 * menu components.
	 */
	private void setListeners() {

		// Sets the copy menu item action listener
		_copyMenuItem.addActionListener(new AcideCopyMenuItemAction());
	}
}
