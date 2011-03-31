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
package acide.gui.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * <p>
 * ACIDE - A Configurable IDE menu bar keyboard listener.
 * </p>
 * <p>
 * Enables the menu items which have a accelerator so it can be invoked because
 * of it.
 * </p>
 * <p>
 * <b>IMPORTANT:</b> If the component does not implement this class, then the
 * menu items accelerators will not work.
 * </p>
 * 
 * @version 0.8
 * @see KeyAdapter
 */
public class AcideMenuBarKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		// Enables the file menu
		enableFileMenu();

		// Enables the edit menu
		enableEditMenu();

		// Enables the project menu
		enableProjectMenu();

		// Enables the view menu
		enableViewMenu();

		// Enables the configuration menu
		enableConfigurationMenu();

		// Enables the help menu
		enableHelpMenu();
	}

	/**
	 * Enables the menu items in the help menu.
	 */
	private void enableHelpMenu() {

		// Enables the help menu item
		AcideMainWindow.getInstance().getMenu().getHelpMenu()
				.getShowHelpMenuItem().setEnabled(true);
	}

	/**
	 * Enables the menu items in the configuration menu.
	 */
	private void enableConfigurationMenu() {

		// Enables the Spanish menu item
		AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getLanguageMenu().getSpanishMenuItem().setEnabled(true);

		// Enables the English menu item
		AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getLanguageMenu().getEnglishMenuItem().setEnabled(true);
	}

	/**
	 * Enables the menu items in the view menu.
	 */
	private void enableViewMenu() {

		// Enables the show log tab menu item
		AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowLogTabMenuItem().setEnabled(true);
	}

	/**
	 * Enables the menu items in the project menu.
	 */
	private void enableProjectMenu() {

		// Enables the new project menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getNewProjectMenuItem().setEnabled(true);

		// Enables the open project menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getOpenProjectMenuItem().setEnabled(true);

		// Enables the save project menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getSaveProjectMenuItem().setEnabled(true);
		
		// Enables the add file menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getAddFileMenuItem().setEnabled(true);
		
		// Enables the compile project menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getCompileMenuItem().setEnabled(true);
		
		// Enables the execute project menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getExecuteMenuItem().setEnabled(true);
	}

	/**
	 * Enables the menu items in the edit menu.
	 */
	private void enableEditMenu() {

		// Enables the undo menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem()
				.setEnabled(true);

		// Enables the redo menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getRedoMenuItem()
				.setEnabled(true);
	}

	/**
	 * Enables the menu items in the file menu.
	 */
	private void enableFileMenu() {

		// Enables the new file menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getNewFileMenuItem().setEnabled(true);

		// Enables the open file menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getOpenFileMenuItem().setEnabled(true);

		// Enables the save file menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getSaveFileMenuItem().setEnabled(true);

		// Enables the save all files menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getSaveAllFilesMenuItem().setEnabled(true);

		// Enables the print file menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getPrintFileMenuItem().setEnabled(true);

		// Enables the exit menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu().getExitMenuItem()
				.setEnabled(true);
	}
}
