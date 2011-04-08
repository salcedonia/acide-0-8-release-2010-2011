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
package acide.gui.menuBar.projectMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.toolBarPanel.menuBarToolBar.AcideMenuBarToolBar;
import acide.language.AcideLanguageManager;
import acide.resources.AcideResourceManager;

/**
 * ACIDE -A Configurable IDE project menu close project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideCloseProjecMenuItemtListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Asks to the user for saving the project
		boolean isCancelSelected = AcideProjectConfiguration.getInstance()
				.askForSavingProject();

		// If in the closing project operation the cancel option has not
		// been
		// selected
		if (!isCancelSelected) {

			// Enables the close all files menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getCloseAllFilesMenuItem().setEnabled(true);

			// Close all files in the project
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getCloseAllFilesMenuItem().doClick();
			
			// Removes all the nodes in the explorer tree
			AcideMainWindow.getInstance().getExplorerPanel().getRoot()
					.removeAllChildren();

			// Reloads the explorer tree model
			AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
					.reload();

			// Disables the add file menu item in the explorer popup menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getAddFileMenuItem().setEnabled(false);

			// Disables the save project menu item in the explorer popup
			// menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getSaveProjectMenuItem().setEnabled(false);

			// Disables the remove file menu item in the explorer popup menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFileMenuItem().setEnabled(false);

			// Disables the delete file menu item in the explorer popup menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFileMenuItem().setEnabled(false);

			// Removes all the files related to the project
			AcideProjectConfiguration.getInstance().removeFiles();

			// Disables the project menu
			AcideMainWindow.getInstance().getMenu().disableProjectMenu();

			// Disables the open all files menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenAllFilesMenuItem().setEnabled(false);

			// Disables the save project button in the static tool bar
			AcideMenuBarToolBar.getInstance().getSaveProjectButton()
					.setEnabled(false);
			
			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setStatusMessage(" ");

			// Sets the default title to the project
			AcideMainWindow.getInstance().setTitle(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s425")
							+ " - <empty>");
			
			// Updates the ACIDE - A Configurable IDE project configuration
			AcideResourceManager.getInstance().setProperty(
					"projectConfiguration",
					"./configuration/project/default.acidePrj");

			// Sets the project name as empty
			AcideProjectConfiguration.getInstance().setName("");

			// The project has not been modified yet
			AcideProjectConfiguration.getInstance().setIsModified(false);
			
			// Updates the changes in the main window
			AcideMainWindow.getInstance().validate();

			// Repaints the main window
			AcideMainWindow.getInstance().repaint();
		}
	}
}
