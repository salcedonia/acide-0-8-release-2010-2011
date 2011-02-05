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
package gui.menuBar.projectMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.window.AcideWindowConfiguration;
import gui.mainWindow.MainWindow;
import gui.toolBarPanel.staticToolBar.AcideStaticToolBar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE -A Configurable IDE project menu close project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class CloseProjecMenuItemtListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		boolean isCancelOptionSelected = false;

		// If the project has been modified
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Do you want to save it?
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (chosenOption == JOptionPane.OK_OPTION) {

				// Enables the save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);

				// Save the project
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();
			} else if (chosenOption != JOptionPane.NO_OPTION)
				isCancelOptionSelected = true;
		}

		if (!isCancelOptionSelected) {

			// Removes all the nodes in the explorer tree
			MainWindow.getInstance().getExplorerPanel().getRoot()
					.removeAllChildren();

			// Reloads the explorer tree model
			MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

			// Disables the add file menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getAddFile().setEnabled(false);

			// Disables the save project menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getSaveProject().setEnabled(false);

			// Disables the remove file menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(false);

			// Disables the delete file menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFile().setEnabled(false);

			// Saves the window configuration
			AcideWindowConfiguration.getInstance()
					.save();

			// Sets the default title to the project
			MainWindow.getInstance().setTitle(
					labels.getString("s425") + " - <empty>");

			// Removes all the files related to the project
			AcideProjectConfiguration.getInstance().removeFiles();

			// Updates the MAIN WINDOW
			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("defaultAcideProject",
					"./configuration/project/default.acidePrj");

			// Sets the project name as empty
			AcideProjectConfiguration.getInstance().setName("");

			// The project has not been modified yet
			AcideProjectConfiguration.getInstance()
					.setIsModified(false);

			// Enables the close all files menu item
			MainWindow.getInstance().getMenu().getFile().getCloseAllFiles()
					.setEnabled(true);

			// Close all files in the project
			MainWindow.getInstance().getMenu().getFile().getCloseAllFiles()
					.doClick();

			// Disables the project menu
			MainWindow.getInstance().getMenu().disableProjectMenu();

			// Disables the open all files menu item
			MainWindow.getInstance().getMenu().getFile().getOpenAllFiles().setEnabled(false);
			
			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
			
			// Disables the save project button in the static tool bar
			AcideStaticToolBar.getInstance().getSaveProjectButton().setEnabled(false);
		}
	}
}
