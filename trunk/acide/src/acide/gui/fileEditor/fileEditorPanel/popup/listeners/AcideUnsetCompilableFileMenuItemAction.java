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
package acide.gui.fileEditor.fileEditorPanel.popup.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
 * file menu item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideUnsetCompilableFileMenuItemAction implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
	 * ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// If it is COMPILABLE FILE and not MAIN FILE
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().isCompilableFile()
				&& !AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile()) {

			// Sets COMPILER FILE to false
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setCompilableFile(false);

			// Updates the status message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath());

			// Removes the COMPILABLE icon from the selected file editor
			// panel
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setIconAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex(),
							null);

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

				// Searches for the file into the project configuration file
				// list
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath()))
						// Sets the COMPILABLE FILE as false
						AcideProjectConfiguration.getInstance()
								.getFileAt(index)
								.setIsCompilableFile(false);
				}
			}
		}
	}
}
