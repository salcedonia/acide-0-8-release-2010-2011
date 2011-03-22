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
package acide.gui.menuBar.fileMenu.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file menu close file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideCloseFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If it is a valid file editor panel index
		if (selectedFileEditorPanelIndex != -1) {

			// If the editor has been modified
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.isRedButton()) {

				// Do you want to save it?
				int returnValue = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s643"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If it is OK
				if (returnValue == JOptionPane.OK_OPTION) {

					// Saves the file editor panel
					AcideMainWindow.getInstance().getMenu().getFileMenu()
							.saveFile(selectedFileEditorPanelIndex);
				}
			}

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Updates the file state in the project configuration
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getFileListSize(); index++) {

					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
									.getAbsolutePath())) {

						// Sets the file as closed
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsOpened(false);
					}
				}

				// The project is modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}

			// Removes the tab from the tabbed pane
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().remove(selectedFileEditorPanelIndex);

			// Validates the changes in the tabbed pane
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().validate();

			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanelIndex() != -1)
				
				// Updates the focus and so on...
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.updatesFileEditorAt(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());
		}
	}
}
