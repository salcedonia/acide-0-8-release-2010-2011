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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.factory.AcideGUIFactory;
import es.configuration.project.AcideProjectConfiguration;
import gui.mainWindow.MainWindow;

/**
 * ACIDE -A Configurable IDE project menu new project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class NewProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		boolean cancelOptionSelected = false;

		// If the project has been modified
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Do you want to save it?
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s657"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (returnValue == JOptionPane.OK_OPTION) {

				// Enables the save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);

				// Does the save project menu item action
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();
			} else if (returnValue != JOptionPane.NO_OPTION)
				cancelOptionSelected = true;
		}

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = MainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();

		// Closes the opened files in the editor
		for (int index = numberOfFileEditorPanels - 1; index >= 0; index--) {

			// Sets the selected file editor at the last editor
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If the file is modified
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {

				// Do you want to save it?
				int returnValue = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s643"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If OK
				if (returnValue == JOptionPane.OK_OPTION)
					MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
			}
		}

		// Restores the original file editor panel
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);

		// Displays the new project configuration window
		if (!cancelOptionSelected)

			// Shows the project configuration window
			MainWindow.getInstance().setNewProjectConfigurationWindow(
					AcideGUIFactory.getInstance()
							.buildNewProjectConfigurationWindow());
	}
}
