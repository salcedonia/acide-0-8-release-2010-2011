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

import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.configuration.project.AcideProjectConfiguration;
import acide.factory.gui.AcideGUIFactory;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE -A Configurable IDE project menu new project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideNewProjectMenuItemListener implements ActionListener {

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
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveProjectMenuItem().setEnabled(true);

				// Does the save project menu item action
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveProjectMenuItem().doClick();
			} else if (returnValue != JOptionPane.NO_OPTION)
				cancelOptionSelected = true;
		}

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();

		// Closes the opened files in the editor
		for (int index = numberOfFileEditorPanels - 1; index >= 0; index--) {

			// Sets the selected file editor at the last editor
			AcideMainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If the file is modified
			if (AcideMainWindow.getInstance().getFileEditorManager().isRedButton()) {

				// Do you want to save it?
				int returnValue = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s643"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If OK
				if (returnValue == JOptionPane.OK_OPTION)
					AcideMainWindow.getInstance().getMenu().getFileMenu().saveOrSaveAS();
			}
		}

		// Restores the original file editor panel
		AcideMainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);

		// Displays the new project configuration window
		if (!cancelOptionSelected)

			// Shows the project configuration window
			AcideMainWindow.getInstance().setNewProjectConfigurationWindow(
					AcideGUIFactory.getInstance()
							.buildNewProjectConfigurationWindow());
	}
}
