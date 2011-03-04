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
package gui.menuBar.fileMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * ACIDE - A Configurable IDE file menu save all files item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class SaveAllFilesMenuItemListener implements ActionListener {

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
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = MainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();

		// From the first to the last file editor
		for (int index = 0; index < numberOfFileEditorPanels; index++) {

			// Sets the selected file editor panel at the current index
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// Does the save or save as action
			MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
		}

		// Restores the original selected file editor panel
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);
	}
}
