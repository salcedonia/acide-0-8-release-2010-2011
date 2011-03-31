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
import acide.gui.menuBar.editMenu.gui.AcideSearchReplaceWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * ACIDE - A Configurable IDE search/replace window mouse listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideSearchReplaceWindowMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Updates the search/replace window variables
		AcideSearchReplaceWindow.getInstance().initializeVariables();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {

		// Gets the selected text
		String selectedText = AcideMainWindow.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getActiveTextEditionArea().getSelectedText();

		// If something is selected
		if (selectedText != null) {

			// Sets the selected text radio button to true in the search/replace
			// window
			AcideSearchReplaceWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Sets the all radio button to false in the search/replace window
			AcideSearchReplaceWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(false);
			
			// Updates the search text field with the selected text in
			// the
			// search/replace window
			AcideSearchReplaceWindow.getInstance().getSearchTextField()
					.setText(selectedText);

		} else {

			// Sets the current document radio button to true in the
			// search/replace window
			AcideSearchReplaceWindow.getInstance()
					.getCurrentDocumentRadioButton().setSelected(true);

			// Sets the all radio button to true in the search/replace window
			AcideSearchReplaceWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(true);
			
			// Sets the replace text field to empty in the
			// search/replace
			// window
			AcideSearchReplaceWindow.getInstance().getSearchTextField()
					.setText("");
		}

		// Updates the search/replace window variables
		AcideSearchReplaceWindow.getInstance().initializeVariables();
	}
}
