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
import acide.gui.menuBar.editMenu.gui.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.AcideSearchWindow;

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

		// Updates the search window variables
		AcideSearchWindow.getInstance().setCurrentPosition(-2);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsEnd(false);
		AcideSearchWindow.getInstance().getSearchEngine()
				.setTemporalPosition(-2);
		AcideSearchWindow.getInstance().getSearchEngine().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.setIsFirst(true);
		AcideSearchWindow.getInstance().setSelectedText(null);

		// Updates the replace window variables
		AcideReplaceWindow.getInstance().setCurrentPosition(-2);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsEnd(false);
		AcideReplaceWindow.getInstance().getSearchEngine()
				.setTemporalPosition(-2);
		AcideReplaceWindow.getInstance().getSearchEngine().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setSelectedText(null);
		AcideReplaceWindow.setIsFirst(true);
		AcideReplaceWindow.setIsFirstReplacement(true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {

		String selectedText = null;

		// Gets the selected file editor panel index
		int numeditor = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanelIndex();

		// Gets the selected text
		selectedText = AcideMainWindow.getInstance().getFileEditorManager()
				.getFileEditorPanelAt(numeditor).getActiveTextEditionArea()
				.getSelectedText();

		// If there is selected text
		if (selectedText != null) {

			// Selects the selected text radio button in the search window
			AcideSearchWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Deselects the both directions radio button in the search window
			AcideSearchWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(false);
		} else {

			// Selects the current document radio button in the search window
			AcideSearchWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(true);

			// Selects the both directions radio button in the search window
			AcideSearchWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(true);
		}

		// If there is selected text
		if (selectedText != null) {

			// Selects the selected text radio button in the replace window
			AcideReplaceWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Deselects the both directions radio button in the replace window
			AcideReplaceWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(false);
		} else {

			// Selects the current document radio button in the replace window
			AcideReplaceWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(true);

			// Selects the both directions radio button in the replace window
			AcideReplaceWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(true);
		}

		// Updates the search window variables
		AcideSearchWindow.getInstance().setCurrentPosition(-2);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsEnd(false);
		AcideSearchWindow.getInstance().getSearchEngine()
				.setTemporalPosition(-2);
		AcideSearchWindow.getInstance().getSearchEngine().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.setIsFirst(true);
		AcideSearchWindow.getInstance().setSelectedText(null);

		// Updates the replace window variables
		AcideReplaceWindow.getInstance().setCurrentPosition(-2);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsEnd(false);
		AcideReplaceWindow.getInstance().getSearchEngine()
				.setTemporalPosition(-2);
		AcideReplaceWindow.getInstance().getSearchEngine().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setSelectedText(null);
		AcideReplaceWindow.setIsFirst(true);
		AcideReplaceWindow.setIsFirstReplacement(true);
	}
}
