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
import acide.gui.menuBar.editMenu.gui.replace.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.search.AcideSearchWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * ACIDE - A Configurable IDE search and replace window mouse listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideSearchAndReplaceWindowMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Updates the options in the search and replace windows
		updatesSearchAndReplaceWindows();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the selected text
		String selectedText = AcideMainWindow.getInstance().getFileEditorManager()
				.getFileEditorPanelAt(selectedFileEditorPanelIndex)
				.getActiveTextEditionArea().getSelectedText();

		// If something is selected
		if (selectedText != null) {

			// Sets the selected text radio button to true in the SEARCH WINDOW
			AcideSearchWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Sets the selected text radio button to true in the REPLACE WINDOW
			AcideReplaceWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Sets the all radio button to false in the SEARCH WINDOW
			AcideSearchWindow.getInstance().getAllRadioButton()
					.setEnabled(false);

			// Sets the all radio button to false in the REPLACE WINDOW
			AcideReplaceWindow.getInstance().getAllRadioButton()
					.setEnabled(false);

		} else {

			// Sets the current document radio button to true in the SEARCH
			// WINDOW
			AcideSearchWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(true);

			// Sets the current document radio button to true in the REPLACE
			// WINDOW
			AcideReplaceWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(true);
			
			// Sets the all radio button to true in the SEARCH
			// WINDOW
			AcideSearchWindow.getInstance().getAllRadioButton()
					.setEnabled(true);
			
			// Sets the all radio button to true in the REPLACE
			// WINDOW
			AcideReplaceWindow.getInstance().getAllRadioButton()
					.setEnabled(true);
		}

		// Updates the options in the search and replace windows
		updatesSearchAndReplaceWindows();
	}
	
	/**
	 * Updates the configuration of the ACIDE - A Configurable IDE search and replace windows.
	 */
	private void updatesSearchAndReplaceWindows() {
		
		// SEARCH
		AcideSearchWindow.getInstance().setCurrentPosition(-2);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsEnd(false);
		AcideSearchWindow.getInstance().getSearch().setTemporalPosition(-2);
		AcideSearchWindow.getInstance().getSearch().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setSelectedText(null);
		AcideSearchWindow.setIsFirst(true);

		// REPLACE
		AcideReplaceWindow.getInstance().setCurrentPosition(-2);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsEnd(false);
		AcideReplaceWindow.getInstance().getSearch().setTemporalPosition(-2);
		AcideReplaceWindow.getInstance().getSearch().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setSelectedText(null);
		AcideReplaceWindow.setIsFirstSearch(true);
		AcideReplaceWindow.setIsFirstReplacement(true);
	}
}
