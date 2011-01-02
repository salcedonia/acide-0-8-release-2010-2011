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
package gui.listeners;

import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.gui.replace.AcideReplaceWindow;
import gui.menuBar.editMenu.gui.search.AcideSearchWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**																
 * ACIDE - A Configurable IDE mouse listener.
 *					
 * @version 0.8			
 * @see MouseAdapter																											
 */
public class AcideMouseListener extends MouseAdapter{

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		
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

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {

		String selectedText = null;
		int numEditor;
		numEditor = MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanelIndex();
		selectedText = MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor).getActiveTextEditionArea()
				.getSelectedText();
		
		if (selectedText != null) {
			AcideSearchWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
			AcideSearchWindow.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			AcideSearchWindow.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			AcideSearchWindow.getInstance().getAllRadioButton().setEnabled(true);
		}
		if (selectedText != null) {
			AcideReplaceWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
			AcideReplaceWindow.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			AcideReplaceWindow.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			AcideReplaceWindow.getInstance().getAllRadioButton().setEnabled(true);
		}
		
		// SEARCH
		AcideSearchWindow.getInstance().setCurrentPosition(-2);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsEnd(false);
		AcideSearchWindow.getInstance().getSearch().setTemporalPosition(-2);
		AcideSearchWindow.getInstance().getSearch().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.setIsFirst(true);
		AcideSearchWindow.getInstance().setSelectedText(null);
		
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
