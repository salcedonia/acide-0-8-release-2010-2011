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
package gui.consolePanel.listeners;

import gui.consolePanel.AcideConsolePanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * ACIDE - A Configurable IDE console panel popup menu listener.										
 *					
 * @version 0.8	
 * @see MouseAdapter																													
 */
public class AcideConsolePanelPopupMenuListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/**
	 * Shows the popup menu.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void maybeShowPopup(MouseEvent mouseEvent) {

		// Gets the ACIDE - A Configurable IDE console panel
		AcideConsolePanel consolePanel = MainWindow.getInstance().getConsolePanel();
		
		// If the popup has to be shown
		if (mouseEvent.isPopupTrigger()) {

			// If there is not selected text
			if (consolePanel.getTextPane().getSelectedText() == null) {

				// Disables the copy menu item
				consolePanel.getPopupMenu().getCopy().setEnabled(false);

				// Disables the cut menu item
				consolePanel.getPopupMenu().getCut().setEnabled(false);
			} else {

				// Enables the copy menu item
				consolePanel.getPopupMenu().getCopy().setEnabled(true);

				// If the caret is after the prompt
				if (consolePanel.getTextPane().getSelectionStart() < consolePanel.getPromptCaretPosition())
					consolePanel.getPopupMenu().getCut().setEnabled(false);
				else
					consolePanel.getPopupMenu().getCut().setEnabled(true);
			}

			// If the caret position is before the prompt
			if (consolePanel.getTextPane().getSelectionStart() < consolePanel.getPromptCaretPosition())
				consolePanel.getPopupMenu().getPaste().setEnabled(false);
			else
				consolePanel.getPopupMenu().getPaste().setEnabled(true);

			// If the console text pane is editable
			if(consolePanel.getTextPane().isEditable())
			
				// Shows the popup menu
				consolePanel.getPopupMenu().show(mouseEvent.getComponent(), mouseEvent.getX(), mouseEvent.getY());
		}
	}
}
