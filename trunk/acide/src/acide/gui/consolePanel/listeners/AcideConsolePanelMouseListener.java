/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.gui.consolePanel.listeners;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE console panel mouse listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideConsolePanelMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {

		// If the console panel text pane is editable
		if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
				.isEditable()) {

			// Checks if there is text selected
			boolean thereIsTextSelected = AcideMainWindow.getInstance()
					.getConsolePanel().getTextPane().getSelectedText() == null;

			// If the caret position is before the prompt caret position
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.getCaretPosition() < AcideMainWindow.getInstance()
					.getConsolePanel().getPromptCaretPosition()
					&& thereIsTextSelected)

				// Puts the caret position after the prompt caret position
				AcideMainWindow
						.getInstance()
						.getConsolePanel()
						.getTextPane()
						.setCaretPosition(
								AcideMainWindow.getInstance().getConsolePanel()
										.getPromptCaretPosition());
		}
	}
}