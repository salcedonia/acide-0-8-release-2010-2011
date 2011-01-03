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
package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * ACIDE - A Configurable IDE file editor text edition area keyboard listener.
 * 
 * @version 0.8
 * @see KeyAdapter
 */
public class AcideFileEditorTextEditionAreaKeyboardListener extends KeyAdapter {
	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent keyEvent) {
		dispatchEvent(keyEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {
		dispatchEvent(keyEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent keyEvent) {
		dispatchEvent(keyEvent);
	};

	/**
	 * Dispatches the keyboard event.
	 * 
	 * @param keyEvent key event.
	 */
	private void dispatchEvent(KeyEvent keyEvent) {

		// Gets the selected file editor panel
		AcideFileEditorPanel selectedFileEditorPanel = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel();

		if (selectedFileEditorPanel.getTextEditionPanelList().get(0)
				.getBraceMatcher() != -1) {

			// Removes the highlighting of the brace
			selectedFileEditorPanel.getStyledDocument().removeHighlightBrace(
					selectedFileEditorPanel.getTextEditionPanelList().get(0)
							.getBraceMatcher());

			// Sets the brace matcher as -1
			selectedFileEditorPanel.getTextEditionPanelList().get(0)
					.setBraceMatcher(-1);
		}

		if (selectedFileEditorPanel.getTextEditionPanelList().get(1)
				.getBraceMatcher() != -1) {
			
			// Removes the highlighting of the brace
			selectedFileEditorPanel.getStyledDocument().removeHighlightBrace(
					selectedFileEditorPanel.getTextEditionPanelList().get(1)
							.getBraceMatcher());
			
			// Sets the brace matcher as -1
			selectedFileEditorPanel.getTextEditionPanelList().get(1)
					.setBraceMatcher(-1);
		}
	}
}
