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
package acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE insert key listener.
 * 
 * @version 0.8
 * @see KeyListener
 */
public class AcideInsertKeyListener implements KeyListener{

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {
		dispatchEvent(keyEvent);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent keyEvent) {
		
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent keyEvent) {
		
	}

	/**
	 * Dispatches the key event.
	 * 
	 * @param keyEvent
	 *            key event.
	 */
	private void dispatchEvent(KeyEvent keyEvent) {

		// INSERT
		if (keyEvent.getKeyCode() == KeyEvent.VK_INSERT) {

			if (AcideMainWindow.getInstance().getStatusBar()
					.getEditionModeMessage().equals("INS"))

				// Updates the insert message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setEditionModeMessage("OVR");

			else if (AcideMainWindow.getInstance().getStatusBar()
					.getEditionModeMessage().equals("OVR"))

				// Updates the insert message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setEditionModeMessage("INS");

			// Sets the edition mode to overwrite in all the opened file editor
			// panels
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(index)
						.setEditionMode(
								!AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getEditionMode());
			}
		}
	}
}
