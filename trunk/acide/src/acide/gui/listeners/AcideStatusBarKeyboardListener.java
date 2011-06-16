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

import java.awt.Toolkit;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import acide.gui.mainWindow.AcideMainWindow;
import acide.utils.AcideOSChecker;

/**
 * ACIDE - A Configurable IDE status bar keyboard listener.
 * 
 * Handles the keyboard events which update the status bar panels, such as CAPS
 * LOCK, NUM LOCK, SCROLL LOCK and INSERT.
 * 
 * @version 0.8
 * @see KeyAdapter
 */
public class AcideStatusBarKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		// CAPS LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_CAPS_LOCK) {

			// CAPS LOCK only valid in WINDOWS
			if (AcideOSChecker.isWindows()) {

				// Gets the state
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());

				// Always the opposite
				if (!state)
					AcideMainWindow.getInstance().getStatusBar()
							.setCapsLock("CAPS");
				else
					AcideMainWindow.getInstance().getStatusBar()
							.setCapsLock("    ");
			} else
				AcideMainWindow.getInstance().getStatusBar()
						.setCapsLock("    ");
		}

		// NUM LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_NUM_LOCK) {

			// NUM LOCK only valid WINDOWS
			if (AcideOSChecker.isWindows()) {

				// Gets the state
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());

				// Always the opposite
				if (!state)
					AcideMainWindow.getInstance().getStatusBar()
							.setNumLockMessage("NUM");
				else
					AcideMainWindow.getInstance().getStatusBar()
							.setNumLockMessage("   ");
			} else
				AcideMainWindow.getInstance().getStatusBar()
						.setNumLockMessage("   ");
		}

		// SCROLL LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_SCROLL_LOCK) {

			// SCROLL LOCK only valid in WINDOWS
			if (AcideOSChecker.isWindows()) {

				// Gets the state
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());

				// Always the opposite
				if (!state)
					AcideMainWindow.getInstance().getStatusBar()
							.setScrollLockMessage("SCROLL");
				else
					AcideMainWindow.getInstance().getStatusBar()
							.setScrollLockMessage("     ");
			} else
				AcideMainWindow.getInstance().getStatusBar()
						.setScrollLockMessage("     ");
		}

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
