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
package acide.gui.consolePanel.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;

/**
 * ACIDE - A Configurable IDE console panel caret listener.
 * 
 * @version 0.8
 * @see CaretListener
 */
public class AcideConsolePanelCaretListener implements CaretListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
	 */
	public void caretUpdate(CaretEvent caretEvent) {

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// If there is no selected text in the console
				if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectedText() == null) {
					
					// If the caret position is behind the prompt mark
					if (AcideMainWindow.getInstance().getConsolePanel()
							.getTextPane().getCaretPosition() < AcideMainWindow
							.getInstance().getConsolePanel().getTextPane()
							.getText().length()
							- AcideMainWindow
							.getInstance().getConsolePanel().getSelectionSize()) {

						// Puts the caret to the end of the text
						AcideMainWindow
								.getInstance()
								.getConsolePanel()
								.getTextPane()
								.setCaretPosition(
										AcideMainWindow.getInstance()
												.getConsolePanel()
												.getTextPane().getText()
												.length());
					}
				}
			}
		});
	}
}
