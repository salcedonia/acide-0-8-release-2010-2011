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
package acide.gui.mainWindow.utils;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE last element on focus.
 * 
 * @version 0.8
 */
public enum AcideLastElementOnFocus {

	/**
	 * The last element on focus is the ACIDE - A Configurable IDE file editor
	 * panel.
	 */
	FILE_EDITOR,
	/**
	 * The last element on focus is the ACIDE - A Configurable IDE console
	 * panel.
	 */
	CONSOLE_PANEL;

	/**
	 * Sets the focus on the last element on focus in the ACIDE - A Configurable
	 * IDE main window.
	 * 
	 * @param lastElementOnFocus
	 */
	public static void setFocusOnLastElementOnFocus(
			AcideLastElementOnFocus lastElementOnFocus) {

		switch (lastElementOnFocus) {

		case CONSOLE_PANEL:

			// Sets the focus on the ACIDE - A Configurable IDE console panel
			AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.requestFocusInWindow();
			break;
		case FILE_EDITOR:

			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels() > 0)
				// Sets the focus on the ACIDE - A Configurable IDE file editor
				// panel
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().requestFocusInWindow();
			break;
		}
	}
}
