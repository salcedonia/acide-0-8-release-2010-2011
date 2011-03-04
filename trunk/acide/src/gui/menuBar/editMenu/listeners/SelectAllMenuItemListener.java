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
package gui.menuBar.editMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * ACIDE - A Configurable IDE edit menu select all menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class SelectAllMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Sets the caret in the first position of the editor
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
				.setCaretPosition(0);

		// Get the text length
		int length = MainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
				.getText().length();

		// Sets the selection from the first the last
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
				.setSelectionEnd(length);
	}
}
