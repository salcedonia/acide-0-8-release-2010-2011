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
package gui.menuBar.fileMenu.listeners;

import es.project.AcideProjectFileType;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.SwingUtilities;

import language.AcideLanguageManager;
import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file menu new file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class NewFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Enables the file menu
		MainWindow.getInstance().getMenu().enableFileMenu();

		// Enables the edit menu
		MainWindow.getInstance().getMenu().enableEditMenu();

		// Adds the new tab to the tabbed pane
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.updatesTabbedPane(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s79"), "", true,
						AcideProjectFileType.NORMAL, 0, 0);

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s80"));

		// Enables the file menu
		MainWindow.getInstance().getMenu().enableFileMenu();

		// Enables the edit menu
		MainWindow.getInstance().getMenu().enableEditMenu();

		// Updates the undo manager
		AcideUndoRedoManager.getInstance().update();

		// Updates the status message in the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setStatusMessage(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s79"));

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// Sets the focus on the text area
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().requestFocusInWindow();
			}
		});
	}
}
