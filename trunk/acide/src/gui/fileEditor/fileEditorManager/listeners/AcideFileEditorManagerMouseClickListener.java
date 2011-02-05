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
package gui.fileEditor.fileEditorManager.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor manager mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideFileEditorManagerMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/**
	 * Dispatches the mouse event.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void dispatchEvent(MouseEvent mouseEvent) {

		// If there are opened editors
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getComponentCount() != 0) {

			// Gets the selected editor path
			String selectedEditorPath = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getAbsolutePath();

			// Sets the focus on the selected editor
			SwingUtilities.invokeLater(new Runnable() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see java.lang.Runnable#run()
				 */
				@Override
				public void run() {

					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex() != -1) {

						// Gets the active text pane
						JTextPane activeTextPane = MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										MainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex())
								.getActiveTextEditionArea();

						// Sets the caret position at its position
						activeTextPane.setCaretPosition(activeTextPane
								.getCaretPosition());

						// Sets the caret visible
						activeTextPane.getCaret().setVisible(true);

						// Sets the focus on the active text component
						activeTextPane.requestFocusInWindow();
					}
				}
			});

			// If has path
			if (selectedEditorPath != null) {

				File file = new File(selectedEditorPath);

				if ((file.lastModified() != MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getLastChange())
						|| (file.length() != MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getLastSize())) {

					// Gets the language
					AcideLanguageManager language = AcideLanguageManager.getInstance();

					try {
						language.getLanguage(AcideResourceManager.getInstance()
								.getProperty("language"));
					} catch (Exception exception) {

						// Updates the log
						AcideLog.getLog().error(exception.getMessage());
						exception.printStackTrace();
					}

					// Gets the labels
					ResourceBundle labels = language.getLabels();

					// Ask to the user for saving 
					int chosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s65"));

					// OK OPTION
					if (chosenOption == JOptionPane.OK_OPTION) {

						TextFile newTextFile = new TextFile();
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.loadText(newTextFile.load(selectedEditorPath));
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(file.lastModified());
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(file.length());
					} else {
						
						// NO OPTION
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(file.lastModified());
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(file.length());
					}
				}
			}
		}

		// Updates the status bar with the type of file selected in the file
		// editor
		MainWindow.getInstance().getStatusBar()
				.updatesStatusBarFromFileEditor();

		// Selects the node in the tree that matches with the clicked file
		// editor
		MainWindow.getInstance().getExplorerPanel().selectTreeNodeFromFileEditor();

		// Updates the button icons
		MainWindow.getInstance().getFileEditorManager().updatesButtonIcons();
	}
}
