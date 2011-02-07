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

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFileType;
import es.text.AcideTextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import language.AcideLanguageManager;

import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file menu open all files menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class OpenAllFilesMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

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
		final ResourceBundle labels = language.getLabels();

		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(AcideProjectConfiguration.getInstance()
					.getFileAt(index).getAbsolutePath());

			// If the file is not a directory and exists
			if (!AcideProjectConfiguration.getInstance().getFileAt(index)
					.isDirectory()
					&& file.exists()) {

				AcideTextFile textFile = AcideIOFactory.getInstance().buildFile();
				String text = null;
				text = textFile.load(AcideProjectConfiguration.getInstance()
						.getFileAt(index).getAbsolutePath());
				String filePath = AcideProjectConfiguration.getInstance()
						.getFileAt(index).getAbsolutePath();

				// Checks if the file is already opened
				boolean isOpened = false;

				for (int position = 0; position < MainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); position++) {
					if (MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(position).getAbsolutePath()
							.equals(filePath)) {
						isOpened = true;
					}
				}

				// If it is not opened
				if (!isOpened) {

					String fileName = null;

					// Gets the file name
					if (filePath != null) {

						int lastIndexOfSlash = filePath.lastIndexOf("\\");
						if (lastIndexOfSlash == -1)
							lastIndexOfSlash = filePath.lastIndexOf("/");
						fileName = filePath.substring(lastIndexOfSlash + 1,
								filePath.length());
					}

					// Updates the status message in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideProjectConfiguration.getInstance()
											.getFileAt(index).getAbsolutePath());

					// Check if it is a MAIN or COMPILABLE FILE
					AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

					// COMPILABLE
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index).isCompilableFile()) {

						fileType = AcideProjectFileType.COMPILABLE;

						// Adds the <COMPILABLE> tag in the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance()
												.getFileAt(index)
												.getAbsolutePath()
												+ " <COMPILABLE>");
					}

					// MAIN
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index).isMainFile()) {

						fileType = AcideProjectFileType.MAIN;

						// Adds the <MAIN> tag in the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance()
												.getFileAt(index)
												.getAbsolutePath()
												+ " <MAIN>");
					}

					// Opens a new tab in the editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.newTab(fileName, filePath, text, true, fileType, 0);

					// Checks if it is marked as a MAIN or COMPILABLE FILE
					for (int i = 0; i < MainWindow.getInstance()
							.getFileEditorManager().getNumberOfFileEditorPanels(); i++) {

						if (MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(i)
								.getAbsolutePath()
								.equals(AcideProjectConfiguration.getInstance()
										.getFileAt(index).getAbsolutePath())) {

							// IS COMPILABLE FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index).isCompilableFile())
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i)
										.setCompilableFile(true);

							// IS MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index).isMainFile())
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i)
										.setMainFile(true);
						}
					}

					// Enables the file menu
					MainWindow.getInstance().getMenu().enableFileMenu();

					// Enables the edit menu
					MainWindow.getInstance().getMenu().enableEditMenu();

					// Updates the undo manager
					AcideUndoRedoManager.getInstance().update();

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(false);
				}
				else{
					
					// Is already opened
				}
			} else {

				// If the file does not exist
				if (!file.exists()) {

					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s970")
									+ AcideProjectConfiguration.getInstance()
											.getFileAt(index).getAbsolutePath()
									+ labels.getString("s971"), "Error",
							JOptionPane.ERROR_MESSAGE);

					// Removes the file from the project
					AcideProjectConfiguration.getInstance().removeFileAt(index);

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			}
		}

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// Sets the focus in the edition area
				MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().requestFocusInWindow();

				// Sets the caret visible
				MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().getCaret().setVisible(true);

				// Selects the tree node
				MainWindow.getInstance().getExplorerPanel()
						.selectTreeNodeFromFileEditor();

				// Updates the status bar with the selected editor
				MainWindow.getInstance().getStatusBar()
						.updatesStatusBarFromFileEditor();
			}
		});
	}
}
