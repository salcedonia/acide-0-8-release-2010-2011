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
package gui.menuBar.fileMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.SwingUtilities;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFileType;
import es.text.AcideTextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

/**																
 * ACIDE - A Configurable IDE file menu open file menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class OpenFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		AcideTextFile textFile = AcideIOFactory.getInstance().buildFile();
		String filePath = " ";
		filePath = textFile.askAbsolutePath();

		// If the file exists
		if (filePath != null) {

			boolean isOpened = false;

			// Checks if the file is already opened
			int fileIndex = -1;
			for (int position = 0; position < MainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels(); position++) {
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(position).getAbsolutePath().equals(filePath)) {
					isOpened = true;
					fileIndex = position;
				}
			}

			// If it is not opened
			if (!isOpened) {

				String text = null;
				text = textFile.load(filePath);

				// If the text is not empty
				if (text != null) {

					// Searches for the file into the project configuration file list
					int fileProjectIndex = -1;
					for (int index = 0; index < AcideProjectConfiguration.getInstance()
							.getNumberOfFilesFromList(); index++) {
						if (AcideProjectConfiguration.getInstance().getFileAt(index)
								.getAbsolutePath().equals(filePath))
							fileProjectIndex = index;
					}

					AcideProjectFileType fileType = AcideProjectFileType.NORMAL;
					
					// If belongs to the project
					if (fileProjectIndex > -1) {

						// Updates the status message in the status bar
						MainWindow.getInstance().getStatusBar().setStatusMessage(
								AcideProjectConfiguration.getInstance()
										.getFileAt(fileProjectIndex)
										.getAbsolutePath());

						// Is COMPILABLE FILE?
						if (AcideProjectConfiguration.getInstance().getFileAt(
										fileProjectIndex)
								.isCompilableFile()) {
							fileType = AcideProjectFileType.COMPILABLE;

							// Updates the status message in the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration.getInstance()
													.getFileAt(
															fileProjectIndex)
													.getAbsolutePath()
													+ " <COMPILABLE>");
						}

						// Is MAIN FILE?
						if (AcideProjectConfiguration.getInstance().getFileAt(
										fileProjectIndex).isMainFile()) {
							fileType = AcideProjectFileType.MAIN;

							// Updates the status message in the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration.getInstance()
													.getFileAt(
															fileProjectIndex)
													.getAbsolutePath()
													+ " <MAIN>");
						}

					} else {

						// If it does not belong to the project

						// Updates the status message in the status bar
						MainWindow.getInstance().getStatusBar().setStatusMessage(
								filePath);

					}

					// Opens a new tab with the content
					MainWindow.getInstance().getFileEditorManager().newTab(
							filePath, filePath, text, true, fileType, 0);
					
					// Updates the log
					AcideLog.getLog().info(labels.getString("s84") + filePath);
					AcideLog.getLog().info(labels.getString("s85") + filePath
							+ labels.getString("s86"));

					// Enables the file menu
					MainWindow.getInstance().getMenu().enableFileMenu();

					// Enables the edit menu
					MainWindow.getInstance().getMenu().enableEditMenu();
					
					// Updates the undo manager
					AcideUndoRedoManager.getInstance().update();

					// Sets the caret in the first position of the editor
					MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel().getActiveTextEditionArea().setCaretPosition(0);

					// Sets the new file state to opened
					for (int filePosition = 0; filePosition < AcideProjectConfiguration.getInstance()
							.getFileListSize(); filePosition++) {
						if (AcideProjectConfiguration.getInstance().getFileAt(
										filePosition).getAbsolutePath().equals(
										filePath)) {
							AcideProjectConfiguration.getInstance().getFileAt(
											filePosition).setIsOpened(true);
						}
					}

					// Sets the focus on the selected file at the editor
					for (int index = 0; index < MainWindow.getInstance()
							.getFileEditorManager()
							.getNumberOfFileEditorPanels(); index++) {

						final int editorIndex = index;

						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index).getAbsolutePath()
								.equals(filePath)) {

							SwingUtilities.invokeLater(new Runnable() {
								/*
								 * (non-Javadoc)
								 * 
								 * @see java.lang.Runnable#run()
								 */
								@Override
								public void run() {

									// Sets the focus on the text area
									MainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(editorIndex)
											.getActiveTextEditionArea()
											.requestFocusInWindow();
								}
							});
						}
					}
					// Not default project
					if (!AcideProjectConfiguration.getInstance().isDefaultProject())
						
						// The project has been modified
						AcideProjectConfiguration.getInstance()
								.setIsModified(true);

				} else {

					// EMPTY FILE
					
					// Updates the log
					AcideLog.getLog().info(labels.getString("s88"));
				}

			} else {

				// If it is already opened

				final int editorIndex = fileIndex;

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {

						// Sets the selected editor
						MainWindow.getInstance().getFileEditorManager()
								.setSelectedFileEditorPanelAt(editorIndex);

						// Sets the focus on the text component
						MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(editorIndex)
								.getActiveTextEditionArea()
								.requestFocusInWindow();
					}
				});
			}
		} else

			// FILE DOESN'T EXISTS
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s83"));
	}
}