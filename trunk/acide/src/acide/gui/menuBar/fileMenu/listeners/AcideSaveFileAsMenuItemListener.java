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
package acide.gui.menuBar.fileMenu.listeners;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * <p>
 * ACIDE - A Configurable IDE file menu save file as menu item listener.
 * </p>
 * <p>
 * There are four cases to be contemplated:
 * <ul>
 * <li>The new file belongs to the project and it is opened.</li>
 * <li>The new file belongs to the project and it is not opened.</li>
 * <li>The new file does not belong to the project and it is opened.</li>
 * <li>The new file does not belong to the project and it is not opened.</li>
 * </ul>
 * </p>
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveFileAsMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// If there are opened files
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() != 0) {

			String absoluteFilePath = null;
			String lastOpenedFileDirectory = null;

			// Creates the file chooser
			JFileChooser fileChooser = new JFileChooser();

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastOpenedFileDirectory = AcideResourceManager.getInstance()
						.getProperty("lastOpenedFileDirectory");

				// Sets the current directory to the last opened file directory
				fileChooser.setCurrentDirectory(new File(
						lastOpenedFileDirectory));

				// Clears the previous selected files
				fileChooser.setSelectedFiles(new File[0]);

				// Disables the multiple selection of files
				fileChooser.setMultiSelectionEnabled(false);

				// Sets only files
				fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Ask to the user for saving the changes
				int returnValueSaveFile = fileChooser.showSaveDialog(null);

				// Gets the selected file
				File selectedFile = fileChooser.getSelectedFile();

				// Ask the user for saving it
				if (returnValueSaveFile == JFileChooser.APPROVE_OPTION) {

					// Gets the ACIDE - A Configurable IDE last opened
					// file
					// directory
					absoluteFilePath = fileChooser.getSelectedFile()
							.getAbsolutePath();

					// If the user selected something
					if (absoluteFilePath != null) {

						// Gets the file name
						int lastIndexOfSlash = absoluteFilePath
								.lastIndexOf("\\");
						if (lastIndexOfSlash == -1)
							lastIndexOfSlash = absoluteFilePath
									.lastIndexOf("/");
						String fileName = absoluteFilePath
								.substring(lastIndexOfSlash + 1,
										absoluteFilePath.length());

						// Gets the overwritten index
						int overwrittenIndex = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(fileName);

						// Gets the current file content
						String currentFileContent = AcideMainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getTextEditionAreaContent();

						// If exists
						if (selectedFile.exists()) {

							// Ask if are you sure about the operation
							int returnValueAreYouSure = JOptionPane
									.showConfirmDialog(
											null,
											AcideLanguageManager.getInstance()
													.getLabels()
													.getString("s954"),
											AcideLanguageManager.getInstance()
													.getLabels()
													.getString("s953"),
											JOptionPane.YES_NO_OPTION);

							// If it is ok
							if (returnValueAreYouSure == JOptionPane.YES_OPTION) {

								// If the overwritten file exists in the tabbed
								// pane
								if (overwrittenIndex != -1) {

									// Removes the tab in the file editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getTabbedPane()
											.remove(AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex());

									// Validates the changes in the file editor
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane().validate();

									// Removes the tab that is overwritten from
									// the file
									// editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													overwrittenIndex);

									// Updates the components related to the
									// file panel
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.updateRelatedComponentsAt(
													overwrittenIndex);
								}

								// Saves the file
								boolean savingResult = AcideFileManager
										.getInstance().write(absoluteFilePath,
												currentFileContent);

								// If it could save it
								if (savingResult) {

									// Overwrites the opened file editor
									overwriteOpenedFileEditor(absoluteFilePath);
								} else {

									// Updates the log
									AcideLog.getLog().info(
											AcideLanguageManager.getInstance()
													.getLabels()
													.getString("s95")
													+ absoluteFilePath);
								}

							} else if (returnValueAreYouSure == JOptionPane.NO_OPTION) {

								// If the overwritten file exists in the tabbed
								// pane
								if (overwrittenIndex != -1) {

									// Removes the tab in the file editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getTabbedPane()
											.remove(AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex());

									// Validates the changes in the file editor
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane().validate();

									// Removes the tab that is overwritten from
									// the file
									// editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													overwrittenIndex);

									// Updates the components related to the
									// file panel
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.updateRelatedComponentsAt(
													overwrittenIndex);
								}

								// Overwrites the opened file editor
								overwriteOpenedFileEditor(absoluteFilePath);
							}
						} else {

							// If the overwritten file exists in the tabbed
							// pane
							if (overwrittenIndex != -1) {

								// Removes the tab in the file editor
								AcideMainWindow
										.getInstance()
										.getFileEditorManager()
										.getTabbedPane()
										.remove(AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex());

								// Validates the changes in the file editor
								AcideMainWindow.getInstance()
										.getFileEditorManager().getTabbedPane()
										.validate();

								// Removes the tab that is overwritten from the
								// file
								// editor
								AcideMainWindow
										.getInstance()
										.getFileEditorManager()
										.setSelectedFileEditorPanelAt(
												overwrittenIndex);

								// Updates the components related to the file
								// panel
								AcideMainWindow
										.getInstance()
										.getFileEditorManager()
										.updateRelatedComponentsAt(
												overwrittenIndex);
							}

							// Saves the file
							boolean savingResult = AcideFileManager
									.getInstance().write(absoluteFilePath,
											currentFileContent);

							// If it could save it
							if (savingResult) {

								// Overwrites the opened file editor
								overwriteOpenedFileEditor(absoluteFilePath);
							} else {

								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s95")
												+ absoluteFilePath);
							}
						}
					} else if (returnValueSaveFile == JFileChooser.CANCEL_OPTION) {

						// Cancel selection
						fileChooser.cancelSelection();

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s308"));

						// Removes the tab in the file editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// Validates the changes in the file editor
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().validate();
					}
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		} else
			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s89"));
	}

	/**
	 * <p>
	 * Overwrites an opened file editor with the new file.
	 * </p>
	 * 
	 * @param newFilePath
	 *            new file path.
	 */
	private void overwriteOpenedFileEditor(String newFilePath) {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s307")
						+ newFilePath);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels().getString("s93")
						+ newFilePath
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s94"));

		// Gets the new file name
		int lastIndexOfSlash1 = newFilePath.lastIndexOf("\\");
		if (lastIndexOfSlash1 == -1)
			lastIndexOfSlash1 = newFilePath.lastIndexOf("/");
		String newFileName = newFilePath.substring(lastIndexOfSlash1 + 1,
				newFilePath.length());

		// Updates the name property
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.putProperty("name", newFileName);

		// Sets the title
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getTabbedPane()
				.setTitleAt(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().getSelectedIndex(),
						newFileName);

		// Sets the file editor panel absolute path
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().setAbsolutePath(newFilePath);

		// Sets the file editor panel tool tip text
		AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.setToolTipText(newFilePath);

		// Builds the file to get the last changes
		File file = new File(AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getAbsolutePath());

		// Sets the last modification change
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel()
				.setLastChange(file.lastModified());

		// Sets the last length change
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().setLastChange(file.length());

		// Adds the new file to the recent files list
		AcideWorkbenchConfiguration.getInstance().getRecentFilesConfiguration()
				.addRecentFileToList(newFilePath);

		// Updates the ACIDE - A Configurable IDE last
		// opened
		// file
		// directory
		AcideResourceManager.getInstance().setProperty(
				"lastOpenedFileDirectory", newFilePath);
	}
}
