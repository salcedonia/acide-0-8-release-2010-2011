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

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file menu save file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveFileMenuItemListener implements ActionListener {

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

			// If it is the NEW FILE
			if (AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getAbsolutePath()
					.equals(AcideLanguageManager.getInstance().getLabels()
							.getString("s79"))) {

				// Saves the new file
				saveNewFile();

			} else {

				// Try to save the file content
				boolean savingResult = AcideFileManager
						.getInstance()
						.write(AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath(),
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getTextEditionAreaContent());

				// If it could save it
				if (savingResult) {

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s93")
									+ AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath()
									+ AcideLanguageManager.getInstance()
											.getLabels().getString("s94"));

					// Sets the green button
					AcideMainWindow.getInstance().getFileEditorManager()
							.setGreenButton();

					// Gets the file name
					int lastIndexOfSlash = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath()
							.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath()
								.lastIndexOf("/");
					lastIndexOfSlash++;
					String fileName = AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getAbsolutePath()
							.substring(
									lastIndexOfSlash,
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath().length());

					// Updates the name property
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getStyledDocument()
							.putProperty("name", fileName);
					
					// Sets the title
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.setTitleAt(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane().getSelectedIndex(),
									fileName);

					// Sets the file path
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setAbsolutePath(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());

					// Sets the tool tip text
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.setToolTipText(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());

					// Saves the original file
					File projectFile = new File(AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath());

					// Sets the last change
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setLastChange(projectFile.lastModified());

					// Sets the last size
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setLastSize(projectFile.length());

					// Updates the file disk copy
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setFileDiskCopy(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getTextEditionAreaContent());

					// Updates the save project in the menu bar tool bar
					AcideMainWindow.getInstance().getToolBarPanel()
							.getMenuBarToolBar().updateStateOfFileButtons();

					// Adds the new file to the recent files list
					AcideWorkbenchConfiguration.getInstance()
							.getRecentFilesConfiguration()
							.addRecentFileToList(fileName);
				}
			}
		} else
			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s89"));
	}

	/**
	 * Saves the new file.
	 */
	private void saveNewFile() {

		String absoluteFilePath = null;
		String lastOpenedFileDirectory = null;

		// Creates the file chooser
		JFileChooser fileChooser = new JFileChooser();

		try {

			// Gets the ACIDE - A Configurable IDE last opened file directory
			lastOpenedFileDirectory = AcideResourceManager.getInstance()
					.getProperty("lastOpenedFileDirectory");

			// Sets the current directory to the last opened file directory
			fileChooser.setCurrentDirectory(new File(lastOpenedFileDirectory));

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

					// Gets the name
					int index = absoluteFilePath.lastIndexOf("\\");
					if (index == -1)
						index = absoluteFilePath.lastIndexOf("/");
					String name = absoluteFilePath.substring(index + 1,
							absoluteFilePath.length());

					// Gets the overwritten index
					int overwrittenIndex = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(name);

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
	}
	
	/**
	 * <p>
	 * Overwrites an opened file editor with the new project file.
	 * </p>
	 * <p>
	 * If the selected file is opened in the tabbed pane, then updates its
	 * properties, whereas if it is not opened it updates its properties and
	 * adds it to the explorer tree.
	 * </p>
	 * 
	 * @param filePath
	 *            chosen file path.
	 */
	private void overwriteOpenedFileEditor(String filePath) {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s307")
						+ filePath);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels().getString("s93")
						+ filePath
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s94"));

		// Gets the file name
		int lastIndexOfSlash = filePath.lastIndexOf("\\");
		if (lastIndexOfSlash == -1)
			lastIndexOfSlash = filePath.lastIndexOf("/");
		String fileName = filePath.substring(lastIndexOfSlash + 1,
				filePath.length());

		// Updates the name property
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.putProperty("name", fileName);

		// Sets the title
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getTabbedPane()
				.setTitleAt(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().getSelectedIndex(), fileName);

		// Sets the file editor panel absolute path
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().setAbsolutePath(filePath);

		// Sets the file editor panel tool tip text
		AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.setToolTipText(filePath);

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
				.addRecentFileToList(filePath);

		// Updates the ACIDE - A Configurable IDE last
		// opened
		// file
		// directory
		AcideResourceManager.getInstance().setProperty(
				"lastOpenedFileDirectory", filePath);

		// Gets the project file
		AcideProjectFile projectFile = AcideProjectConfiguration.getInstance()
				.getFileAt(filePath);

		// If the selected file belongs to the project
		if (projectFile != null) {

			// If the file is not opened
			if (!projectFile.isOpened()) {

				// The file now is opened
				projectFile.setIsOpened(true);

				// Creates the image icon to set
				ImageIcon imageIcon = null;

				// Updates its type
				switch (projectFile.getType()) {

				case NORMAL:

					// It is not a main file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

					// It is not a compilable file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setCompilableFile(false);
					break;
				case COMPILABLE:

					// It is not a main file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

					// It is a compilable file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setCompilableFile(true);

					// Creates the COMPILABLE image icon to set
					imageIcon = new ImageIcon(
							"./resources/icons/editor/compilable.png");

					break;
				case MAIN:

					// It is a main file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(true);

					// It is a compilable file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setCompilableFile(true);

					// Creates the MAIN image icon to set
					imageIcon = new ImageIcon(
							"./resources/icons/editor/main.png");

					break;
				}

				// Sets the icon in the selected file editor panel
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								imageIcon);
				
				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			}
		}
	}
}
