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
package acide.files;

import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.*;

import acide.resources.AcideResourceManager;

import java.io.*;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * <p>
 * ACIDE - A Configurable IDE file manager.
 * </p>
 * <p>
 * Handles the saving and loading operations over text files, and ask to the
 * user about the files or directories to select in dialogs.
 * </p>
 * 
 * @version 0.8
 */
public class AcideFileManager {

	/**
	 * ACIDE - A Configurable IDE file manager unique instance.
	 */
	private static AcideFileManager _instance;
	/**
	 * ACIDE - A Configurable IDE file manager file chooser.
	 */
	private JFileChooser _fileChooser;

	/**
	 * Returns the ACIDE - A Configurable IDE file manager unique instance.
	 * 
	 * @return ACIDE - A Configurable IDE file manager unique instance.
	 */
	public static AcideFileManager getInstance() {
		if (_instance == null)
			_instance = new AcideFileManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE file manager.
	 */
	public AcideFileManager() {
		// Creates the file chooser
		_fileChooser = new JFileChooser();
	}

	/**
	 * <p>
	 * Asks for open a file to the user with the chosen options given as
	 * parameters.
	 * </p>
	 * <p>
	 * Returns the absolute paths selected by the user in a file chooser dialog.
	 * </p>
	 * 
	 * @param isFile
	 *            flag that indicates if the current path has to be extracted
	 *            and updated from the last file or project directory stored in
	 *            the ACIDE - A Configurable IDE resource manager.
	 * 
	 * @return the absolute paths selected by the user in a file chooser dialog.
	 */
	public String askForOpenFile(boolean isFile) {

		File selectedFile = null;
		String absolutePath = null;
		String lastPath = null;

		try {

			// If is for open a file
			if (isFile)

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedFileDirectory");
			else
				// Gets the ACIDE - A Configurable IDE last opened project
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedProjectDirectory");

			// Sets the title of the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory to the default path
			_fileChooser
					.setCurrentDirectory(new File(lastPath));

			// Clears the previous selected files
			_fileChooser.setSelectedFiles(new File[0]);
			
			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file
				selectedFile = _fileChooser.getSelectedFile();

				// Gets the absolute path
				absolutePath = selectedFile.getAbsolutePath();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s300")
								+ absolutePath);

				// If is for open a file
				if (isFile)
					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedFileDirectory",
							absolutePath);
				else
					// Updates the ACIDE - A Configurable IDE last opened
					// project
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedProjectDirectory",
							absolutePath);

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return absolutePath;
	}

	/**
	 * <p>
	 * Asks for open a file to the user with the chosen options given as
	 * parameters.
	 * </p>
	 * <p>
	 * Returns the absolute paths selected by the user in a file chooser dialog.
	 * </p>
	 * 
	 * @param currentDirectory
	 *            current directory to set in the file chooser.
	 * 
	 * @return the absolute paths selected by the user in a file chooser dialog.
	 */
	public String askForOpenFile(String currentDirectory) {

		File selectedFile = null;
		String absolutePath = null;

		try {

			// Sets the title of the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory
			_fileChooser.setCurrentDirectory(new File(currentDirectory));

			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Clears the previous selected files
			_fileChooser.setSelectedFiles(new File[0]);
			
			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file
				selectedFile = _fileChooser.getSelectedFile();

				// Gets the absolute path
				absolutePath = selectedFile.getAbsolutePath();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s300")
								+ absolutePath);

				// Updates the ACIDE - A Configurable IDE last opened
				// file
				// directory
				AcideResourceManager.getInstance().setProperty(
						"lastOpenedFileDirectory",
						absolutePath);

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return absolutePath;
	}

	/**
	 * <p>
	 * Asks for open files to the user with the chosen options given as
	 * parameters.
	 * </p>
	 * <p>
	 * Returns an array that contains the absolute paths selected by the user in
	 * a file chooser dialog.
	 * </p>
	 * 
	 * @param isFile
	 *            flag that indicates if the current path has to be extracted
	 *            and updated from the last file or project directory stored in
	 *            the ACIDE - A Configurable IDE resource manager.
	 * 
	 * @return an array that contains the absolute paths selected by the user in
	 *         a file chooser dialog.
	 */
	public String[] askForOpenFiles(boolean isFile) {

		File[] selectedFiles = null;
		String absolutePaths[] = null;
		String lastPath = null;

		try {

			// If is for open a file
			if (isFile)

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedFileDirectory");
			else
				// Gets the ACIDE - A Configurable IDE last opened project
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedProjectDirectory");

			// Sets the title of the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory to the default path
			_fileChooser
					.setCurrentDirectory(new File(lastPath));

			// Clears the previous selected files
			_fileChooser.setSelectedFiles(new File[0]);
			
			// Enables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(true);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected files
				selectedFiles = _fileChooser.getSelectedFiles();

				// Creates the string array
				absolutePaths = new String[selectedFiles.length];

				// Stores the selected absolute paths in the string array
				for (int index = 0; index < selectedFiles.length; index++) {

					// Gets the absolute path
					absolutePaths[index] = selectedFiles[index]
							.getAbsolutePath();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s300")
									+ absolutePaths[index]);

					// If is for open a file
					if (isFile)
						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedFileDirectory",
								absolutePaths[index]);
					else
						// Updates the ACIDE - A Configurable IDE last opened
						// project
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedProjectDirectory",
								absolutePaths[index]);
				}

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return absolutePaths;
	}

	/**
	 * Returns the directory absolute path selected by the user in a file
	 * chooser dialog.
	 * 
	 * @return the directory absolute path selected by the user in a file
	 *         chooser dialog.
	 */
	public String askForOpenDirectory() {

		File selectedFile = null;
		String absolutePath = null;
		String lastPath = null;

		try {

			// Gets the ACIDE - A Configurable IDE last opened file
			// directory
			lastPath = AcideResourceManager.getInstance().getProperty(
					"lastOpenedFileDirectory");

			// Sets the title of the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory to the default path
			_fileChooser
					.setCurrentDirectory(new File(lastPath));

			// Clears the previous selected files
			_fileChooser.setSelectedFiles(new File[0]);
			
			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only directories
			_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file
				selectedFile = _fileChooser.getSelectedFile();

				// Gets the absolute path
				absolutePath = selectedFile.getAbsolutePath();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s300")
								+ absolutePath);

				// Updates the ACIDE - A Configurable IDE last opened
				// file
				// directory
				AcideResourceManager.getInstance().setProperty(
						"lastOpenedFileDirectory",
						absolutePath);

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return absolutePath;
	}

	/**
	 * Asks to the user for the path to save a file.
	 * 
	 * @param isNewProjectFile
	 *            flag that indicates if it is the new project file or not for
	 *            the new project file menu item.
	 * 
	 * @return the absolute file path.
	 */
	public String askForSaving(boolean isNewProjectFile) {

		String absoluteFilePath = null;
		String lastOpenedFileDirectory = null;

		try {

			// Gets the ACIDE - A Configurable IDE last opened file directory
			lastOpenedFileDirectory = AcideResourceManager.getInstance()
					.getProperty("lastOpenedFileDirectory");

			// Sets the current directory to the last opened file directory
			_fileChooser.setCurrentDirectory(new File(lastOpenedFileDirectory)
					.getParentFile());

			// Clears the previous selected files
			_fileChooser.setSelectedFiles(new File[0]);
			
			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask to the user for saving the changes
			int returnValueSaveFile = _fileChooser.showSaveDialog(_fileChooser);

			// Gets the selected file
			File selectedFile = _fileChooser.getSelectedFile();

			// Ask the user for saving it
			if (returnValueSaveFile == JFileChooser.APPROVE_OPTION) {

				// If exists
				if (selectedFile.exists()
						&& _fileChooser.getDialogType() == JFileChooser.SAVE_DIALOG) {

					// Ask if are you sure about the operation
					int returnValueAreYouSure = JOptionPane.showConfirmDialog(
							null, AcideLanguageManager.getInstance()
									.getLabels().getString("s954"),
							AcideLanguageManager.getInstance().getLabels()
									.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					// If it is ok
					if (returnValueAreYouSure == JOptionPane.YES_OPTION) {

						// Gets the ACIDE - A Configurable IDE last opened file
						// directory
						absoluteFilePath = _fileChooser.getSelectedFile()
								.getAbsolutePath();

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s307")
										+ absoluteFilePath);

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedFileDirectory",
								absoluteFilePath);
					}

					// If it is no
					if (returnValueAreYouSure == JOptionPane.NO_OPTION) {

						// If it is the new project file
						if (isNewProjectFile) {

							// Removes the tab from the file editor
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.remove(AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex());

							// Validates the changes in the file editor
							AcideMainWindow.getInstance()
									.getFileEditorManager().getTabbedPane()
									.validate();
						}
					}
				}
			} else if (returnValueSaveFile == JFileChooser.CANCEL_OPTION) {

				// Cancel selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s308"));

				// If it is the new project file
				if (isNewProjectFile) {

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

		return absoluteFilePath;
	}

	/**
	 * Loads the text file content into a string.
	 * 
	 * @param fileName
	 *            file name.
	 * @return the file content.
	 * @throws IOException.
	 */
	public String load(String fileName) {

		try {

			// Creates the buffered reader with the file
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(fileName)));
			StringBuffer stringBuffer = new StringBuffer("");
			String string;

			// Reads the file content
			while (true) {
				string = reader.readLine();
				if (string == null)
					break;
				stringBuffer.append(string + "\n");
			}
			reader.close();

			// Calls to the garbage collector
			System.gc();

			// Returns the file content
			return stringBuffer.toString();

		} catch (IOException exception) {

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s309")
							+ fileName);
			// exception.printStackTrace();

			return null;
		}
	}

	/**
	 * Saves the file content given as a parameter into a file given as a
	 * parameter as well.
	 * 
	 * @param file
	 *            file to save in.
	 * @param fileContent
	 *            file content to store into the file.
	 * 
	 * @return true if the operation was succeed and false in other case.
	 */
	public boolean write(String file, String fileContent) {

		try {

			// Creates the print writer with the file
			PrintWriter printerWriter = new PrintWriter(new BufferedWriter(
					new FileWriter(file)));

			// Prints the file content into it
			printerWriter.print(fileContent);

			// Closes the print writer
			printerWriter.close();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s310")
							+ file);

			return true;
		} catch (IOException exception) {

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s311")
							+ file);
			exception.printStackTrace();

			return false;
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file manager file chooser.
	 * 
	 * @return the ACIDE - A Configurable IDE file manager file chooser.
	 */
	public JFileChooser getFileChooser() {
		return _fileChooser;
	}
}
