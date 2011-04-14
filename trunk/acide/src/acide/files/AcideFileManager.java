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

import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.*;

import acide.resources.AcideResourceManager;

import java.io.*;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file manager.
 * 
 * Handles the saving and loading operations over text files, and ask to the
 * user about the files or directories to select in dialogs.
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
	 * Returns absolute path selected by the user in a file chooser dialog.
	 * 
	 * @return the absolute path selected by the user in a file chooser dialog.
	 */
	public String askAbsolutePath() {

		String absolutePath = null;
		String defaultPath = null;
		File file = null;

		try {

			// Gets the default path for the start point
			defaultPath = AcideResourceManager.getInstance().getProperty(
					"defaultPath");
			file = new File(defaultPath);

			// Sets the title of the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Ask for the file to the user
		int returnValue = _fileChooser.showOpenDialog(null);

		// If OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the absolute path
			absolutePath = _fileChooser.getSelectedFile().getAbsolutePath();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s300")
							+ absolutePath);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("defaultPath",
					absolutePath);

		} else if (returnValue == JFileChooser.CANCEL_OPTION) {

			_fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s302"));
		}
		return absolutePath;
	}

	/**
	 * Returns the array which contains the absolute paths of the file or files
	 * selected by the user in a file chooser dialog.
	 * 
	 * @return the array which contains the absolute paths of the file or files
	 *         selected by the user in a file chooser dialog.
	 */
	public String[] askMultipleAbsolutePath() {

		File[] selectedFiles;

		String absolutePaths[] = null;
		String text = null;
		File file = null;

		try {

			// Gets the default path for the start point
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);

			// Sets the title for the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(file.getParentFile());

			// Enables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(true);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Ask for the file to the user
		int returnValue = _fileChooser.showOpenDialog(null);

		// If OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the selected files
			selectedFiles = _fileChooser.getSelectedFiles();

			// Creates the string array
			absolutePaths = new String[selectedFiles.length];

			// Stores the selected absolute paths in the string array
			for (int index = 0; index < selectedFiles.length; index++) {

				// Gets the absolute path
				absolutePaths[index] = selectedFiles[index].getAbsolutePath();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s300")
								+ absolutePaths[index]);

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty("defaultPath",
						absolutePaths[index]);
			}

		} else if (returnValue == JFileChooser.CANCEL_OPTION) {

			_fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s302"));
		}
		return absolutePaths;
	}

	/**
	 * Returns the absolute path of a selected directory which has been selected
	 * by the user in a file chooser dialog.
	 * 
	 * @return the absolute path of a selected directory which has been selected
	 *         by the user in a file chooser dialog.
	 */
	public String askDirectoryAbsolutePath() {

		String path = " ";
		String text = null;
		File file = null;

		try {

			// Gets the default path
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);

			// Sets the current directory to the default path parent directory
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Only directories
		_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		int returnValue = _fileChooser.showOpenDialog(null);

		// If OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the absolute path of the directory
			path = _fileChooser.getSelectedFile().getAbsolutePath();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s303")
							+ path);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("defaultPath", path);

		} else if (returnValue == JFileChooser.CANCEL_OPTION) {

			_fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s304"));
		}
		return path;
	}

	/**
	 * Returns the absolute path of a selected directory or file which has been
	 * selected by the user in a file chooser dialog which only admits the file
	 * extensions specified by the parameter filter.
	 * 
	 * @param filter
	 *            file extension filter.
	 * 
	 * @return the absolute path of a selected directory or file which has been
	 *         selected by the user in a file chooser dialog which only admits
	 *         the file extensions specified by the parameter filter.
	 */
	public String askAbsolutePathWithFilter(
			AcideTextFileExtensionFilterManager filter) {

		JFileChooser fileChooser = new JFileChooser();
		String selectedFileAbsolutePath = " ";
		String text = null;
		File file = null;
		try {

			// Gets the default path
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);

			// Sets the selected filter
			fileChooser.setFileFilter(filter);

			// Sets the current directory to the default path parent directory
			fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Asks to the user
		int returnValue = fileChooser.showOpenDialog(null);

		// If OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the selected file absolute path
			selectedFileAbsolutePath = fileChooser.getSelectedFile()
					.getAbsolutePath();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s305")
							+ selectedFileAbsolutePath);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("defaultPath",
					selectedFileAbsolutePath);
		} else if (returnValue == JFileChooser.CANCEL_OPTION) {

			fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s306"));
		}
		return selectedFileAbsolutePath;
	}

	/**
	 * Writes on a text file.
	 * 
	 * @param isNewProjectFile
	 *            flag that indicates if it is the new project file or not for
	 *            the new project file menu item.
	 * 
	 * @return the absolute file path.
	 */
	public String askSavingFileEditorFile(boolean isNewProjectFile) {

		String absoluteFilePath = " ";
		String defaultPath = null;
		File file = null;

		try {

			// Gets the default path
			defaultPath = AcideResourceManager.getInstance().getProperty(
					"defaultPath");
			file = new File(defaultPath);

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(file);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		boolean isApproved = true;

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
				int returnValueAreYouSure = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s954"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If YES
				if (returnValueAreYouSure == JOptionPane.YES_OPTION) {
					isApproved = true;
				}

				// If NO
				if (returnValueAreYouSure == JOptionPane.NO_OPTION) {

					isApproved = false;

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
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().validate();
					}
				}
			} else
				isApproved = true;
		} else if (returnValueSaveFile == JFileChooser.CANCEL_OPTION) {

			// Cancel selection
			_fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s308"));

			isApproved = false;

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

		// If YES
		if (isApproved) {

			// Gets the absolute path
			absoluteFilePath = _fileChooser.getSelectedFile().getAbsolutePath();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s307")
							+ absoluteFilePath);

			// Updates the RESOURCE MANAGER default path
			AcideResourceManager.getInstance().setProperty("defaultPath",
					absoluteFilePath);
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
