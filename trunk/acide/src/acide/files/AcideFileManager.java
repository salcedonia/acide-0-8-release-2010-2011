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

import javax.swing.*;

import acide.resources.AcideResourceManager;

import java.io.*;

import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
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
	 * Asks to the user for some files with the options that corresponds.
	 * 
	 * @param fileOperation
	 *            file operation for an opening or saving dialog.
	 * @param fileTarget
	 *            file target to get the current directory and for updating the
	 *            two global variables that store the last file or project
	 *            directory opened in ACIDE - A Configurable IDE.
	 * @param fileType
	 *            file type of the file to be selected, it means, a file or a
	 *            directory.
	 * @param currentDirectory
	 *            if it is "" indicates that the current directory has to be
	 *            taken from the global variables.
	 * @param filter
	 *            filter to apply.
	 * 
	 * @return the absolute paths of the selected files.
	 */
	public String[] askForFiles(AcideFileOperation fileOperation,
			AcideFileTarget fileTarget, AcideFileType fileType,
			String currentDirectory, AcideFileExtensionFilterManager filter) {

		// Removes the previous filter
		_fileChooser.removeChoosableFileFilter(_fileChooser.getFileFilter());

		// If there is a defined filter
		if (filter != null)

			// Sets the filter
			_fileChooser.addChoosableFileFilter(filter);

		File[] selectedFiles = null;
		String absolutePaths[] = null;

		try {

			// If there is no a defined current directory
			if (currentDirectory.equals("")) {

				switch (fileTarget) {

				case FILES:

					// Gets the ACIDE - A Configurable IDE last opened file
					// directory
					currentDirectory = AcideResourceManager.getInstance()
							.getProperty("lastOpenedFileDirectory");
					break;
				case PROJECTS:
					// Gets the ACIDE - A Configurable IDE last opened project
					// directory
					currentDirectory = AcideResourceManager.getInstance()
							.getProperty("lastOpenedProjectDirectory");
					break;
				}
			}

			// Sets the current directory to the current directory
			_fileChooser.setCurrentDirectory(new File(currentDirectory));

			// Clears the previous selected files
			_fileChooser.setSelectedFile(new File(""));

			// Enables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(true);

			switch (fileType) {

			case FILE:
				// Sets only files
				_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				break;

			case DIRECTORY:
				// Sets only directories
				_fileChooser
						.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				break;
			}

			int returnValue = 0;

			switch (fileOperation) {
			case OPEN:

				// Ask for the file to open to the user
				returnValue = _fileChooser.showOpenDialog(null);
				break;
			case SAVE:

				// Ask for the file to save to the user
				returnValue = _fileChooser.showSaveDialog(null);
				break;
			}

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

					switch (fileTarget) {

					case FILES:

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance()
								.setProperty("lastOpenedFileDirectory",
										absolutePaths[index]);
						break;
					case PROJECTS:

						// Updates the ACIDE - A Configurable IDE last opened
						// project
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedProjectDirectory",
								absolutePaths[index]);
						break;
					}
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
	 * Asks to the user for one file with the options that corresponds.
	 * 
	 * @param fileOperation
	 *            file operation for an opening or saving dialog.
	 * @param fileTarget
	 *            file target to get the current directory and for updating the
	 *            two global variables that store the last file or project
	 *            directory opened in ACIDE - A Configurable IDE.
	 * @param fileType
	 *            file type of the file to be selected, it means, a file or a
	 *            directory.
	 * @param currentDirectory
	 *            if it is "" indicates that the current directory has to be
	 *            taken from the global variables.
	 * @param filter
	 *            filter to apply.
	 * @return the absolute path of the selected file.
	 */
	public String askForFile(AcideFileOperation fileOperation,
			AcideFileTarget fileTarget, AcideFileType fileType,
			String currentDirectory, AcideFileExtensionFilterManager filter) {

		// Removes the previous filter
		_fileChooser.removeChoosableFileFilter(_fileChooser.getFileFilter());

		// If there is a defined filter
		if (filter != null)

			// Sets the filter
			_fileChooser.addChoosableFileFilter(filter);
		
		String absolutePath = null;

		try {

			// If there is no a defined current directory
			if (currentDirectory.equals("")) {

				switch (fileTarget) {

				case FILES:

					// Gets the ACIDE - A Configurable IDE last opened file
					// directory
					currentDirectory = AcideResourceManager.getInstance()
							.getProperty("lastOpenedFileDirectory");
					break;
				case PROJECTS:
					// Gets the ACIDE - A Configurable IDE last opened project
					// directory
					currentDirectory = AcideResourceManager.getInstance()
							.getProperty("lastOpenedProjectDirectory");
					break;
				}
			}

			// Sets the current directory
			_fileChooser.setCurrentDirectory(new File(currentDirectory));

			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Clears the previous selected files
			_fileChooser.setSelectedFile(new File(""));

			switch (fileType) {

			case FILE:
				// Sets only files
				_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				break;

			case DIRECTORY:
				// Sets only directories
				_fileChooser
						.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				break;
			}

			int returnValue = 0;

			switch (fileOperation) {
			case OPEN:

				// Ask for the file to open to the user
				returnValue = _fileChooser.showOpenDialog(null);
				break;
			case SAVE:

				// Ask for the file to save to the user
				returnValue = _fileChooser.showSaveDialog(null);
				break;
			}

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file absolute path
				absolutePath = _fileChooser.getSelectedFile().getAbsolutePath();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s300")
								+ absolutePath);

				switch (fileTarget) {

				case FILES:

					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedFileDirectory", absolutePath);
					break;
				case PROJECTS:

					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedProjectDirectory", absolutePath);
					break;
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

		return absolutePath;
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
