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
package es.text;

import gui.mainWindow.MainWindow;

import javax.swing.*;

import operations.log.AcideLog;

import resources.AcideResourceManager;

import java.io.*;
import java.util.ResourceBundle;

import language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE text file.
 * 
 * @version 0.8
 */
public class AcideTextFile {

	/**
	 * ACIDE - A Configurable IDE text file file chooser.
	 */
	private JFileChooser _fileChooser;

	/**
	 * Creates a new ACIDE - A Configurable IDE text file.
	 */
	public AcideTextFile() {
		_fileChooser = new JFileChooser();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text file absolute path.
	 * 
	 * @return the ACIDE - A Configurable IDE text file absolute path.
	 */
	public String askAbsolutePath() {

		String absolutePath = null;
		String text = null;
		File file = null;

		try {
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Ask for the file to the user
		int value = _fileChooser.showOpenDialog(null);

		// If selects ok
		if (value == JFileChooser.APPROVE_OPTION) {

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

		} else if (value == JFileChooser.CANCEL_OPTION) {

			_fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s302"));
		}
		return absolutePath;

	}

	/**
	 * Returns the text file path selected in a file chooser.
	 * 
	 * @return the text file path selected in a file chooser.
	 */
	public String readPath() {

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

		String path = " ";
		String text = null;
		File file = null;

		try {
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {
			path = _fileChooser.getSelectedFile().getAbsolutePath();

			// Updates the log
			AcideLog.getLog().info(labels.getString("s303") + path);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("defaultPath", path);
		} else if (value == JFileChooser.CANCEL_OPTION) {

			_fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(labels.getString("s304"));
		}
		return path;
	}

	/**
	 * Returns the text file name.
	 * 
	 * @param filter
	 *            filter for the text files.
	 * 
	 * @return the text file name.
	 */
	public String read(TextFileFilter filter) {

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

		JFileChooser fileChooser = new JFileChooser();
		String fileName = " ";
		String text = null;
		File file = null;
		try {
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		int value = fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = fileChooser.getSelectedFile().getAbsolutePath();

			// Updates the log
			AcideLog.getLog().info(labels.getString("s305") + fileName);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("defaultPath",
					fileName);
		} else if (value == JFileChooser.CANCEL_OPTION) {

			fileChooser.cancelSelection();

			// Updates the log
			AcideLog.getLog().info(labels.getString("s306"));
		}
		return fileName;
	}

	/**
	 * Writes on a text file.
	 * 
	 * @return the absolute file path.
	 */
	public String askSavingFileEditorFile() {

		String absoluteFilePath = " ";
		String text = null;
		File file = null;

		try {
			text = AcideResourceManager.getInstance()
					.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		boolean isApproved = true;

		// Ask to the user for saving the changes
		int returnValueSaveFile = _fileChooser.showSaveDialog(_fileChooser);
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

					if (MainWindow.getInstance().getMenu().isNPF()) {

						// Removes the tab from the file editor
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(MainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// Validates the changes in the file editor
						MainWindow.getInstance().getFileEditorManager()
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

			if (MainWindow.getInstance().getMenu().isNPF()) {

				// Removes the tab in the file editor
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.remove(MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex());

				// Validates the changes in the file editor
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.validate();
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

			// Updates the RESOURCE MANAGER
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
			System.gc();

			// Returns the file content
			return stringBuffer.toString();

		} catch (IOException exception) {

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s309")
							+ fileName);
			exception.printStackTrace();

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
	 * Returns the ACIDE - A Configurable IDE text file file chooser.
	 * 
	 * @return the ACIDE - A Configurable IDE text file file chooser.
	 */
	public JFileChooser getFileChooser() {
		return _fileChooser;
	}
}
