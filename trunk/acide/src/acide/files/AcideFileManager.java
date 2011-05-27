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
	 * Loads the text file content into a string.
	 * 
	 * @param fileName
	 *            file name.
	 * @return the file content.
	 * @throws IOException.
	 */
	public String load(String fileName) {

		FileReader reader = null;

		// Creates the string buffer
		StringBuffer stringBuffer = new StringBuffer();
		try {

			// Creates the file reader
			reader = new FileReader(fileName);

			int character;

			while ((character = reader.read()) != -1) {
				stringBuffer.append((char) character);
			}

			// Closes the reader
			reader.close();

		} catch (IOException ex) {

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s309")
							+ fileName);

			return null;
		}

		// Calls to the garbage collector
		System.gc();

		// Returns the file content
		return stringBuffer.toString();
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
}
