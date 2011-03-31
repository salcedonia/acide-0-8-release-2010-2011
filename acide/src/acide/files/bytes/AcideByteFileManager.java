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
package acide.files.bytes;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE byte file manager.
 * 
 * @version 0.8
 */
public class AcideByteFileManager {

	/**
	 * ACIDE - A Configurable IDE byte file manager unique class instance.
	 */
	private static AcideByteFileManager _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE byte file manager unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE byte file manager unique class
	 *         instance.
	 */
	public static AcideByteFileManager getInstance() {

		if (_instance == null)
			_instance = new AcideByteFileManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE byte file manager.
	 */
	public AcideByteFileManager() {

	}

	/**
	 * Copies the content from the source file to the target file.
	 * 
	 * @param sourcePath
	 *            source file path.
	 * @param targetPath
	 *            target file path.
	 * @throws IOException.
	 */
	public void copy(String sourcePath, String targetPath) throws IOException {

		// Gets the source file
		File sourceFile = new File(sourcePath);
		
		// Gets the target file
		File targetFile = new File(targetPath);
		
		FileInputStream fileInputStream = null;
		FileOutputStream fileOutputStream = null;

		try {
			
			// Creates the file input stream
			fileInputStream = new FileInputStream(sourceFile);
			
			// Creates the file output stream
			fileOutputStream = new FileOutputStream(targetFile);
			
			byte[] buffer = new byte[4096];
			int bytesRead;
			while ((bytesRead = fileInputStream.read(buffer)) != -1)
				fileOutputStream.write(buffer, 0, bytesRead); 
		} finally {
			
			if (fileInputStream != null)
				try {
					
					// Closes the file input stream
					fileInputStream.close();
				} catch (IOException exception) {

					// Displays an error message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s265")
							+ sourcePath, AcideLanguageManager.getInstance()
							.getLabels().getString("s266"),
							JOptionPane.ERROR_MESSAGE);

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
				}

			if (fileOutputStream != null)
				try {
					
					// Closes the file output stream
					fileOutputStream.close();
				} catch (IOException exception) {

					// Displays an error message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s267")
							+ targetPath, AcideLanguageManager.getInstance()
							.getLabels().getString("268"),
							JOptionPane.ERROR_MESSAGE);

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
				}
		}
	}
	
	/**
	 * Reallocates the source path into the target path.
	 * 
	 * @param source
	 *            source path.
	 * @param target
	 *            target path.
	 * 
	 * @return true if the operation succeed and false in other case.
	 */
	public boolean reallocateFile(String source, String target) {

		File sourceFile = new File(source);

		// Avoids infinite loops
		if (!sourceFile.exists())
			return false;

		File targetFile = new File(target);
		if (targetFile.exists())
			targetFile.delete();
		try {
			targetFile.createNewFile();
		} catch (IOException exception) {

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s211"),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s210"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s210")
							+ ": "
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s211"));
			exception.printStackTrace();
		}

		boolean saved = false;
		try {

			// Copies the binary files
			AcideByteFileManager.getInstance().copy(source, target);

			// They have been copied successfully
			saved = true;
		} catch (IOException exception) {

			// They have not been copied succesfully
			saved = false;

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s945"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// If they have been copied successfully
		if (saved)

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s212")
							+ target);
		else

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s213")
							+ target);

		// Deletes the source file
		boolean deleted = sourceFile.delete();

		// If it could been deleted
		if (deleted)

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s214")
							+ source);
		else

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s215")
							+ source);

		return (saved && deleted);
	}
}
