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
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import acide.resources.AcideResourceManager;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**									
 * Handles the byte files of ACIDE - A Configurable IDE.
 * 										
 * @version 0.8																														
 */
public class AcideByteFile {

	/**
	 * Copies the content from one file to another.
	 * 
	 * @param fromFileName origin file.
	 * @param toFileName destination file.
	 * @throws IOException.
	 */
	public static void copy(String fromFileName, String toFileName)
			throws IOException {
				
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
		
		File fromFile = new File(fromFileName);
		File toFile = new File(toFileName);
		FileInputStream from = null;
		FileOutputStream to = null;
		
		try {
			from = new FileInputStream(fromFile);
			to = new FileOutputStream(toFile);
			byte[] buffer = new byte[4096];
			int bytesRead;
			while ((bytesRead = from.read(buffer)) != -1)
				to.write(buffer, 0, bytesRead); // write
		} finally {
			if (from != null)
				try {
					from.close();
				} catch (IOException exception) {
					
					// Error message
					JOptionPane
							.showMessageDialog(null, labels.getString("s265")
									+ fromFileName, labels.getString("s266"),
									JOptionPane.ERROR_MESSAGE);
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
				}
				
			if (to != null)
				try {
					to.close();
				} catch (IOException exception) {
					
					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s267") + toFileName,
							labels.getString("268"), JOptionPane.ERROR_MESSAGE);
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
				}
		}
	}
}
