package es.bytes;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import operations.log.AcideLog;
import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************																
 * Handles the byte files of ACIDE - A Configurable IDE.										
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8																														
 ***********************************************************************/
public class ByteFile {

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
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
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
