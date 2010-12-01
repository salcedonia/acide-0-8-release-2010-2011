package es.text;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.swing.filechooser.FileFilter;

import operations.log.AcideLog;

import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************																
 * Handle the text file filters of ACIDE - A Configurable IDE.											
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
 * @see FileFilter																													
 ***********************************************************************/
public class TextFileFilter extends FileFilter {

	/**
	 * File description.
	 */
	private String _description;
	/**
	 * File extensions.
	 */
	private List<String> _extensions;

	/**
	 * Creates a new text file filter with a new description given
	 * as a parameter.
	 * 
	 * @param description new description for the text file filter.
	 */
	public TextFileFilter(String description) {
		
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
		
		if (description == null)
			throw new NullPointerException(labels.getString("s312"));
		
		_description = description;
		_extensions = new ArrayList<String>();
	}

	/**
	 * Returns true if the file contains a valid extension and false in other case.
	 * 
	 * @param file file to check.
	 */
	public boolean accept(File file) {
		if (file.isDirectory() || _extensions.size() == 0) {
			return true;
		}
		String fileName = file.getName().toLowerCase();
		for (String extension : _extensions) {
			if (fileName.endsWith(extension)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the description.
	 * 
	 * @return the description.
	 */
	public String getDescription() {
		
		StringBuffer buffer = new StringBuffer(_description);
		buffer.append(" (");
		
		for (String extension : _extensions)
			buffer.append(extension).append(" ");
		
		return buffer.append(")").toString();
	}

	/**
	 * Sets a new value for the description
	 * 
	 * @param description new value to set
	 */
	public void setDescription(String description) {
		
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
		
		if (description == null)
			throw new NullPointerException(labels.getString("s313"));
		
		_description = description;
	}

	/**
	 * Adds a new extension to the list.
	 * 
	 * @param extension new extension to add.
	 */
	public void addExtension(String extension) {
		
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
		
		if (extension == null)
			throw new NullPointerException(labels.getString("s314"));
	
		_extensions.add(extension.toLowerCase());
	}

	/** 
	 * Removes an extension from the list of available extensions.
	 * 
	 * @param extension extension to remove.
	 */
	public void removeExtension(String extension) {
		_extensions.remove(extension);
	}

	/**
	 * Deletes all the extensions.
	 */
	public void clearExtensions() {
		_extensions.clear();
	}

	/**
	 * Returns the list of extensions
	 * 
	 * @return the list of extensions
	 */
	public List<String> getExtensions() {
		return _extensions;
	}
}
