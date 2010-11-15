package es.text;

import java.io.File;
import javax.swing.filechooser.*;

/************************************************************************																
 * Handles the file extensions of of ACIDE - A Configurable IDE											
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
public class ExtensionFilter extends FileFilter {

	/**
	 * ACIDE - A Configurable IDE valid extensions 
	 */
	private String[] _extensions;
	/**
	 * Extension description
	 */
	private String _description;

	/**
	 * Class constructor
	 * 
	 * @param extension
	 *            new extension
	 */
	public ExtensionFilter(String extension) {

		this(new String[] { extension }, null);
	}

	/**
	 * Class constructor
	 * 
	 * @param extensions
	 *            extensions to set
	 * @param description
	 *            description to set
	 */
	public ExtensionFilter(String[] extensions, String description) {
		_extensions = new String[extensions.length];
		for (int i = extensions.length - 1; i >= 0; i--) {
			_extensions[i] = extensions[i].toLowerCase();
		}
		_description = (description == null ? extensions[0] + " files"
				: description);
	}

	/**
	 * Returns true if the file contains a valid extension and false in other
	 * case
	 * 
	 * @param file
	 *            file to check
	 */
	public boolean accept(File file) {

		if (file.isDirectory()) {
			return true;
		}
		String name = file.getName().toLowerCase();

		for (int i = _extensions.length - 1; i >= 0; i--) {
			if (name.endsWith(_extensions[i])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the extension description
	 * 
	 * @return the extension description
	 */
	public String getDescription() {
		return _description;
	}
}
