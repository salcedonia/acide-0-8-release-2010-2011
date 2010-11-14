package es.text;

import java.io.Serializable;
import java.util.StringTokenizer;

import utils.ObjectList;

/************************************************************************																
 * Handles the valid extensions of the files for ACIDE - A Configurable IDE											
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
 * @see Serializable																														
 ***********************************************************************/
public class ValidExtensions implements Serializable {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Extension list
	 */
	private ObjectList _list;
	/**
	 * Class instance
	 */
	private static ValidExtensions _instance;

	/**
	 * Class constructor
	 */
	public ValidExtensions() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Loads a new valid extension set
	 * 
	 * @param extensions
	 *            new value to set
	 */
	public void load(ValidExtensions extensions) {
		_instance = extensions;
	}

	/**
	 * Returns the extension a the position <b>POS</b> in the list
	 * 
	 * @param pos
	 *            position to get
	 * @return the extension a the position <b>POS</b> in the list
	 */
	public Object getExtensionAt(int pos) {
		return _list.getObjectAt(pos);
	}

	/**
	 * Sets the new extension for the value at the position <b>POS</b> at the
	 * list
	 * 
	 * @param element
	 *            new value to set
	 */
	public void setExtensionAt(Object element) {
		_list.insert(_list.size(), element);
	}

	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static ValidExtensions getInstance() {
		if (_instance == null)
			_instance = new ValidExtensions();
		return _instance;
	}

	/**
	 * Checks if the extension given as a parameter is a valid or invalid
	 * extension, returning true in for the first case and false in another
	 * 
	 * @param extension
	 *            extension to check
	 * 
	 * @return true if it is valid extension and false in other case
	 */
	public boolean isValidExtension(String extension) {

		for (int i = 0; i < _list.size(); i++)
			if (extension.endsWith((String) _list.getObjectAt(i)))
				return true;

		return false;
	}

	/**
	 * Converts the list of extensions into different tokens and save them into a
	 * string separated by commas
	 * 
	 * @param string
	 *            string to save
	 */
	public void tokenizeExtensions(String string) {

		StringTokenizer tokens = new StringTokenizer(string, ",");

		for (int i = 0; i < tokens.countTokens(); i++)
			setExtensionAt(tokens.nextToken());
	}
}
