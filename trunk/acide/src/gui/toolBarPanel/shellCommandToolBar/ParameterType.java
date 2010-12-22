package gui.toolBarPanel.shellCommandToolBar;

import java.util.ResourceBundle;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;


/************************************************************************
 * Defines the parameter types for the shell tool bar commands of 
 * ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public enum ParameterType {

	/**
	 * No parameter.
	 */
	NONE,
	/**
	 * Type text.
	 */
	TEXT,
	/**
	 * Type file.
	 */
	FILE,
	/**
	 * Type directory.
	 */
	DIRECTORY;
	
	/**
	 * Parse a parameter type given as a parameter from string to ParameterType.
	 *  
	 * @param parameterTypeString parameter to parse.
	 * @return the parsed value.
	 */
	public static ParameterType fromStringToEnum(String parameterTypeString){
		
		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		if(parameterTypeString.matches(labels.getString("s1005")))
			return NONE;
		if(parameterTypeString.matches(labels.getString("s1006")))
			return TEXT;
		if(parameterTypeString.matches(labels.getString("s1007")))
			return FILE;
		if(parameterTypeString.matches(labels.getString("s1008")))
			return DIRECTORY;
		
		return null;
	}
}
