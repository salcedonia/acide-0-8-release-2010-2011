package resources.exception;

import java.util.ResourceBundle;

import language.AcideLanguage;
import operations.log.AcideLog;

/************************************************************************																
 * Missed property exception of ACIDE - A Configurable IDE.											
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
 * @see Exception																													
 ***********************************************************************/
public class MissedPropertyException extends Exception {

	/**
	 * Missed property exception class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Parameter which raised the exception.
	 */
	private String _parameter;
	/**
	 * Labels to display in the selected language.
	 */
	private static ResourceBundle _labels = AcideLanguage.getInstance().getLabels();
	
	/**
	 * Creates a new missed property exception.
	 * 
	 * @param parameter parameter that raised the exception.
	 */
	public MissedPropertyException(String parameter) {
		
		// Creates the exception
		super(_labels.getString("s426") + parameter + "'");
		
		// Updates the log
		AcideLog.getLog().info(_labels.getString("s427") + parameter + "'");
		
		// Stores the parameter which raised the exception
		_parameter = parameter;
	}

	/**
	 * Returns the parameter which raised the exception.
	 * 
	 * @return the parameter which raised the exception.
	 */
	public String getParameter() {
		return _parameter;
	}
}

