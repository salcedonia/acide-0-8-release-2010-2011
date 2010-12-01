package es.configuration.grammar;

/************************************************************************																
 * Grammar configuration of ACIDE - A Configurable IDE.											
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
public class GrammarConfiguration {

	/**
	 * Grammar path.
	 */
	public static String _path;

	/**
	 * Class constructor.
	 */
	public GrammarConfiguration() {

	}

	/**
	 * Class constructor.
	 * 
	 * @param path absolute configuration file path.
	 */
	public GrammarConfiguration(String path) {	
		_path = path;
	}

	/**
	 * Returns the grammar path.
	 * 
	 * @return the grammar path.
	 */
	public static String getPath() {
		return _path;
	}

	/**
	 * Sets a new value to the grammar path.
	 * 
	 * @param path new value to set.
	 */
	public static void setPath(String path) {	
		_path = path;
	}
}
