package operations.factory;

import es.text.TextFile;
 
/************************************************************************																
 * Builds the Input/Output objects of ACIDE - A Configurable IDE											
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
public class IOFactory {
	
	/**
	 * Class instance
	 */
	private static IOFactory _instance;

	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static IOFactory getInstance() {
		if (_instance == null)
			_instance = new IOFactory();
		return _instance;
	}

	/**
	 * Builds a text file
	 * 
	 * @return the new text file
	 */
	public TextFile buildFile() {
		return new TextFile();
	}
}
