package es.configuration.menu.exception;

/************************************************************************
 * Incorrect menu configuration file format exception of ACIDE - A Configurable
 * IDE.
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
 * @see Exception
 ***********************************************************************/
public class IncorrectMenuConfigurationFileFormatException extends Exception {

	/**
	 * Incorrect menu configuration file format serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new incorrect menu configuration file format exception.
	 * 
	 * @param message
	 *            error message.
	 */
	public IncorrectMenuConfigurationFileFormatException(String message) {
		super(message);
	}
}
