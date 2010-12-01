package operations.log;

import java.io.IOException;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/************************************************************************
 * ACIDE - A Configurable IDE Log.
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
 * @see Logger
 ***********************************************************************/
public class AcideLog {

	/**
	 * ACIDE - A Configurable IDE Log.
	 */
	private static Logger _log;

	/**
	 * Starts the log of the application.
	 */
	public static void startLog() {

		if (_log == null) {

			_log = Logger.getLogger("LogAcide");
			_log.setLevel(Level.ALL);

			try {
				PatternLayout p = new PatternLayout(
						"%d{date} [%t] %-5p %c - %m\n");
				RollingFileAppender roll = new RollingFileAppender(p,
						"./log/logfile.txt");
				roll.setAppend(false);
				roll.setMaxBackupIndex(2);
				roll.setMaxFileSize("100KB");
				roll.rollOver();
				_log.addAppender(roll);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Returns ACIDE - A Configurable IDE Log.
	 * 
	 * @return ACIDE - A Configurable IDE Log.
	 * @see Logger
	 */
	public static Logger getLog() {
		return _log;
	}

	/**
	 * Sets a new value to ACIDE - A Configurable IDE Log.
	 * 
	 * @param name
	 *            new value to set.
	 * 
	 * @return new ACIDE - A Configurable IDE Log.
	 * @see Logger
	 */
	public static Logger getLog(String name) {
		return Logger.getLogger(name);
	}
}
