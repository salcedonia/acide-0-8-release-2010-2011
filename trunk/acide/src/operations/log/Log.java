package operations.log;

import java.io.IOException;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/**
 * Handle the log of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class Log {

	/**
	 * Log of the application.
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
						".//log/logfile.txt");
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
	 * Returns the log of the application.
	 * 
	 * @return The log of the application.
	 */
	public static Logger getLog() {
		return _log;
	}

	/**
	 * Set a new value to the log of the application.
	 * 
	 * @param name
	 *            new value to set.
	 * 
	 * @return The new log.
	 */
	public static Logger getLog(String name) {
		return Logger.getLogger(name);
	}

}
