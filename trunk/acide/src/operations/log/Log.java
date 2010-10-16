package operations.log;

import java.io.IOException;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/**
 * 
 */
public class Log {

	/**
	 * 
	 */
	private static Logger _log;

	/**
	 * 
	 */
	public static void startLog() {
		
		if (_log == null) {
			_log = Logger.getLogger("LogAcide");
			_log.setLevel(Level.ALL);
			
			try {
				PatternLayout p = new PatternLayout("%d{date} [%t] %-5p %c - %m\n");
				RollingFileAppender roll = new RollingFileAppender(p,".//log/logfile.txt");
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
	 *
	 * 
	 * @return 
	 */
	public static Logger getLog() {
		return _log;
	}

	/**
	 * @param name
	 *            
	 * @return
	 */
	public static Logger getLog(String name) {
		return Logger.getLogger(name);
	}

}
