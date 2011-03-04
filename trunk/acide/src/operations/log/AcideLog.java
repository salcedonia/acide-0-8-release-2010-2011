/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package operations.log;

import java.io.IOException;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/**
 * ACIDE - A Configurable IDE Log.
 * 
 * @version 0.8
 * @see Logger
 */
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
