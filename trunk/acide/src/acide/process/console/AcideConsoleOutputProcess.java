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
package acide.process.console;

import acide.gui.consolePanel.AcideConsolePanel;

import java.io.*;

import acide.log.AcideLog;

/**
 * <p>
 * Handles the ACIDE - A Configurable IDE output process thread.
 * </p>
 * <p>
 * Prints the output of the ACIDE - A Configurable IDE console panel process
 * into the text pane of the ACIDE - A Configurable IDE console panel.
 * </p>
 * 
 * @version 0.8
 * @see Thread
 */
class AcideConsoleOutputProcess extends Thread {

	/**
	 * ACIDE - A Configurable IDE input stream.
	 */
	private InputStream _inputStream;
	/**
	 * ACIDE - A Configurable IDE console panel.
	 */
	private AcideConsolePanel _consolePanel;
	/**
	 * ACIDE - A Configurable IDE
	 */
	private StringBuffer _stringBuffer;

	/**
	 * Creates a new ACIDE - A Configurable IDE console process thread.
	 * 
	 * @param inputStream
	 *            input stream.
	 * @param consolePanel
	 *            ACIDE - A Configurable IDE console panel.
	 * @see InputStream
	 * @see AcideConsolePanel
	 */
	public AcideConsoleOutputProcess(InputStream inputStream,
			AcideConsolePanel consolePanel) {

		// Stores the input stream
		_inputStream = inputStream;

		// Stores the console panel
		_consolePanel = consolePanel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Thread#run()
	 */
	@Override
	public synchronized void run() {

		try {

			// Creates the input stream reader
			InputStreamReader inputStreamReader = new InputStreamReader(
					_inputStream);

			// Creates the buffered reader
			BufferedReader bufferedReader = new BufferedReader(
					inputStreamReader);

			// Creates the string buffer
			_stringBuffer = new StringBuffer(1024);

			int character = 0;

			while ((character = bufferedReader.read()) != -1) {

				// If it is not the carriage return
				if (character != 13)
					_stringBuffer.append((char) character);

				// When the buffer reader is empty
				if (!bufferedReader.ready()) {

					// Adds the text to the console panel
					_consolePanel.addText(_stringBuffer.toString());

					// Clears the buffer
					_stringBuffer.delete(0, _stringBuffer.length());
				}
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
