/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.process.execution;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import acide.log.AcideLog;
import acide.process.gui.AcideProgressWindow;

/**
 * <p>
 * Handles the ACIDE - A Configurable IDE project execution output process
 * thread.
 * </p>
 * <p>
 * Prints the output of the project execution process into the ACIDE - A
 * Configurable IDE progress window.
 * </p>
 * 
 * @version 0.8
 * @see Thread
 */
public class AcideProjectExecutionOutputProcess extends Thread {

	/**
	 * ACIDE - A Configurable IDE input stream.
	 */
	private InputStream _inputStream;
	/**
	 * ACIDE - A Configurable IDE progress window.
	 */
	private AcideProgressWindow _progressWindow;
	/**
	 * ACIDE - A Configurable IDE
	 */
	private StringBuffer _stringBuffer;

	/**
	 * Creates a new ACIDE - A Configurable IDE project execution output process
	 * thread.
	 * 
	 * @param inputStream
	 *            input stream.
	 * @param progressWindow
	 *            ACIDE - A Configurable IDE progress window.
	 * @see InputStream
	 * @see AcideProgressWindow
	 */
	public AcideProjectExecutionOutputProcess(InputStream inputStream,
			AcideProgressWindow progressWindow) {

		// Stores the input stream
		_inputStream = inputStream;

		// Stores the progress window
		_progressWindow = progressWindow;
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
					_progressWindow.setText(_stringBuffer.toString());

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
