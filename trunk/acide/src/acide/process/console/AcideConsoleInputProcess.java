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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;

import acide.log.AcideLog;

/**
 * Handles the process input thread to read the commands in the ACIDE - A
 * Configurable IDE console panel.
 * 
 * @version 0.8
 * @see Thread
 */
public class AcideConsoleInputProcess extends Thread {

	/**
	 * Buffered writer.
	 */
	private BufferedWriter _writer;
	/**
	 * Input stream.
	 */
	private InputStream _input;

	/**
	 * Creates a new input process thread.
	 * 
	 * @param writer
	 *            new buffered reader.
	 * @param input
	 *            new input stream.
	 */
	public AcideConsoleInputProcess(BufferedWriter writer, InputStream input) {
		
		// Stores the writer
		_writer = writer;
		
		// Stores the input
		_input = input;
	}

	/**
	 * Main method of the input process thread.
	 */
	public synchronized void run() {

		try {
			
			// Flushes the writer
			_writer.flush();
			
			// Creates the buffered reader
			BufferedReader bufferedReader = new BufferedReader(
					new InputStreamReader(_input));

			int i = 0;

			// Using this avoids the possibility to enter in a block
			if (_input.available() > 0) {

				while ((i = bufferedReader.read()) != -1) {
					_writer.write((char) i);
					_writer.flush();
				}
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}