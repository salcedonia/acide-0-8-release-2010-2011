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
package acide.process.externalCommand;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;

import javax.swing.JOptionPane;
import javax.swing.JTextPane;

import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.console.AcideConsoleInputProcess;

/**
 * ACIDE - A Configurable IDE external command process.
 * 
 * @version 0.8
 * @see Thread
 */
public class AcideExternalCommandProcess extends Thread {

	/**
	 * Shell path.
	 */
	private final String _shellPath;
	/**
	 * Shell directory.
	 */
	private final String _shellDirectory;
	/**
	 * Text component.
	 */
	private JTextPane _textComponent;

	/**
	 * Creates a new ACIDE - A Configurable IDE external command process.
	 * 
	 * @param shellPath
	 *            shell path.
	 * @param shellDirectory
	 *            shell directory.
	 * @param textComponent
	 *            text component.
	 */
	public AcideExternalCommandProcess(String shellPath, String shellDirectory,
			JTextPane textComponent) {

		// Stores the shell path
		_shellPath = shellPath;

		// Stores the shell directory
		_shellDirectory = shellDirectory;

		// Stores the text component
		_textComponent = textComponent;
	}

	/**
	 * Main method of the ACIDE - A Configurable IDE console process.
	 */
	public synchronized void run() {

		try {

			// Open the console stream
			Process _process = Runtime.getRuntime().exec(_shellPath, null,
					new File(_shellDirectory));

			// Creates the output stream writer
			BufferedWriter _writer = new BufferedWriter(new OutputStreamWriter(
					_process.getOutputStream()));

			// Creates the input stream
			AcideConsoleInputProcess inputThread = new AcideConsoleInputProcess(
					_writer, System.in);

			// Creates the error stream
			AcideOutputProcess errorGobbler = new AcideOutputProcess(
					_process.getErrorStream(), _textComponent);

			// Creates the output stream
			AcideOutputProcess outputGobbler = new AcideOutputProcess(
					_process.getInputStream(), _textComponent);

			// Starts the error stream
			errorGobbler.start();

			// Starts the output stream
			outputGobbler.start();

			// Starts the input stream
			inputThread.start();

			// Waits until the thread to finish its job
			try {
				_process.waitFor();
			} catch (InterruptedException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		} catch (Exception exception) {

			// If the user selects an existing file, but it is not a valid
			// application

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Displays an error message
			JOptionPane.showMessageDialog(
					AcideMainWindow.getInstance(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s1017"), "Error",
					JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * Executes a command in the console given as a parameter.
	 * 
	 * @param shell
	 *            shell in which the command is going to be executed.
	 * @param shellPath
	 *            shell path.
	 * @param command
	 *            command to execute.
	 * @param exitCommand
	 *            exit command.
	 * @param consolePanel
	 *            console panel in which the result of the execution will be
	 *            displayed.
	 */
	public void executeCommand(String shell, String shellPath, String command,
			String exitCommand, JTextPane consolePanel) {

		Process process = null;
		String pathOutput = shellPath;

		try {

			File filePath = new File(pathOutput);
			process = Runtime.getRuntime().exec(shell, null, filePath);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		} finally {
			// Creates the writer stream
			BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
					process.getOutputStream()));

			// Creates the error stream
			AcideOutputProcess errorGobbler = new AcideOutputProcess(
					process.getErrorStream(), consolePanel);

			// Creates the output stream
			AcideOutputProcess outputGobbler = new AcideOutputProcess(
					process.getInputStream(), consolePanel);

			// Starts the threads
			errorGobbler.start();
			outputGobbler.start();

			try {
				writer.write(command + '\n');
				writer.flush();
				writer.write(exitCommand + '\n');
				writer.flush();
			} catch (IOException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}
}
