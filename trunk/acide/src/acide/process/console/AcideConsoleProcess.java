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

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.consolePanel.AcideConsolePanel;
import acide.gui.mainWindow.AcideMainWindow;

import java.io.*;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE console process.
 * 
 * @version 0.8
 * @see Thread
 */
public class AcideConsoleProcess extends Thread {

	/**
	 * ACIDE - A Configurable IDE console process buffered writer.
	 */
	private static BufferedWriter _writer = null;
	/**
	 * ACIDE - A Configurable IDE console process shell process to execute.
	 */
	public Process _process = null;

	/**
	 * Creates a new ACIDE - A Configurable IDE console process.
	 */
	public AcideConsoleProcess() {

	}

	/**
	 * Main method of the ACIDE - A Configurable IDE console process.
	 */
	public synchronized void run() {

		// Use "/bin/sh" in Linux.
		String shellPath = null;
		String shellDirectory = null;

		try {

			// Gets the shell path
			shellPath = AcideResourceManager.getInstance().getProperty(
					"consolePanel.shellPath");

			// Gets the shell directory
			shellDirectory = AcideResourceManager.getInstance().getProperty(
					"consolePanel.shellDirectory");

			// Checks if both paths exist
			File shellPathFile = new File(shellPath);
			File shellDirectoryFile = new File(shellDirectory);

			// If both resources exist
			if ((shellPathFile.exists() && shellPathFile.isFile())
					&& (shellDirectoryFile.exists() && shellDirectoryFile
							.isDirectory())) {

				// Open the console stream
				File path = new File(shellDirectory);
				_process = Runtime.getRuntime().exec(shellPath, null, path);

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {

						// As it is a valid console, it is editable
						AcideMainWindow.getInstance().getConsolePanel()
								.getTextPane().setEditable(true);
					}
				});

				// Creates the output stream writer
				_writer = new BufferedWriter(new OutputStreamWriter(
						_process.getOutputStream()));

				// Creates the input stream
				AcideConsoleInputProcess inputThread = new AcideConsoleInputProcess(
						_writer, System.in);

				// Creates the error stream
				AcideConsoleOutputProcess errorGobbler = new AcideConsoleOutputProcess(
						_process.getErrorStream(), AcideMainWindow
								.getInstance().getConsolePanel());

				// Creates the output stream
				AcideConsoleOutputProcess outputGobbler = new AcideConsoleOutputProcess(
						_process.getInputStream(), AcideMainWindow
								.getInstance().getConsolePanel());

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
			} else {

				// Sets the console default configuration
				setDefaultConfiguration();
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

			// The project configuration has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);

			// Sets the console default configuration
			setDefaultConfiguration();
		}
	}

	/**
	 * Sets the console default configuration.
	 */
	private void setDefaultConfiguration() {

		// Sets the default project configuration for the shell path
		AcideProjectConfiguration.getInstance().setShellPath("");

		// Updates the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.shellPath", "");

		// Sets the default project configuration for the shell directory
		AcideProjectConfiguration.getInstance().setShellDirectory("");

		// Updates the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.shellDirectory", "");

		// Sets the default project configuration for the exit command
		AcideProjectConfiguration.getInstance().setExitCommand("");

		// Updates the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.exitCommand", "");

		// Sets the default project configuration for the echo command
		AcideProjectConfiguration.getInstance().setIsEchoCommand(false);

		// Updates the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.isEchoCommand", "false");

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// Clears the console buffer
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.setText("");

				// Validates the changes in the console panel
				AcideMainWindow.getInstance().getConsolePanel().validate();

				// Repaint the console panel
				AcideMainWindow.getInstance().getConsolePanel().repaint();

				// As it is not a valid console, it is not editable
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.setEditable(false);
			}
		});
	}

	/**
	 * Returns the buffered writer.
	 * 
	 * @return the buffered writer.
	 */
	public BufferedWriter getWriter() {
		return _writer;
	}

	/**
	 * Returns the process to execute.
	 * 
	 * @return the process to execute.
	 */
	public Process getProcess() {
		return _process;
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
			String exitCommand, AcideConsolePanel consolePanel) {

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
			AcideConsoleOutputProcess errorGobbler = new AcideConsoleOutputProcess(
					process.getErrorStream(), consolePanel);

			// Creates the output stream
			AcideConsoleOutputProcess outputGobbler = new AcideConsoleOutputProcess(
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
