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
package acide.process.execution;

import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.gui.AcideProgressWindow;

/**
 * ACIDE - A Configurable IDE project execution process.
 * 
 * @version 0.8
 * @see Thread
 */
public class AcideProjectExecutionProcess extends Thread {

	/**
	 * ACIDE - A Configurable IDE project execution process executable path.
	 */
	private String _executablePath;
	/**
	 * ACIDE - A Configurable IDE project execution process arguments.
	 */
	private String _arguments;

	/**
	 * Creates a new ACIDE - A Configurable IDE project execution process.
	 * 
	 * @param executablePath
	 *            executable path.
	 * @param arguments
	 *            project execution arguments.
	 */
	public AcideProjectExecutionProcess(String executablePath, String arguments) {

		// Stores the executable path
		_executablePath = executablePath;

		// Stores the arguments
		_arguments = arguments;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {

		try {

			// Sets the initial text in the progress window
			AcideProgressWindow.getInstance().setInitialText(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s2004"));

			// Displays the progress window
			AcideProgressWindow.getInstance().showWindow();

			// Launches the executable
			Process process = Runtime.getRuntime().exec(
					"\"" + _executablePath + "\" " + _arguments);

			// Creates the output stream
			AcideProjectExecutionOutputProcess outputGobbler = new AcideProjectExecutionOutputProcess(
					process.getInputStream(), AcideProgressWindow.getInstance());

			// Creates the error stream
			AcideProjectExecutionOutputProcess errorGobbler = new AcideProjectExecutionOutputProcess(
					process.getErrorStream(), AcideProgressWindow.getInstance());

			// Starts the output stream
			outputGobbler.start();
			
			// Starts the error stream
			errorGobbler.start();

			// Waits until the thread to finish its job
			process.waitFor();

			// Enables the close button in the progress window
			AcideProgressWindow.getInstance().enableCloseButton();

		} catch (Exception exception) {

			// Displays an error message
			JOptionPane.showMessageDialog(null, exception.getMessage());

			// Closes the progress window
			AcideProgressWindow.getInstance().closeWindow();

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
