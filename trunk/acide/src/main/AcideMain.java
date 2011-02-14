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
package main;

import es.configuration.project.workbench.AcideWorkbenchManager;
import gui.mainWindow.MainWindow;
import gui.splashScreen.AcideSplashScreenWindow;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE main class.
 * 
 * @version 0.8
 */
public class AcideMain {

	/**
	 * Creation message used for specifying the expected message to determine if it is the first instance 
	 * being executed.
	 */
	public static final String CREATION_MESSAGE = "ACIDE - A Configurable IDE is already running";
	/**
	 * Socket listening port for the server socket and the connections.
	 */
	public static final int PORT = 7777;
	/**
	 * ACIDE - A Configurable IDE language labels to display in the selected language.
	 */
	private ResourceBundle _labels;
	
	/**
	 * Creates a new instance of ACIDE - A Configurable IDE main class.
	 * 
	 * Executes a new thread in which a new server socket is listened. If it is
	 * not opened yet executes the rest of the application and shows an error
	 * message in other case.
	 */
	public AcideMain() {

		// Starts the log
		AcideLog.startLog();

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		_labels = language.getLabels();

		// Sets the Look and Feel
		setLookAndFeel(_labels);
		
		new Thread() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Thread#run()
			 */
			@Override
			public void run() {
				listen();
			}
		}.start();
	}

	/**
	 * Listens to the server socket in the port which corresponds. Then it reads a message from 
	 * a socket connection.If the message matches with the creation message it means that another 
	 * instance of ACIDE - A Configurable IDE has been initialized already.
	 */
	public void listen() {

		ServerSocket serverSocket = null;
		try {
			// Creates the server socket in the specified port
			serverSocket = new ServerSocket(PORT);
		} catch (IOException exception) {

			Socket socket = null;
			try {

				// Creates the socket connection
				socket = new Socket("127.0.0.1", PORT);

				// Reads the message in the port
				DataInputStream dataInputStream = new DataInputStream(
						socket.getInputStream());
				String message = dataInputStream.readUTF();

				// If it is the expected message, then the application is
				// already being executed
				if (CREATION_MESSAGE.equals(message))

					// Shows an error message
					JOptionPane
							.showMessageDialog(
									null,
									_labels.getString("s1025"),
									_labels.getString("s1023"),
									JOptionPane.WARNING_MESSAGE);

			} catch (IOException ioexception) {
				// Shows an error message
				JOptionPane
						.showMessageDialog(
								null,
								_labels.getString("s1026"),
								_labels.getString("s1023"),
								JOptionPane.ERROR_MESSAGE);
			} finally {
				try {
					if (socket != null)
						socket.close();
				} catch (Exception ex) {

					// Shows an error message
					JOptionPane
							.showMessageDialog(
									null,
									_labels.getString("s1026"),
									_labels.getString("s1023"),
									JOptionPane.ERROR_MESSAGE);
				}
			}
			return;
		}

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s1024"));

		new Thread() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Thread#run()
			 */
			@Override
			public void run() {
				
				// Executes the application
				executeApplication();
			}
		}.start();

		try {
			
			// Creates the socket connection
			Socket socket = null;
			
			// When the server socket is available
			while ((socket = serverSocket.accept()) != null) {
				
				// Creates the data output stream
				DataOutputStream dataOutputStream = new DataOutputStream(socket.getOutputStream());
				
				// Writes the message in the socket
				dataOutputStream.writeUTF(CREATION_MESSAGE);
				
				// Closes the connection
				socket.close();
			}
		} catch (Exception exception) {

			// Shows an error message
			JOptionPane
					.showMessageDialog(
							null,
							"Error creating the socket connection at the ACIDE - A Configurable IDE launching process",
							_labels.getString("s1023"),
							JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * Executes ACIDE - A Configurable IDE.
	 */
	private void executeApplication() {

		// Shows the splash screen
		AcideSplashScreenWindow.getInstance().showSplashScreenWindow();

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s555"));

		// Loads the ACIDE - A Configurable IDE workbench configuration
		AcideWorkbenchManager.getInstance()
				.loadMainWindowWorkbenchConfiguration(
						AcideWorkbenchManager.getInstance()
								.getConfigurationFileContent());

		// Closes the splash screen
		AcideSplashScreenWindow.getInstance().closeSplashScreenWindow();

		// Shows the main window
		MainWindow.getInstance().showAcideMainWindow();
	}

	/**
	 * Sets the look and feel of ACIDE - A Configurable IDE.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void setLookAndFeel(ResourceBundle labels) {

		try {
			
			// Sets the operative system look and feel
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s549"));
		} catch (ClassNotFoundException exception) {

			// Updates the log
			AcideLog.getLog().error(
					labels.getString("s550") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		} catch (InstantiationException exception) {

			// Updates the log
			AcideLog.getLog().error(
					labels.getString("s552") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		} catch (IllegalAccessException exception) {

			// Updates the log
			AcideLog.getLog().error(
					labels.getString("s553") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		} catch (UnsupportedLookAndFeelException exception) {

			// Updates the log
			AcideLog.getLog().error(
					labels.getString("s554") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		}
	}

	/**
	 * <p>
	 * Main method of the application.
	 * </p>
	 * <p>
	 * Creates and configures the application log, load the project
	 * configuration and builds the main window of the application.
	 * </p>
	 * 
	 * @param args
	 *            entry arguments for the application.
	 */
	public static void main(String[] args) {
		new AcideMain();
	}
}
