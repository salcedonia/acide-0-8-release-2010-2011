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

import java.util.ResourceBundle;

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

		// Shows the splash screen
		final AcideSplashScreenWindow splashScreen = new AcideSplashScreenWindow();
		splashScreen.showSplashScreenWindow();

		// Starts the log
		AcideLog.startLog();

		// Updates the splash screen window
		AcideSplashScreenWindow.setProgressBar(2, "Creating and configuring the ACIDE - A Configurable IDE Log");

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Updates the splash screen window
		AcideSplashScreenWindow.setProgressBar(5, "Creating and configuring the ACIDE - A Configurable IDE Language");

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Updates the splash screen window
		AcideSplashScreenWindow.setProgressBar(7, "Creating and configuring the ACIDE - A Configurable IDE Language labels");

		// Sets the Look and Feel
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			// UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel");
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

		// Updates the splash screen window
		AcideSplashScreenWindow.setProgressBar(7, "Setting the ACIDE - A Configurable IDE Look And Feel");

		// Updates the log
		AcideLog.getLog().info(labels.getString("s555"));

		// Updates the splash screen window
		AcideSplashScreenWindow.setProgressBar(12, "Loading the ACIDE - A Configurable IDE Workbench Configuration");

		// Loads the ACIDE - A Configurable IDE workbench configuration
		AcideWorkbenchManager.getInstance()
				.loadMainWindowWorkbenchConfiguration(
						AcideWorkbenchManager.getInstance()
								.getConfigurationFileContent());

		// Closes the splash screen
		splashScreen.closeSplashScreenWindow();

		// Shows the main window
		MainWindow.getInstance().showAcideMainWindow();		
	}
}
