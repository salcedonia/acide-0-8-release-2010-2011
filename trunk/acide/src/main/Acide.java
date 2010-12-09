package main;

import es.configuration.project.workbench.AcideWorkbenchManager;
import gui.mainWindow.MainWindow;
import gui.splashScreen.AcideSplashScreen;

import java.util.ResourceBundle;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Main class of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 ***********************************************************************/
public class Acide {

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
		AcideSplashScreen splashScreen = new AcideSplashScreen();
		splashScreen.showSplashScreenWindow();

		// Starts the log
		AcideLog.startLog();
		AcideSplashScreen.setProgressBar(2);

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		AcideSplashScreen.setProgressBar(5);

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		AcideSplashScreen.setProgressBar(7);

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
		AcideSplashScreen.setProgressBar(7);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s555"));

		AcideSplashScreen.setProgressBar(12);

		// Builds the main window and sets the Listeners
		MainWindow.getInstance().getMenu().setListeners();

		AcideSplashScreen.setProgressBar(99);

		// Loads the ACIDE - A Configurable IDE workbench configuration
		AcideWorkbenchManager.getInstance()
				.loadMainWindowWorkbenchConfiguration(
						AcideWorkbenchManager.getInstance()
								.getConfigurationFileContent());

		AcideSplashScreen.setProgressBar(100);

		// Closes the splash screen
		splashScreen.closeSplashScreenWindow();
	}
}
