package main;

import java.util.ResourceBundle;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import language.Language;

import es.configuration.project.defaultConfiguration.ProjectLauncher;

import operations.factory.OperationsFactory;
import operations.log.Log;
import properties.PropertiesManager;
import gui.mainWindow.MainWindow;
import gui.splashScreen.SplashScreen;

/************************************************************************																
 * Main class of ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
	 * Main method of the application
	 * </p>
	 * <p>
	 * Creates and configures the application log, load the project
	 * configuration and builds the main window of the application
	 * </p>
	 * 
	 * @param args
	 *            entry arguments for the application
	 */
	@SuppressWarnings("static-access")
	public static void main(String[] args) {

		// Shows the splash screen
		SplashScreen splashScreen = new SplashScreen();
		splashScreen.showSplashScreenWindow();

		// Starts the log
		Log.startLog();
		SplashScreen.setProgressBar(2);

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		SplashScreen.setProgressBar(5);

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		SplashScreen.setProgressBar(7);

		// Sets the Look and Feel
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			//UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel");
			Log.getLog().info(labels.getString("s549"));
		} catch (ClassNotFoundException exception) {
			
			// Updates the log
			Log.getLog().error(
					labels.getString("s550") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		} catch (InstantiationException exception) {
			
			// Updates the log
			Log.getLog().error(
					labels.getString("s552") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		} catch (IllegalAccessException exception) {
			
			// Updates the log
			Log.getLog().error(
					labels.getString("s553") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		} catch (UnsupportedLookAndFeelException exception) {
			
			// Updates the log
			Log.getLog().error(
					labels.getString("s554") + exception.getMessage()
							+ labels.getString("s551"));
			exception.printStackTrace();
		}
		SplashScreen.setProgressBar(7);
		
		// Updates the log
		Log.getLog().info(labels.getString("s555"));

		SplashScreen.setProgressBar(12);

		// Wait a little while
		try {
			Thread.sleep(800);
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
		}

		// Builds the main window and sets the Listeners
		MainWindow.getInstance().getMenu().setListeners();

		SplashScreen.setProgressBar(99);

		// Loads the last project configuration
		ProjectLauncher defaultProject = OperationsFactory.getInstance()
				.buildProjectLauncher();
		defaultProject.load(defaultProject.getConfigurationFileContent());

		// Waits a little time
		try {
			Thread.sleep(1000);
		} catch (Exception e) {
			
		}

		SplashScreen.setProgressBar(100);

		// Closes the splash screen
		splashScreen.closeSplashScreenWindow();
	}
}
