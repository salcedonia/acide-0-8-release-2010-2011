package main;

import java.util.ResourceBundle;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import language.Language;
import org.apache.log4j.Logger;

import es.configuration.project.defaultConfiguration.DefaultProjectLoader;

import operations.factory.OperationsFactory;
import operations.log.Log;
import properties.PropertiesManager;
import gui.MainWindow;
import gui.splashScreen.SplashScreen;

/**
 * Main class of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class Acide {

	/**
	 * Main method of the application.
	 * 
	 * @param args
	 *            Entry arguments for the application.
	 */
	public static void main(String[] args) {

		// SHOWS THE SPLASH SCREEN
		SplashScreen splashScreen = new SplashScreen();
		splashScreen.showSplashScreenWindow();

		// STARTS THE LOG
		Log.startLog();
		SplashScreen.setProgressBar(0);

		// GET THE LOG
		Logger logger = Log.getLog();
		SplashScreen.setProgressBar(2);

		// GET THE LANGUAGE TO DISPLAY
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		SplashScreen.setProgressBar(5);

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		SplashScreen.setProgressBar(7);

		// SET THE LOOK AND FEEL OF THE APPLICATION
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			logger.info(labels.getString("s549"));
		} catch (ClassNotFoundException e) {
			logger.error(labels.getString("s550") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		} catch (InstantiationException e) {
			logger.error(labels.getString("s552") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			logger.error(labels.getString("s553") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		} catch (UnsupportedLookAndFeelException e) {
			logger.error(labels.getString("s554") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		}
		SplashScreen.setProgressBar(7);
		logger.info(labels.getString("s555"));
		
		SplashScreen.setProgressBar(12);

		// WAIT A LITTLE WHILE
	    try { Thread.sleep(800); } catch (Exception e) {}

		// BUILD THE MAIN WINDOW OF THE APPLICATION AND SET THE LISTENERS
		MainWindow.getInstance().getMenu().setListeners();
		
		SplashScreen.setProgressBar(97);

		// LOAD THE LAST CONFIGURATION OF ACIDE
		DefaultProjectLoader defaultProject = OperationsFactory.getInstance()
				.buildDefaultProjectLoader();
		defaultProject.load(defaultProject.getDefaultFileContent());
		
		// WAIT A LITTLE WHILE
	    try { Thread.sleep(1000); } catch (Exception e) {}

		SplashScreen.setProgressBar(99);

		// CLOSE THE SPLASH SCREEN
		splashScreen.closeSplashScreenWindow();
	}
}
