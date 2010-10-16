package main;

import java.util.ResourceBundle;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import language.Language;
import org.apache.log4j.Logger;
import es.configuration.programmingLanguage.ProgrammingLanguage;
import operations.configuration.LoadDefaultProject;
import operations.factory.OperationsFactory;
import operations.log.Log;
import properties.PropertiesManager;
import properties.exception.MissedPropertyException;
import gui.MainWindow;
import gui.splashScreen.SplashScreen;

/**
 * 
 */
public class Acide {

	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		
		SplashScreen.showStartingWindow();
		Log.startLog();
		SplashScreen.setProgressBar(0);
		Logger logger = Log.getLog();
		SplashScreen.setProgressBar(2);
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		SplashScreen.setProgressBar(5);
		ResourceBundle labels = language.getLabels();
		SplashScreen.setProgressBar(7);
		
		// Look and Feel of the OS
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
		ProgrammingLanguage programmingLanguage = ProgrammingLanguage.getInstance();
		
		try {
			programmingLanguage.load(PropertiesManager.getProperty("languagePath"));
			MainWindow.getInstance().getStatusBar().setMessagelexical(labels.getString("s449") + " " + ProgrammingLanguage.getInstance().getName());
		} catch (MissedPropertyException e) {
			e.printStackTrace();
		}
		
		SplashScreen.setProgressBar(10);
		
		String currentGrammar = null;
		String grammarName = null;
		try {
			currentGrammar = PropertiesManager.getProperty("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1) index = currentGrammar.lastIndexOf("/");
			grammarName = currentGrammar.substring(index + 1,currentGrammar.length() - 4);
			MainWindow.getInstance().getStatusBar().setMessageGrammar(labels.getString("s248") + " " + grammarName);
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,e.getMessage(),labels.getString("s945"),JOptionPane.ERROR_MESSAGE);
			logger.error(e.getMessage());
		}
		
		SplashScreen.setProgressBar(12);
		
		// Main Window
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.getMenu().setListeners();

		SplashScreen.setProgressBar(15);
		
		//Load Default configuration of ACIDE
		OperationsFactory operationsFactory = OperationsFactory.getInstance();
		LoadDefaultProject defaultProject = operationsFactory.buildLoadDefaultProject();
		defaultProject.loadDefault(defaultProject.preloadDefault());	
	
		SplashScreen.setProgressBar(17);
		
	}
}
