package gui;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;
import properties.PropertiesManager;

/**
 * Listener of the main window of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class MainWindowListener extends WindowAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
	 * )
	 */
	public void windowClosing(WindowEvent arg0) {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS TO DISPLAY
		final ResourceBundle labels = language.getLabels();

		// IS THE PROJECT CONFIGURATION MODIFIED?
		if (MainWindow.getInstance().getProjectConfiguration().isModified()) {

			// ASK TO THE USER FOR SAVING THE CONFIGURATION
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_OPTION);

			// IF THE USER CHOOSES YES
			if (chosenOption == JOptionPane.OK_OPTION) {

				// ENABLE THE MENU
				MainWindow.getInstance().getMenu().getProject().getSaveProject()
						.setEnabled(true);
				
				// SAVE THE PROJECT
				MainWindow.getInstance().getMenu().getProject().getSaveProject()
						.doClick();
			}
		}

		// CHECK THE OPENED FILES IN THE EDITOR
		int selectedEditor = MainWindow.getInstance().getEditorBuilder()
				.getSelectedEditorIndex();
		int numEditors = MainWindow.getInstance().getEditorBuilder().getNumEditors();

		// STARTS WITH THE LAST ONE
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(numEditors - 1);
		
		for (int cont = numEditors - 1; cont >= 0; cont--) {
			MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(cont);

			// IF THE FILE IN THE EDITOR IS MODIFIED
			if (MainWindow.getInstance().getEditorBuilder().isRedButton()) {

				// ASK TO THE USER IF WANTS TO SAVE IT
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// IF YES
				if (chosenOption == JOptionPane.OK_OPTION) {
					MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
				}
			}
		}

		// SET THE SELECTED EDITOR TO THE PREVIOUS 
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(selectedEditor);

		// CLOSE THE OUTPUT
		MainWindow.getInstance().getOutput().executeExitCommand();

		// UPDATES THE CONFIGURATION
		try {

			// MENU CONFIGURATION
			String currentMenu = PropertiesManager
					.getProperty("currentMenuConfiguration");

			if ((currentMenu.endsWith("lastModified.menuCfg"))
					|| (currentMenu.endsWith("newMenu.menuCfg"))) {
				String previous = PropertiesManager
						.getProperty("previousMenuConfiguration");
				PropertiesManager.setProperty("currentMenuConfiguration",
						previous);
			}

			// TOOLBAR
			String currentToolBar = PropertiesManager
					.getProperty("currentToolBarConfiguration");
			if ((currentToolBar.endsWith("lastModified.BHcfg"))
					|| currentToolBar.endsWith("newToolBar.BHcfg")) {
				String previous = PropertiesManager
						.getProperty("previousToolBarConfiguration");
				PropertiesManager.setProperty(
						"currentToolBarConfiguration", previous);
			}

			// GRAMMAR
			String currentGrammar = PropertiesManager
					.getProperty("currentGrammar");
			if ((currentGrammar.endsWith("lastModified.jar"))
					|| (currentGrammar.endsWith("newGrammar.jar"))) {
				String previous = PropertiesManager
						.getProperty("previousGrammar");
				PropertiesManager.setProperty("currentGrammar", previous);
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null, e.getMessage(),
					labels.getString("s294"), JOptionPane.ERROR_MESSAGE);
		}

		// STORES THE CONFIGURATION OF THE FILES
		MainWindow.getInstance().closeDefaultProject();

		// SAVE THE CONFIGURATION PARAMETERS OF THE MAIN WINDOW
		MainWindow.getInstance().getProjectConfiguration().saveMainWindowParameters();
	}
}
