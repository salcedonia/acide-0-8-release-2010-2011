package gui.menu.configuration.lexicon;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.menu.MenuConfiguration;
import es.text.TextFileFilter;
import gui.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import org.apache.log4j.Logger;

import language.Language;

import operations.factory.GUIFactory;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * Lexicon menu option of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class LexiconMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Modify menu option.
	 */
	private JMenuItem _modifyLexicon;
	/**
	 * New menu option.
	 */
	private JMenuItem _newLexicon;
	/**
	 * Save menu option.
	 */
	private JMenuItem _saveLexicon;
	/**
	 * Load menu option.
	 */
	private JMenuItem _loadLexicon;
	/**
	 * Save as menu option.
	 */
	private JMenuItem _saveAsLexicon;

	/**
	 * Constructor of the class.
	 */
	public LexiconMenu() {

		// MENU ITEM
		_modifyLexicon = new JMenuItem();
		_loadLexicon = new JMenuItem();
		_newLexicon = new JMenuItem();
		_saveLexicon = new JMenuItem();
		_saveAsLexicon = new JMenuItem();

		setLanguageLabels();
	}

	/**
	 * Set the language labels to display in the menu options.
	 */
	public void setLanguageLabels() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();

		// MODIFY
		_modifyLexicon.setText(labels.getString("s29"));
		_modifyLexicon.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// LOAD
		_loadLexicon.setText(labels.getString("s35"));
		_loadLexicon.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// NEW
		_newLexicon.setText(labels.getString("s249"));

		// SAVE
		_saveLexicon.setText(labels.getString("s250"));

		// SAVE AS
		_saveAsLexicon.setText(labels.getString("s286"));
	}

	/**
	 * Set the listeners for the menu options.
	 */
	public void setListeners() {

		// MODIFY
		_modifyLexicon.addActionListener(new ModifyLexiconListener());

		// LOAD
		_loadLexicon.addActionListener(new LoadLexiconListener());

		// SAVE
		_saveLexicon.addActionListener(new SaveLexicalListener());

		// SAVE AS
		_saveAsLexicon.addActionListener(new SaveAslexicalListener());

		// NEW
		_newLexicon.addActionListener(new NewLexicalListener());
	}

	/**
	 * Builds the menu.
	 */
	public void buildMenu() {

		removeAll();

		if (MenuConfiguration.getNewLexical())
			add(_newLexicon);
		if (MenuConfiguration.getLoadParameters())
			add(_loadLexicon);
		if (MenuConfiguration.getLexicon())
			add(_modifyLexicon);
		if (MenuConfiguration.getSaveLexical())
			add(_saveLexicon);
		if (MenuConfiguration.getSaveAslexical())
			add(_saveAsLexicon);
	}

	/**
	 * Returns the load parameters menu item.
	 * 
	 * @return The load parameters menu item.
	 */
	public JMenuItem getLoadParameters() {
		return _loadLexicon;
	}

	/**
	 * Set a new value to the load parameters menu item.
	 * 
	 * @param loadParameters
	 *            New value to set.
	 */
	public void setLoadParameters(JMenuItem loadParameters) {
		_loadLexicon = loadParameters;
	}

	/**
	 * Returns the modify lexicon menu item.
	 * 
	 * @return The modify lexicon menu item.
	 */
	public JMenuItem getModifyLexicon() {
		return _modifyLexicon;
	}

	/**
	 * Set a new value to modify lexicon menu item.
	 * 
	 * @param modifyLexicon
	 *            New value to set.
	 */
	public void setModifyLexicon(JMenuItem modifyLexicon) {
		_modifyLexicon = modifyLexicon;
	}

	/**
	 * Returns the new lexicon menu item.
	 * 
	 * @return The new lexicon menu item.
	 */
	public JMenuItem getNewLexicon() {
		return _newLexicon;
	}

	/**
	 * Set a new value to the new lexicon menu item.
	 * 
	 * @param newLexicon
	 *            New value to set.
	 */
	public void setNewLexicon(JMenuItem newLexicon) {
		_newLexicon = newLexicon;
	}

	/**
	 * Returns the save lexicon menu item.
	 * 
	 * @return The save lexicon menu item.
	 */
	public JMenuItem getSaveLexicon() {
		return _saveLexicon;
	}

	/**
	 * Set a new value to the save lexicon menu item.
	 * 
	 * @param saveLexicon
	 *            New value to set.
	 */
	public void setSaveLexicon(JMenuItem saveLexicon) {
		_saveLexicon = saveLexicon;
	}

	/**
	 * Returns the save as lexicon menu item.
	 * 
	 * @return The save as lexicon menu item.
	 */
	public JMenuItem getSaveAsLexicon() {
		return _saveAsLexicon;
	}

	/**
	 * Set a new value to the save as lexicon menu item.
	 * 
	 * @param saveAsLexicon
	 *            New value to set.
	 */
	public void setSaveAsLexicon(JMenuItem saveAsLexicon) {
		_saveAsLexicon = saveAsLexicon;
	}
}

/**
 * Listener of the modify menu option.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class ModifyLexiconListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildLexiconGUI();
	}
}

/**
 * Listener of the save as menu option.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class SaveAslexicalListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		// GET THE LOGGER
		Logger logger = Log.getLog();

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		// GET THE LABELS TO DISPLAY
		ResourceBundle labels = language.getLabels();

		JFileChooser chooser = new JFileChooser(labels.getString("s126"));
		TextFileFilter filter = new TextFileFilter(labels.getString("s287"));
		filter.addExtension("xml");
		chooser.setFileFilter(filter);
		chooser.setCurrentDirectory(new File("./configuration/lexical"));

		int option = chooser.showSaveDialog(null);
		String filePath = " ";

		if (option == JFileChooser.APPROVE_OPTION)
			filePath = chooser.getSelectedFile().getAbsolutePath();

		// IF THE PATH IS OK
		if (!filePath.equals(" ")) {

			// GET THE NAME OF THE LANGUAGE
			int index = filePath.lastIndexOf("\\");
			if (index == -1)
				index = filePath.lastIndexOf("/");
			String fileName = filePath.substring(index + 1, filePath.length());
			
			if (fileName.contains(".")) {
				index = fileName.lastIndexOf(".");
				fileName = fileName.substring(0, index);
			}

			// SAVE LANGUAGE AS
			LexiconConfiguration programmingLanguage = LexiconConfiguration
					.getInstance();
			boolean result = programmingLanguage.saveAs(fileName, false,
					filePath);

			// IF IT COULD SAVE IT
			if (result) {
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		} else
			logger.info(labels.getString("s92"));
	}
}

/**
 * Listener of the save menu option.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class SaveLexicalListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		// GET THE LOGGER
		Logger logger = Log.getLog();

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		LexiconConfiguration programmingLanguage = LexiconConfiguration
				.getInstance();

		// GET THE NAME OF THE CURRENT LANGUAGE
		String path = programmingLanguage.getName();

		// IF IT IS OK
		if (!path.equals(" ")) {

			// GET THE NAME
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String languageName = path.substring(index + 1, path.length());

			if (languageName.contains(".")) {
				index = languageName.lastIndexOf(".");
				languageName = languageName.substring(0, index);
			}

			// SAVE IT
			boolean result = programmingLanguage.save(languageName, false);

			// IF IT COULD SAVE IT
			if (result)
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);
			else
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
		} else {
			logger.info(labels.getString("s92"));
		}
	}
}

/**
 * Listener of the new menu option.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class NewLexicalListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		// GET THE LABELS TO DISPLAY
		ResourceBundle labels = language.getLabels();

		String languagePath = "";
		String languageName = "";
		languageName = JOptionPane.showInputDialog(null, labels
				.getString("s453"), labels.getString("s454"),
				JOptionPane.INFORMATION_MESSAGE);

		// IF IS A NAME VALID
		if (!languageName.equals("")) {

			if(languageName.contains(".xml"))
				languagePath = "./configuration/lexical/" + languageName;
			else
				languagePath = "./configuration/lexical/" + languageName + ".xml";

			// RESET ALL THE OPENED FILES IN THE EDITOR WITH THE NEW LEXICAL
			// CONFIGURATION
			LexiconConfiguration.getInstance().newLexical(languagePath);
			int numEditors = MainWindow.getInstance().getEditorBuilder()
					.getNumEditors();
			for (int j = 0; j < numEditors; j++)
				MainWindow.getInstance().getEditorBuilder().getEditorAt(j)
						.resetDocument();

			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().getMessagelexical()
					.setText(
							labels.getString("s449")
									+ " "
									+ LexiconConfiguration.getInstance()
											.getName());
			// UPDATES THE PROJECT CONFIGURATION
			MainWindow.getInstance().getProjectConfiguration()
					.setLexicalConfiguration(
							LexiconConfiguration.getInstance().getName());

			// NOT DEFAULT PROJECT
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);
			}
		} else {

			// MISSED NAME
			JOptionPane.showMessageDialog(null, labels.getString("s976"),
					labels.getString("s972"), JOptionPane.WARNING_MESSAGE);
		}
	}
}

/**
 * Listener of the load menu option.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class LoadLexiconListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		ResourceBundle labels = Language.getInstance().getLabels();
		TextFileFilter filter = new TextFileFilter(labels.getString("s327"));
		filter.addExtension(".xml");

		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addChoosableFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/lexical/"));

		int chosenOption = fileChooser.showOpenDialog(fileChooser);

		String filePath = " ";

		if (chosenOption == JFileChooser.APPROVE_OPTION)
			filePath = fileChooser.getSelectedFile().getAbsolutePath();

		// IF THE PATH IS OK
		if (!filePath.equals(" ")) {

			// LOAD THE LEXICAL CONFIGURATION
			LexiconConfiguration.getInstance().load(
					fileChooser.getSelectedFile().getAbsolutePath());

			// RESET ALL THE OPENED FILES IN THE EDITOR WITH THE NEW LEXICAL
			// CONFIGURATION
			int numEditors = MainWindow.getInstance().getEditorBuilder()
					.getNumEditors();
			for (int i = 0; i < numEditors; i++)
				MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
						.resetDocument();

			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().getMessagelexical()
					.setText(
							labels.getString("s449")
									+ " "
									+ LexiconConfiguration.getInstance()
											.getName());
			// UPDATES THE PROJECT CONFIGURATION
			MainWindow.getInstance().getProjectConfiguration()
					.setLexicalConfiguration(
							LexiconConfiguration.getInstance().getPath());
			
			// NOT DEFAULT PROJECT
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);
			}
		}
	}
}