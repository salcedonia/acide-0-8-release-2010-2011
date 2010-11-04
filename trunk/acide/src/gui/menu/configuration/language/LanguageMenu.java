package gui.menu.configuration.language;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.menu.MenuConfiguration;
import gui.MainWindow;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;
import gui.toolBarButton.ToolBarCommand;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import operations.log.Log;

import org.apache.log4j.Logger;

import language.Language;
import properties.PropertiesManager;

/**
 * Language menu of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class LanguageMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the Spanish icon.
	 */
	private static final String SPANISH = "./resources/icons/menu/configuration/language/spanish.png";
	/**
	 * Image file for the English icon.
	 */
	private static final String ENGLISH = "./resources/icons/menu/configuration/language/english.png";
	/**
	 * Spanish menu item.
	 */
	private JMenuItem _spanish;
	/**
	 * English menu item.
	 */
	private JMenuItem _english;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();;
	
	/**
	 * Constructor of the class.
	 */
	public LanguageMenu(){
				
		// MENU ITEM
		_spanish = new JMenuItem(new ImageIcon(SPANISH));
		_english = new JMenuItem(new ImageIcon(ENGLISH));
		
		setLanguageLabels();
	}
	
	/**
	 * Set the labels to display in the menu in the selected language.
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
		
		// SPANISH
		_spanish.setText(labels.getString("s11"));
		_spanish.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK));
		
		// ENGLISH
		_english.setText(labels.getString("s12"));
		_english.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.ALT_MASK));	
	}
	
	/**
	 * Builds the menu.
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getSpanish())
			add(_spanish);
		if (MenuConfiguration.getEnglish())
			add(_english);
	}
	
	/**
	 * Set the menu listeners.
	 */
	public void setListeners(){
		
		// SPANISH
		_spanish.addActionListener(new SpanishListener());
		
		// ENGLISH
		_english.addActionListener(new EnglishListener());
	}
	
	/**
	 * Returns the English menu item.
	 * 
	 * @return The English menu item.
	 */
	public JMenuItem getEnglish() {
		return _english;
	}

	/**
	 * Set a new value to the English menu item.
	 * 
	 * @param english New value to set.
	 */
	public void setEnglish(JMenuItem english) {
		_english = english;
	}

	/**
	 * Returns the Spanish menu file.
	 * 
	 * @return The Spanish menu file.
	 */
	public JMenuItem getSpanish() {
		return _spanish;
	}

	/**
	 * Set a new value to the Spanish menu item.
	 * 
	 * @param spanish new value to set.
	 */
	public void setSpanish(JMenuItem spanish) {
		_spanish = spanish;
	}
	
	/**
	 * Changes the language to display in the application and reset
	 * all the components with the new language.
	 * 
	 * @param selectedLanguage New language to set.
	 */
	public void changeLanguage(String selectedLanguage){

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// SET THE SELECTED LANGUAGE
		PropertiesManager.setProperty("language", selectedLanguage);
		
		// GET THE LABELS
		ResourceBundle labels = Language.getInstance().getLabels();
		_logger.info(labels.getString("s100"));
		
		// RESET THE MAIN WINDOW
		MainWindow.getInstance().getMenu().setLanguageLabels();
		ToolBarCommand.buildToolBar();
		ToolBarCommand.buildEditableToolBar();
		MainWindow.getInstance().getExplorer().initPopup();
		MainWindow.getInstance().getOutput().initPopup();
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// RESET THE SEARCH GUI
		SearchGUI searchGUI = SearchGUI.getInstance();
		searchGUI.inicialize();
		searchGUI.validate();
		searchGUI.repaint();
		
		// RESET THE REPLACE GUI
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		replaceGUI.inicialize();
		replaceGUI.validate();
		replaceGUI.repaint();
		
		labels = Language.getInstance().getLabels();
		
		// UPDATES THE STATUS BAR
		MainWindow
				.getInstance()
				.getStatusBar()
				.setMessagelexical(
						labels.getString("s449") + " "
								+ LexiconConfiguration.getInstance().getName());
		try {
			
			// GET THE NAME
			String currentGrammar = PropertiesManager
					.getProperty("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			
			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
			
		} catch (Exception e1) {
			
			// GRAMMAR LOADING FAILED
			_logger.error(e1.getMessage());
			JOptionPane.showMessageDialog(null, e1.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}
		
		// NOT DEFAULT PROJECT
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
			MainWindow.getInstance().getMenu().enableProjectMenu();
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
		}

		if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0) {
			MainWindow.getInstance().getMenu().enableFileMenu();
			MainWindow.getInstance().getMenu().enableEditMenu();
		}
	}
	
	/**
	 * Spanish menu item listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class SpanishListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {		
			changeLanguage("spanish");
		}
	}

	/**
	 * English menu item listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EnglishListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			changeLanguage("english");
		}
	}
}


