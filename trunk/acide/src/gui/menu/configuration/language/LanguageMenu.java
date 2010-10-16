package gui.menu.configuration.language;

import es.configuration.programmingLanguage.ProgrammingLanguage;
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

import operations.configuration.MenuConfiguration;
import operations.log.Log;

import org.apache.log4j.Logger;

import language.Language;
import properties.PropertiesManager;

/**
 * 
 */
public class LanguageMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String SPANISH = "./resources/icons/menu/configuration/language/spanish.png";
	/**
	 * 
	 */
	private static final String ENGLISH = "./resources/icons/menu/configuration/language/english.png";
	/**
	 * 
	 */
	private JMenuItem _spanish;
	/**
	 * 
	 */
	private JMenuItem _english;
	
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
	 * 
	 */
	public void setLanguageLabels() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
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
	 * 
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getSpanish())
			add(_spanish);
		if (MenuConfiguration.getEnglish())
			add(_english);
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// SPANISH
		_spanish.addActionListener(new SpanishListener());
		
		// ENGLISH
		_english.addActionListener(new EnglishListener());
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getEnglish() {
		return _english;
	}

	/**
	 * 
	 * @param english
	 */
	public void setEnglish(JMenuItem english) {
		_english = english;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSpanish() {
		return _spanish;
	}

	/**
	 * 
	 * @param spanish
	 */
	public void setSpanish(JMenuItem spanish) {
		_spanish = spanish;
	}
}

/**
 * 
 */
class SpanishListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
	
		Logger logger = Log.getLog();

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		MainWindow mainWindow = MainWindow.getInstance();
		PropertiesManager.setProperty("language", "0");
		ResourceBundle labels = Language.getInstance().getLabels();
		logger.info(labels.getString("s100"));
		mainWindow.getMenu().setLanguageLabels();
		ToolBarCommand.buildToolBar();
		ToolBarCommand.buildEditableToolBar();
		mainWindow.getExplorer().initPopup();
		mainWindow.validate();
		mainWindow.repaint();

		SearchGUI searchGUI = SearchGUI.getInstance();
		searchGUI.inicialize();
		searchGUI.validate();
		searchGUI.repaint();
		
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		replaceGUI.inicialize();
		replaceGUI.validate();
		replaceGUI.repaint();
		
		labels = Language.getInstance().getLabels();
		
		MainWindow
				.getInstance()
				.getStatusBar()
				.setMessagelexical(
						labels.getString("s449") + " "
								+ ProgrammingLanguage.getInstance().getName());
		try {
			String currentGrammar = PropertiesManager
					.getProperty("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			mainWindow.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
		} catch (Exception e1) {
			logger.error(e1.getMessage());
			JOptionPane.showMessageDialog(null, e1.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}

		String prj = null;
		try {
			prj = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
				.getProjectConfiguration().getName().equals(""))) {
			mainWindow.getMenu().enableProjectMenu();
		}

		int editor = mainWindow.getEditorBuilder().getNumEditors();
		if (editor > 0) {
			mainWindow.getMenu().enableFileMenu();
			mainWindow.getMenu().enableEditMenu();
		}
	}
}

/**
 * 
 */
class EnglishListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		Logger logger = Log.getLog();

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		MainWindow mainWindow = MainWindow.getInstance();
		PropertiesManager.setProperty("language", "1");
		ResourceBundle labels = Language.getInstance().getLabels();
		
		logger.info(labels.getString("s101"));
		mainWindow.getMenu().setLanguageLabels();
		mainWindow.getExplorer().initPopup();
		ToolBarCommand.buildToolBar();
		ToolBarCommand.buildEditableToolBar();
		mainWindow.validate();
		mainWindow.repaint();
		
		SearchGUI searchGUI = SearchGUI.getInstance();
		searchGUI.inicialize();
		searchGUI.validate();
		searchGUI.repaint();
		
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		replaceGUI.inicialize();
		replaceGUI.validate();
		replaceGUI.repaint();
		
		labels = Language.getInstance().getLabels();
		
		MainWindow
				.getInstance()
				.getStatusBar()
				.setMessagelexical(
						labels.getString("s449") + " "
								+ ProgrammingLanguage.getInstance().getName());
		try {
			String currentGrammar = PropertiesManager
					.getProperty("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			mainWindow.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
		} catch (Exception e1) {
			logger.error(e1.getMessage());
			JOptionPane.showMessageDialog(null, e1.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}

		String prj = null;
		try {
			prj = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
				.getProjectConfiguration().getName().equals(""))) {
			mainWindow.getMenu().enableProjectMenu();
		}

		int editor = mainWindow.getEditorBuilder().getNumEditors();
		if (editor > 0) {
			mainWindow.getMenu().enableFileMenu();
			mainWindow.getMenu().enableEditMenu();
		}
	}
}
