package gui.menu.configuration.language;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.menu.MenuConfiguration;
import gui.mainWindow.MainWindow;
import gui.menu.configuration.language.listeners.EnglishListener;
import gui.menu.configuration.language.listeners.SpanishListener;
import gui.menu.edit.gui.replace.ReplaceWindow;
import gui.menu.edit.gui.search.SearchWindow;
import gui.toolBar.ToolBar;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import operations.log.Log;

import language.Language;
import properties.PropertiesManager;

/************************************************************************																
 * Language menu of ACIDE - A Configurable IDE											
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
 * @see JMenu																												
 ***********************************************************************/
public class LanguageMenu extends JMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the Spanish icon
	 */
	private static final String SPANISH = "./resources/icons/menu/configuration/language/spanish.png";
	/**
	 * Image file for the English icon
	 */
	private static final String ENGLISH = "./resources/icons/menu/configuration/language/english.png";
	/**
	 * Spanish menu item
	 */
	private JMenuItem _spanish;
	/**
	 * English menu item
	 */
	private JMenuItem _english;
	
	/**
	 * Class constructor
	 */
	public LanguageMenu(){
				
		// MENU ITEM
		_spanish = new JMenuItem(new ImageIcon(SPANISH));
		_english = new JMenuItem(new ImageIcon(ENGLISH));
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the labels to display in the menu in the selected language
	 */
	public void setLanguageLabels() {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
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
	 * Builds the menu
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getSpanish())
			add(_spanish);
		if (MenuConfiguration.getEnglish())
			add(_english);
	}
	
	/**
	 * Sets the menu item Listeners
	 */
	public void setListeners(){
		
		// SPANISH
		_spanish.addActionListener(new SpanishListener());
		
		// ENGLISH
		_english.addActionListener(new EnglishListener());
	}
	
	/**
	 * Returns the English menu item
	 * 
	 * @return the English menu item
	 */
	public JMenuItem getEnglish() {
		return _english;
	}

	/**
	 * Sets a new value to the English menu item
	 * 
	 * @param english new value to set
	 */
	public void setEnglish(JMenuItem english) {
		_english = english;
	}

	/**
	 * Returns the Spanish menu item
	 * 
	 * @return the Spanish menu item
	 */
	public JMenuItem getSpanish() {
		return _spanish;
	}

	/**
	 * Sets a new value to the Spanish menu item
	 * 
	 * @param spanish new value to set
	 */
	public void setSpanish(JMenuItem spanish) {
		_spanish = spanish;
	}
	
	/**
	 * Changes the language to display in the application and reset
	 * all the components with the new language
	 * 
	 * @param selectedLanguage new language to set
	 */
	public void changeLanguage(String selectedLanguage){

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// SET THE SELECTED LANGUAGE
		PropertiesManager.setProperty("language", selectedLanguage);
		
		// Gets the labels
		ResourceBundle labels = Language.getInstance().getLabels();
		Log.getLog().info(labels.getString("s100"));
		
		// RESET THE MAIN WINDOW
		MainWindow.getInstance().getMenu().setLanguageLabels();
		ToolBar.buildStaticToolBar();
		ToolBar.buildModifiableToolBar();
		
		// UPDATES THE POPUP MENUS
		
		// EXPLORER POPUP MENU
		MainWindow.getInstance().getExplorer().buildPopupMenu();
		
		// OUTPUT POPUP MENU
		MainWindow.getInstance().getOutput().buildPopupMenu();
		
		// STATUS BAR POPUP MENU
		MainWindow.getInstance().getStatusBar().buildPopupMenu();
		
		// EDITORS POPUP MENU
		int numEditors = MainWindow.getInstance().getEditorManager().getNumEditors();
		for(int pos = 0; pos < numEditors; pos++)
			MainWindow.getInstance().getEditorManager().getEditorAt(pos).buildPopupMenu();
		
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// RESET THE SEARCH GUI
		SearchWindow searchGUI = SearchWindow.getInstance();
		searchGUI.inicialize();
		searchGUI.validate();
		searchGUI.repaint();
		
		// RESET THE REPLACE GUI
		ReplaceWindow replaceGUI = ReplaceWindow.getInstance();
		replaceGUI.inicialize();
		replaceGUI.validate();
		replaceGUI.repaint();
		
		labels = Language.getInstance().getLabels();
		
		// Updates the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
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
			
			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setGrammarMessage(
					labels.getString("s248") + " " + grammarName);
			
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			
			// ERROR MESSAGE
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}
		
		// NOT DEFAULT PROJECT
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
			MainWindow.getInstance().getMenu().enableProjectMenu();
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
		}

		if (MainWindow.getInstance().getEditorManager().getNumEditors() > 0) {
			MainWindow.getInstance().getMenu().enableFileMenu();
			MainWindow.getInstance().getMenu().enableEditMenu();
		}
	}
}