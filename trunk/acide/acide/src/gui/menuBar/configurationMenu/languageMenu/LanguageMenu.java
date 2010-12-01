package gui.menuBar.configurationMenu.languageMenu;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.menu.MenuConfiguration;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.languageMenu.listeners.EnglishMenuItemListener;
import gui.menuBar.configurationMenu.languageMenu.listeners.SpanishMenuItemListener;
import gui.menuBar.editMenu.gui.replace.ReplaceWindow;
import gui.menuBar.editMenu.gui.search.SearchWindow;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import operations.log.AcideLog;
import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************																
 * Language menu of ACIDE - A Configurable IDE.											
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
	 * Language menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Spanish menu item name.
	 */
	public static final String SPANISH_NAME = "Spanish";
	/**
	 * English menu item name.
	 */
	public static final String ENGLISH_NAME = "English";
	/**
	 * Spanish menu item image icon.
	 */
	private static final ImageIcon SPANISH_IMAGE = new ImageIcon("./resources/icons/menu/configuration/language/spanish.png");
	/**
	 * English menu item image icon.
	 */
	private static final ImageIcon ENGLISH_IMAGE = new ImageIcon("./resources/icons/menu/configuration/language/english.png");
	/**
	 * Spanish menu item.
	 */
	private JMenuItem _spanish;
	/**
	 * English menu item.
	 */
	private JMenuItem _english;
	
	/**
	 * Creates a new language menu.
	 */
	public LanguageMenu(){
				
		// MENU ITEM
		_spanish = new JMenuItem(SPANISH_IMAGE);
		_english = new JMenuItem(ENGLISH_IMAGE);
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the labels to display in the menu in the selected language.
	 */
	public void setLanguageLabels() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
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
	 * Builds the language menu.
	 */
	public void buildMenu(){
		
		removeAll();
		
		// SPANISH MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SPANISH_NAME))
			add(_spanish);
		
		// ENGLISH MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(ENGLISH_NAME))
			add(_english);
	}
	
	/**
	 * Sets the language menu item listeners.
	 */
	public void setListeners(){
		
		// SPANISH
		_spanish.addActionListener(new SpanishMenuItemListener());
		
		// ENGLISH
		_english.addActionListener(new EnglishMenuItemListener());
	}
	
	/**
	 * Returns the English menu item.
	 * 
	 * @return the English menu item.
	 */
	public JMenuItem getEnglish() {
		return _english;
	}

	/**
	 * Sets a new value to the English menu item.
	 * 
	 * @param english new value to set.
	 */
	public void setEnglish(JMenuItem english) {
		_english = english;
	}

	/**
	 * Returns the Spanish menu item.
	 * 
	 * @return the Spanish menu item.
	 */
	public JMenuItem getSpanish() {
		return _spanish;
	}

	/**
	 * Sets a new value to the Spanish menu item.
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
	 * @param selectedLanguage new language to set.
	 */
	public void changeLanguage(String selectedLanguage){

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty("language", selectedLanguage);
		
		// Gets the labels
		ResourceBundle labels = AcideLanguage.getInstance().getLabels();
		AcideLog.getLog().info(labels.getString("s100"));
		
		// Resets the main window
		MainWindow.getInstance().getMenu().setLanguageLabels();
		MainWindow.getInstance().buildToolBar();
	
		// Updates the EXPLORER POPUP MENU
		MainWindow.getInstance().getExplorer().buildPopupMenu();
		
		// Updates the OUTPUT POPUP MENU
		MainWindow.getInstance().getOutput().buildPopupMenu();
		
		// Updates the STATUS BAR POPUP MENU
		MainWindow.getInstance().getStatusBar().buildPopupMenu();
		
		// Updates the EDITORS POPUP MENU
		int numEditors = MainWindow.getInstance().getFileEditorManager().getNumFileEditorPanels();
		for(int pos = 0; pos < numEditors; pos++)
			MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(pos).buildPopupMenu();
		
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// Resets the SEARCH GUI
		SearchWindow searchGUI = SearchWindow.getInstance();
		searchGUI.inicialize();
		searchGUI.validate();
		searchGUI.repaint();
		
		// Resets the REPLACE GUI
		ReplaceWindow replaceGUI = ReplaceWindow.getInstance();
		replaceGUI.inicialize();
		replaceGUI.validate();
		replaceGUI.repaint();
		
		// Get the labels
		labels = AcideLanguage.getInstance().getLabels();
		
		// Updates the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ LexiconConfiguration.getInstance().getName());
		try {
			
			// Gets the name
			String currentGrammar = ResourceManager
					.getInstance().getProperty("currentGrammar");
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
			AcideLog.getLog().error(exception.getMessage());
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}
		
		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
			
			// Enables the project menu
			MainWindow.getInstance().getMenu().enableProjectMenu();
			
			// The project configuration has been modified
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
		}

		// If there are opened editors
		if (MainWindow.getInstance().getFileEditorManager().getNumFileEditorPanels() > 0) {
			
			// Enables the file menu
			MainWindow.getInstance().getMenu().enableFileMenu();
			
			// Enables the edit menu
			MainWindow.getInstance().getMenu().enableEditMenu();
		}
	}
}