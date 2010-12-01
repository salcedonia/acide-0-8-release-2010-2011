package gui.menuBar.configurationMenu.lexiconMenu;

import es.configuration.menu.MenuConfiguration;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.LoadLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.ModifyLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.NewLexicalMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.SaveAsLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.SaveLexicalMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Lexicon menu of ACIDE - A Configurable IDE.											
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
public class LexiconMenu extends JMenu {

	/**
	 * Lexicon menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New lexicon menu item name.
	 */
	public static final String NEW_LEXICON_NAME = "New Lexicon";
	/**
	 * Modify lexicon menu item name.
	 */
	public static final String MODIFY_LEXICON_NAME = "Modify Lexicon";
	/**
	 * Save lexicon menu item name.
	 */
	public static final String SAVE_LEXICON_NAME = "Save Lexicon";
	/**
	 * Load lexicon menu item name.
	 */
	public static final String LOAD_LEXICON_NAME = "Load Lexicon";
	/**
	 * Save lexicon as menu item name.
	 */
	public static final String SAVE_LEXICON_AS_NAME = "Save Lexicon As";
	/**
	 * New menu item.
	 */
	private JMenuItem _newLexicon;
	/**
	 * Modify menu item.
	 */
	private JMenuItem _modifyLexicon;
	/**
	 * Save menu item.
	 */
	private JMenuItem _saveLexicon;
	/**
	 * Load menu item.
	 */
	private JMenuItem _loadLexicon;
	/**
	 * Save as menu item.
	 */
	private JMenuItem _saveAsLexicon;

	/**
	 * Creates a new lexicon menu.
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
	 * Sets the language labels to display in the selected language.
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
	 * Sets the lexicon menu item listeners.
	 */
	public void setListeners() {

		// MODIFY
		_modifyLexicon.addActionListener(new ModifyLexiconMenuItemListener());

		// LOAD
		_loadLexicon.addActionListener(new LoadLexiconMenuItemListener());

		// SAVE
		_saveLexicon.addActionListener(new SaveLexicalMenuItemListener());

		// SAVE AS
		_saveAsLexicon.addActionListener(new SaveAsLexiconMenuItemListener());

		// NEW
		_newLexicon.addActionListener(new NewLexicalMenuItemListener());
	}

	/**
	 * Builds the lexicon menu.
	 */
	public void buildMenu() {

		removeAll();

		// NEW LEXICON MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(NEW_LEXICON_NAME))
			add(_newLexicon);
		
		// LOAD LEXICON MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(LOAD_LEXICON_NAME))
			add(_loadLexicon);
		
		// MODIFY LEXICON MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(MODIFY_LEXICON_NAME))
			add(_modifyLexicon);
		
		// SAVE LEXICON MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_LEXICON_NAME))
			add(_saveLexicon);
		
		// SAVE AS LEXICON
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_LEXICON_AS_NAME))
			add(_saveAsLexicon);
	}

	/**
	 * Returns the load parameters menu item.
	 * 
	 * @return the load parameters menu item.
	 */
	public JMenuItem getLoadParameters() {
		return _loadLexicon;
	}

	/**
	 * Sets a new value to the load parameters menu item.
	 * 
	 * @param loadParameters
	 *            new value to set.
	 */
	public void setLoadParameters(JMenuItem loadParameters) {
		_loadLexicon = loadParameters;
	}

	/**
	 * Returns the modify lexicon menu item.
	 * 
	 * @return the modify lexicon menu item.
	 */
	public JMenuItem getModifyLexicon() {
		return _modifyLexicon;
	}

	/**
	 * Sets a new value to modify lexicon menu item.
	 * 
	 * @param modifyLexicon
	 *            new value to set.
	 */
	public void setModifyLexicon(JMenuItem modifyLexicon) {
		_modifyLexicon = modifyLexicon;
	}

	/**
	 * Returns the new lexicon menu item
	 * 
	 * @return the new lexicon menu item
	 */
	public JMenuItem getNewLexicon() {
		return _newLexicon;
	}

	/**
	 * Sets a new value to the new lexicon menu item
	 * 
	 * @param newLexicon
	 *            new value to set
	 */
	public void setNewLexicon(JMenuItem newLexicon) {
		_newLexicon = newLexicon;
	}

	/**
	 * Returns the save lexicon menu item.
	 * 
	 * @return the save lexicon menu item.
	 */
	public JMenuItem getSaveLexicon() {
		return _saveLexicon;
	}

	/**
	 * Sets a new value to the save lexicon menu item.
	 * 
	 * @param saveLexicon
	 *            new value to set.
	 */
	public void setSaveLexicon(JMenuItem saveLexicon) {
		_saveLexicon = saveLexicon;
	}

	/**
	 * Returns the save as lexicon menu item.
	 * 
	 * @return the save as lexicon menu item.
	 */
	public JMenuItem getSaveAsLexicon() {
		return _saveAsLexicon;
	}

	/**
	 * Sets a new value to the save as lexicon menu item.
	 * 
	 * @param saveAsLexicon
	 *            new value to set.
	 */
	public void setSaveAsLexicon(JMenuItem saveAsLexicon) {
		_saveAsLexicon = saveAsLexicon;
	}
}