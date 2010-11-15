package gui.menu.configuration.lexicon;

import es.configuration.menu.MenuConfiguration;
import gui.menu.configuration.lexicon.listeners.LoadLexiconListener;
import gui.menu.configuration.lexicon.listeners.ModifyLexiconListener;
import gui.menu.configuration.lexicon.listeners.NewLexicalListener;
import gui.menu.configuration.lexicon.listeners.SaveAsLexiconListener;
import gui.menu.configuration.lexicon.listeners.SaveLexicalListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Lexicon menu of ACIDE - A Configurable IDE											
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
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Modify menu item
	 */
	private JMenuItem _modifyLexicon;
	/**
	 * New menu item
	 */
	private JMenuItem _newLexicon;
	/**
	 * Save menu item
	 */
	private JMenuItem _saveLexicon;
	/**
	 * Load menu item
	 */
	private JMenuItem _loadLexicon;
	/**
	 * Save as menu item
	 */
	private JMenuItem _saveAsLexicon;

	/**
	 * Class constructor
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
	 * Sets the language labels to display in the selected language
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
	 * Sets the menu item listeners
	 */
	public void setListeners() {

		// MODIFY
		_modifyLexicon.addActionListener(new ModifyLexiconListener());

		// LOAD
		_loadLexicon.addActionListener(new LoadLexiconListener());

		// SAVE
		_saveLexicon.addActionListener(new SaveLexicalListener());

		// SAVE AS
		_saveAsLexicon.addActionListener(new SaveAsLexiconListener());

		// NEW
		_newLexicon.addActionListener(new NewLexicalListener());
	}

	/**
	 * Builds the lexicon menu
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
	 * Returns the load parameters menu item
	 * 
	 * @return the load parameters menu item
	 */
	public JMenuItem getLoadParameters() {
		return _loadLexicon;
	}

	/**
	 * Sets a new value to the load parameters menu item
	 * 
	 * @param loadParameters
	 *            new value to set
	 */
	public void setLoadParameters(JMenuItem loadParameters) {
		_loadLexicon = loadParameters;
	}

	/**
	 * Returns the modify lexicon menu item
	 * 
	 * @return the modify lexicon menu item
	 */
	public JMenuItem getModifyLexicon() {
		return _modifyLexicon;
	}

	/**
	 * Sets a new value to modify lexicon menu item
	 * 
	 * @param modifyLexicon
	 *            new value to set
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
	 * Returns the save lexicon menu item
	 * 
	 * @return the save lexicon menu item
	 */
	public JMenuItem getSaveLexicon() {
		return _saveLexicon;
	}

	/**
	 * Sets a new value to the save lexicon menu item
	 * 
	 * @param saveLexicon
	 *            new value to set
	 */
	public void setSaveLexicon(JMenuItem saveLexicon) {
		_saveLexicon = saveLexicon;
	}

	/**
	 * Returns the save as lexicon menu item
	 * 
	 * @return the save as lexicon menu item
	 */
	public JMenuItem getSaveAsLexicon() {
		return _saveAsLexicon;
	}

	/**
	 * Sets a new value to the save as lexicon menu item
	 * 
	 * @param saveAsLexicon
	 *            new value to set
	 */
	public void setSaveAsLexicon(JMenuItem saveAsLexicon) {
		_saveAsLexicon = saveAsLexicon;
	}
}