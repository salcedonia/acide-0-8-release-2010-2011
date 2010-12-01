package gui.menuBar.configurationMenu.grammarMenu;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import es.configuration.menu.MenuConfiguration;
import gui.menuBar.configurationMenu.grammarMenu.listeners.AutoAnalysisMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.LoadGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.ModifyGrammaMenuItemrListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.NewGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.SaveAsGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.SaveGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.SetPathsMenuItemListener;

import language.AcideLanguage;

import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Grammar menu of ACIDE - A Configurable IDE.											
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
 * @see ActionListener																													
 ***********************************************************************/
public class GrammarMenu extends JMenu {

	/**
	 * Grammar menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New grammar menu item name.
	 */
	public static final String NEW_GRAMMAR_NAME = "New Grammar";
	/**
	 * Load grammar menu item name.
	 */
	public static final String LOAD_GRAMMAR_NAME = "Load Grammar";
	/**
	 * Modify grammar menu item name.
	 */
	public static final String MODIFY_GRAMMAR_NAME = "Modify Grammar";
	/**
	 * Save grammar menu item name.
	 */
	public static final String SAVE_GRAMMAR_NAME = "Save Grammar";
	/**
	 * Save grammar as menu item name.
	 */
	public static final String SAVE_GRAMMAR_AS_NAME = "Save Grammar As";
	/**
	 * Set paths menu item name.
	 */
	public static final String SET_PATHS_NAME = "Set Paths";
	/**
	 * Auto-Analysis menu item name.
	 */
	public static final String AUTO_ANALYSIS_NAME = "Auto-Analysis";
	/**
	 * New grammar menu item.
	 */
	private JMenuItem _newGrammar;
	/**
	 * Load grammar menu item.
	 */
	private JMenuItem _loadGrammar;
	/**
	 * Modify menu item.
	 */
	private JMenuItem _modifyGrammar;
	/**
	 * Save menu item.
	 */
	private JMenuItem _saveGrammar;
	/**
	 * Save as grammar menu item.
	 */
	private JMenuItem _saveAsGrammar;
	/**
	 * Set paths menu item.
	 */
	private JMenuItem _setPaths;
	/**
	 * Auto analysis check box menu item.
	 */
	private JCheckBoxMenuItem _autoAnalysis;
	
	/**
	 * Creates a new grammar menu.
	 */
	public GrammarMenu(){
		
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
		
		// MENU ITEM
		_newGrammar = new JMenuItem();
		_loadGrammar = new JMenuItem();
		_modifyGrammar = new JMenuItem();
		_saveGrammar = new JMenuItem();
		_saveGrammar.setEnabled(false);
		_saveAsGrammar = new JMenuItem();
		_setPaths = new JMenuItem();
		_autoAnalysis = new JCheckBoxMenuItem(labels.getString("s911"));
		_autoAnalysis.setSelected(false);
		
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
		
		// NEW
		_newGrammar.setText(labels.getString("s30"));
		_newGrammar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// LOAD
		_loadGrammar.setText(labels.getString("s226"));
		
		// MODIFY
		_modifyGrammar.setText(labels.getString("s227"));
		
		// SAVE
		_saveGrammar.setText(labels.getString("s251"));
		
		// SAVE AS
		_saveAsGrammar.setText(labels.getString("s285"));
		
		// SET PATHS
		_setPaths.setText(labels.getString("s912"));
	
		// AUTO GRAMMAR ANALYSIS
		_autoAnalysis.setText(labels.getString("s911"));
	}
	
	/**
	 * Sets the grammar menu item listeners.
	 */
	public void setListeners(){
		
		// NEW GRAMMAR
		_newGrammar.addActionListener(new NewGrammarMenuItemListener());
		
		// LOAD GRAMMAR
		_loadGrammar.addActionListener(new LoadGrammarMenuItemListener());
		
		// MODIFY GRAMMAR
		_modifyGrammar.addActionListener(new ModifyGrammaMenuItemrListener());	
		
		// SAVE GRAMMAR
		_saveGrammar.addActionListener(new SaveGrammarMenuItemListener());
		
		// SAVE AS GRAMMAR
		_saveAsGrammar.addActionListener(new SaveAsGrammarMenuItemListener());
		
		// SET PATHS
		_setPaths.addActionListener(new SetPathsMenuItemListener());
		
		// AUTO GRAMMAR ANALYSIS
		_autoAnalysis.addActionListener(new AutoAnalysisMenuItemListener());
	}
	
	/**
	 * Builds the grammar menu.
	 */
	public void buildMenu() {
		
		removeAll();
		
		// NEW GRAMMAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME))
			add(_newGrammar);
		
		// LOAD GRAMMAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(LOAD_GRAMMAR_NAME))
			add(_loadGrammar);
		
		// MODIFY GRAMMAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(MODIFY_GRAMMAR_NAME))
			add(_modifyGrammar);
		
		// SAVE GRAMMAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_NAME))
			add(_saveGrammar);
		
		// SAVE AS GRAMMAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME))
			add(_saveAsGrammar);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LOAD_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(MODIFY_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME)) 
				&& (MenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME)))
			addSeparator();
		
		// SET PATHS MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME))
			add(_setPaths);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LOAD_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(MODIFY_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME)) 
				&& (MenuConfiguration.getInstance().getIsDisplayed(AUTO_ANALYSIS_NAME)))
			addSeparator();
		
		// AUTO-ANALYSIS
		if (MenuConfiguration.getInstance().getIsDisplayed(AUTO_ANALYSIS_NAME))
			add(_autoAnalysis);		
	}
	
	/**
	 * Returns the load grammar menu item.
	 * 
	 * @return the load grammar menu item.
	 */
	public JMenuItem getLoadGrammar() {
		return _loadGrammar;
	}

	/**
	 * Sets a new value to the load grammar menu item.
	 * 
	 * @param loadGrammar new value to set.
	 */ 
	public void setLoadGrammar(JMenuItem loadGrammar) {
		_loadGrammar = loadGrammar;
	}
	
	/**
	 * Returns the modify grammar menu item.
	 * 
	 * @return the modify grammar menu item.
	 */
	public JMenuItem getModifyGrammar() {
		return _modifyGrammar;
	}

	/**
	 * Sets a new value to the modify grammar menu item.
	 * 
	 * @param modifyGrammar new value to set.
	 */
	public void setModifyGrammar(JMenuItem modifyGrammar) {
		_modifyGrammar = modifyGrammar;
	}

	/**
	 * Returns the new grammar menu item.
	 * 
	 * @return the new grammar menu item.
	 */
	public JMenuItem getNewGrammar() {
		return _newGrammar;
	}

	/**
	 * Sets a new value to the new grammar menu item.
	 * 
	 * @param newGrammar new value to set.
	 */
	public void setNewGrammar(JMenuItem newGrammar) {
		_newGrammar = newGrammar;
	}
	
	/**
	 * Returns the save grammar menu item.
	 * 
	 * @return the save grammar menu item.
	 */
	public JMenuItem getSaveGrammar() {
		return _saveGrammar;
	}

	/**
	 * Sets a new value to the save grammar menu item.
	 * 
	 * @param saveGrammar new value to set.
	 */
	public void setSaveGrammar(JMenuItem saveGrammar) {
		_saveGrammar = saveGrammar;
	}
	
	/**
	 * Returns the save as grammar menu item.
	 * 
	 * @return the save as grammar menu item.
	 */ 
	public JMenuItem getSaveAsGrammar() {
		return _saveAsGrammar;
	}

	/**
	 * Sets a new value to the save as grammar menu item.
	 * 
	 * @param saveAsGrammar new value to set.
	 */
	public void setSaveAsGrammar(JMenuItem saveAsGrammar) {
		_saveAsGrammar = saveAsGrammar;
	}
}