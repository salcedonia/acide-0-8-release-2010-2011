package gui.menu.configuration.grammar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import es.configuration.menu.MenuConfiguration;
import gui.menu.configuration.grammar.listeners.AutoAnalysisListener;
import gui.menu.configuration.grammar.listeners.LoadGrammarListener;
import gui.menu.configuration.grammar.listeners.ModifyGrammarListener;
import gui.menu.configuration.grammar.listeners.NewGrammarListener;
import gui.menu.configuration.grammar.listeners.SaveAsGrammarListener;
import gui.menu.configuration.grammar.listeners.SaveGrammarListener;
import gui.menu.configuration.grammar.listeners.SetPathsListener;

import language.Language;

import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Grammar menu of ACIDE - A Configurable IDE											
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
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New grammar menu item
	 */
	private JMenuItem _newGrammar;
	/**
	 * Load grammar menu item
	 */
	private JMenuItem _loadGrammar;
	/**
	 * Modify menu item
	 */
	private JMenuItem _modifyGrammar;
	/**
	 * Save menu item
	 */
	private JMenuItem _saveGrammar;
	/**
	 * Save as grammar menu item
	 */
	private JMenuItem _saveAsGrammar;
	/**
	 * Set paths menu item
	 */
	private JMenuItem _setPaths;
	/**
	 * Auto analysis check box menu item
	 */
	private JCheckBoxMenuItem _autoAnalysis;
	
	/**
	 * Class constructor
	 */
	public GrammarMenu(){
		
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
	 * Sets the menu item listeners
	 */
	public void setListeners(){
		
		// NEW GRAMMAR
		_newGrammar.addActionListener(new NewGrammarListener());
		
		// LOAD GRAMMAR
		_loadGrammar.addActionListener(new LoadGrammarListener());
		
		// MODIFY GRAMMAR
		_modifyGrammar.addActionListener(new ModifyGrammarListener());	
		
		// SAVE GRAMMAR
		_saveGrammar.addActionListener(new SaveGrammarListener());
		
		// SAVE AS GRAMMAR
		_saveAsGrammar.addActionListener(new SaveAsGrammarListener());
		
		// SET PATHS
		_setPaths.addActionListener(new SetPathsListener());
		
		// AUTO GRAMMAR ANALYSIS
		_autoAnalysis.addActionListener(new AutoAnalysisListener());
	}
	
	/**
	 * Builds the grammar menu
	 */
	public void buildMenu() {
		
		removeAll();
		
		if (MenuConfiguration.getNewGrammar())
			add(_newGrammar);
		if (MenuConfiguration.getLoadGrammar())
			add(_loadGrammar);
		if (MenuConfiguration.getModifyGrammar())
			add(_modifyGrammar);
		if (MenuConfiguration.getSaveGrammar())
			add(_saveGrammar);
		if (MenuConfiguration.getSaveAsGrammar())
			add(_saveAsGrammar);
		if ((MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getSaveGrammar() || MenuConfiguration
				.getSaveAsGrammar()) && (MenuConfiguration.getSetPaths()))
			addSeparator();
		if (MenuConfiguration.getSetPaths())
			add(_setPaths);
		if ((MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getSaveGrammar()
				|| MenuConfiguration.getSaveAsGrammar() || MenuConfiguration
				.getSetPaths()) && (MenuConfiguration.getAutoGrammarAnalysis()))
			addSeparator();
		if (MenuConfiguration.getAutoGrammarAnalysis())
			add(_autoAnalysis);		
	}
	
	/**
	 * Returns the load grammar menu item
	 * 
	 * @return the load grammar menu item
	 */
	public JMenuItem getLoadGrammar() {
		return _loadGrammar;
	}

	/**
	 * Sets a new value to the load grammar menu item
	 * 
	 * @param loadGrammar new value to set
	 */ 
	public void setLoadGrammar(JMenuItem loadGrammar) {
		_loadGrammar = loadGrammar;
	}
	
	/**
	 * Returns the modify grammar menu item
	 * 
	 * @return the modify grammar menu item
	 */
	public JMenuItem getModifyGrammar() {
		return _modifyGrammar;
	}

	/**
	 * Sets a new value to the modify grammar menu item
	 * 
	 * @param modifyGrammar new value to set
	 */
	public void setModifyGrammar(JMenuItem modifyGrammar) {
		_modifyGrammar = modifyGrammar;
	}

	/**
	 * Returns the new grammar menu item
	 * 
	 * @return the new grammar menu item
	 */
	public JMenuItem getNewGrammar() {
		return _newGrammar;
	}

	/**
	 * Sets a new value to the new grammar menu item
	 * 
	 * @param newGrammar new value to set
	 */
	public void setNewGrammar(JMenuItem newGrammar) {
		_newGrammar = newGrammar;
	}
	
	/**
	 * Returns the save grammar menu item
	 * 
	 * @return the save grammar menu item
	 */
	public JMenuItem getSaveGrammar() {
		return _saveGrammar;
	}

	/**
	 * Sets a new value to the save grammar menu item
	 * 
	 * @param saveGrammar new value to set
	 */
	public void setSaveGrammar(JMenuItem saveGrammar) {
		_saveGrammar = saveGrammar;
	}
	
	/**
	 * Returns the save as grammar menu item
	 * 
	 * @return the save as grammar menu item
	 */ 
	public JMenuItem getSaveAsGrammar() {
		return _saveAsGrammar;
	}

	/**
	 * Sets a new value to the save as grammar menu item
	 * 
	 * @param saveAsGrammar new value to set
	 */
	public void setSaveAsGrammar(JMenuItem saveAsGrammar) {
		_saveAsGrammar = saveAsGrammar;
	}
}