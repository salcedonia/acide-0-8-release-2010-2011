/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package gui.menuBar.configurationMenu.grammarMenu;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.configurationMenu.grammarMenu.listeners.AutoAnalysisMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.LoadGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.ModifyGrammaMenuItemrListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.NewGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.SaveAsGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.SaveGrammarMenuItemListener;
import gui.menuBar.configurationMenu.grammarMenu.listeners.SetPathsMenuItemListener;

import language.AcideLanguageManager;

import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE grammar menu.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class GrammarMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE grammar menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE grammar menu new grammar menu item name.
	 */
	public static final String NEW_GRAMMAR_NAME = "New Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu load grammar menu item name.
	 */
	public static final String LOAD_GRAMMAR_NAME = "Load Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu modify grammar menu item name.
	 */
	public static final String MODIFY_GRAMMAR_NAME = "Modify Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu save grammar menu item name.
	 */
	public static final String SAVE_GRAMMAR_NAME = "Save Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu save grammar as menu item name.
	 */
	public static final String SAVE_GRAMMAR_AS_NAME = "Save Grammar As";
	/**
	 * ACIDE - A Configurable IDE grammar menu set paths menu item name.
	 */
	public static final String SET_PATHS_NAME = "Set Paths";
	/**
	 * ACIDE - A Configurable IDE grammar menu suto-Analysis menu item name.
	 */
	public static final String AUTO_ANALYSIS_NAME = "Auto-Analysis";
	/**
	 * ACIDE - A Configurable IDE grammar menu new grammar menu item.
	 */
	private JMenuItem _newGrammar;
	/**
	 * ACIDE - A Configurable IDE grammar menu load grammar menu item.
	 */
	private JMenuItem _loadGrammar;
	/**
	 * ACIDE - A Configurable IDE grammar menu modify menu item.
	 */
	private JMenuItem _modifyGrammar;
	/**
	 * ACIDE - A Configurable IDE grammar menu save menu item.
	 */
	private JMenuItem _saveGrammar;
	/**
	 * ACIDE - A Configurable IDE grammar menu save as grammar menu item.
	 */
	private JMenuItem _saveAsGrammar;
	/**
	 * ACIDE - A Configurable IDE grammar menu set paths menu item.
	 */
	private JMenuItem _setPaths;
	/**
	 * ACIDE - A Configurable IDE grammar menu auto analysis check box menu item.
	 */
	private JCheckBoxMenuItem _autoAnalysis;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE grammar menu.
	 */
	public GrammarMenu(){
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
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
	 * Sets the ACIDE - A Configurable IDE grammar menu language labels.
	 */
	public void setLanguageLabels() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
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
	 * Sets the ACIDE - A Configurable IDE grammar menu item listeners.
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
	 * Builds the ACIDE - A Configurable IDE grammar menu.
	 */
	public void buildMenu() {
		
		removeAll();
		
		// NEW GRAMMAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME))
			add(_newGrammar);
		
		// LOAD GRAMMAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_GRAMMAR_NAME))
			add(_loadGrammar);
		
		// MODIFY GRAMMAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_GRAMMAR_NAME))
			add(_modifyGrammar);
		
		// SAVE GRAMMAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_NAME))
			add(_saveGrammar);
		
		// SAVE AS GRAMMAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME))
			add(_saveAsGrammar);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME)) 
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME)))
			addSeparator();
		
		// SET PATHS MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME))
			add(_setPaths);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME)) 
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(AUTO_ANALYSIS_NAME)))
			addSeparator();
		
		// AUTO-ANALYSIS
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(AUTO_ANALYSIS_NAME))
			add(_autoAnalysis);		
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu load grammar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu load grammar menu item.
	 */
	public JMenuItem getLoadGrammar() {
		return _loadGrammar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE grammar menu load grammar menu item.
	 * 
	 * @param loadGrammar new value to set.
	 */ 
	public void setLoadGrammar(JMenuItem loadGrammar) {
		_loadGrammar = loadGrammar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu modify grammar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu modify grammar menu item.
	 */
	public JMenuItem getModifyGrammar() {
		return _modifyGrammar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE grammar menu modify grammar menu item.
	 * 
	 * @param modifyGrammar new value to set.
	 */
	public void setModifyGrammar(JMenuItem modifyGrammar) {
		_modifyGrammar = modifyGrammar;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu new grammar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu new grammar menu item.
	 */
	public JMenuItem getNewGrammar() {
		return _newGrammar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE grammar menu new grammar menu item.
	 * 
	 * @param newGrammar new value to set.
	 */
	public void setNewGrammar(JMenuItem newGrammar) {
		_newGrammar = newGrammar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu save grammar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu save grammar menu item.
	 */
	public JMenuItem getSaveGrammar() {
		return _saveGrammar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE grammar menu save grammar menu item.
	 * 
	 * @param saveGrammar new value to set.
	 */
	public void setSaveGrammar(JMenuItem saveGrammar) {
		_saveGrammar = saveGrammar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu save as grammar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu save as grammar menu item.
	 */ 
	public JMenuItem getSaveAsGrammar() {
		return _saveAsGrammar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE grammar menu save as grammar menu item.
	 * 
	 * @param saveAsGrammar new value to set.
	 */
	public void setSaveAsGrammar(JMenuItem saveAsGrammar) {
		_saveAsGrammar = saveAsGrammar;
	}
}