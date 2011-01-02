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
package gui.menuBar.configurationMenu.lexiconMenu;

import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.LoadLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.ModifyLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.NewLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.SaveAsLexiconMenuItemListener;
import gui.menuBar.configurationMenu.lexiconMenu.listeners.SaveLexiconMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE lexicon menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class LexiconMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE lexicon menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item name.
	 */
	public static final String NEW_LEXICON_NAME = "New Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu modify lexicon menu item name.
	 */
	public static final String MODIFY_LEXICON_NAME = "Modify Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon menu item name.
	 */
	public static final String SAVE_LEXICON_NAME = "Save Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu load lexicon menu item name.
	 */
	public static final String LOAD_LEXICON_NAME = "Load Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon as menu item name.
	 */
	public static final String SAVE_LEXICON_AS_NAME = "Save Lexicon As";
	/**
	 * ACIDE - A Configurable IDE lexicon menu new menu item.
	 */
	private JMenuItem _newLexicon;
	/**
	 * ACIDE - A Configurable IDE lexicon menu modify menu item.
	 */
	private JMenuItem _modifyLexicon;
	/**
	 * ACIDE - A Configurable IDE lexicon menu save menu item.
	 */
	private JMenuItem _saveLexicon;
	/**
	 * ACIDE - A Configurable IDE lexicon menu load menu item.
	 */
	private JMenuItem _loadLexicon;
	/**
	 * ACIDE - A Configurable IDE lexicon menu save as menu item.
	 */
	private JMenuItem _saveAsLexicon;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon menu.
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
	 * Sets the language labels ACIDE - A Configurable IDE lexicon menu.
	 */
	public void setLanguageLabels() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
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
	 * Sets the lACIDE - A Configurable IDE lexicon menu menu item listeners.
	 */
	public void setListeners() {

		// MODIFY
		_modifyLexicon.addActionListener(new ModifyLexiconMenuItemListener());

		// LOAD
		_loadLexicon.addActionListener(new LoadLexiconMenuItemListener());

		// SAVE
		_saveLexicon.addActionListener(new SaveLexiconMenuItemListener());

		// SAVE AS
		_saveAsLexicon.addActionListener(new SaveAsLexiconMenuItemListener());

		// NEW
		_newLexicon.addActionListener(new NewLexiconMenuItemListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void buildMenu() {

		removeAll();

		// NEW LEXICON MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_LEXICON_NAME))
			add(_newLexicon);

		// LOAD LEXICON MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_LEXICON_NAME))
			add(_loadLexicon);

		// MODIFY LEXICON MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_LEXICON_NAME))
			add(_modifyLexicon);

		// SAVE LEXICON MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_LEXICON_NAME))
			add(_saveLexicon);

		// SAVE AS LEXICON
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_LEXICON_AS_NAME))
			add(_saveAsLexicon);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu load parameters menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu load parameters menu
	 *         item.
	 */
	public JMenuItem getLoadParameters() {
		return _loadLexicon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon menu load
	 * parameters menu item.
	 * 
	 * @param loadParameters
	 *            new value to set.
	 */
	public void setLoadParameters(JMenuItem loadParameters) {
		_loadLexicon = loadParameters;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu modify lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu modify lexicon menu
	 *         item.
	 */
	public JMenuItem getModifyLexicon() {
		return _modifyLexicon;
	}

	/**
	 * Sets a new value to ACIDE - A Configurable IDE lexicon menu modify
	 * lexicon menu item.
	 * 
	 * @param modifyLexicon
	 *            new value to set.
	 */
	public void setModifyLexicon(JMenuItem modifyLexicon) {
		_modifyLexicon = modifyLexicon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu new lexicon menu item
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu new lexicon menu item
	 */
	public JMenuItem getNewLexicon() {
		return _newLexicon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon menu new
	 * lexicon menu item
	 * 
	 * @param newLexicon
	 *            new value to set
	 */
	public void setNewLexicon(JMenuItem newLexicon) {
		_newLexicon = newLexicon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu save lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu save lexicon menu
	 *         item.
	 */
	public JMenuItem getSaveLexicon() {
		return _saveLexicon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon menu save
	 * lexicon menu item.
	 * 
	 * @param saveLexicon
	 *            new value to set.
	 */
	public void setSaveLexicon(JMenuItem saveLexicon) {
		_saveLexicon = saveLexicon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu save as lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu save as lexicon menu
	 *         item.
	 */
	public JMenuItem getSaveAsLexicon() {
		return _saveAsLexicon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon menu save as
	 * lexicon menu item.
	 * 
	 * @param saveAsLexicon
	 *            new value to set.
	 */
	public void setSaveAsLexicon(JMenuItem saveAsLexicon) {
		_saveAsLexicon = saveAsLexicon;
	}
}