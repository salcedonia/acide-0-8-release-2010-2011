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
package gui.menuBar.configurationMenu.toolBarMenu;

import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.LoadToolBarMenuItemListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.ModifyToolBarMenuItemListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.NewToolBarMenuItemListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.SaveAsToolBaMenuItemrListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.SaveToolBarMenuItemListener;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE tool bar menu.
 *					
 * @version 0.8
 * @see JMenu																														
 */
public class ToolBarMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE tool bar menu tool bar menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar menu new tool bar menu item name.
	 */
	public static final String NEW_TOOLBAR_NAME = "New Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu load tool bar menu item name.
	 */
	public static final String LOAD_TOOLBAR_NAME = "Load Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu modify tool bar menu item name.
	 */
	public static final String MODIFY_TOOLBAR_NAME = "Modify Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar menu item name.
	 */
	public static final String SAVE_TOOLBAR_NAME = "Save Toolbar";
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar as menu item name.
	 */
	public static final String SAVE_TOOLBAR_AS_NAME = "Save Toolbar As";
	/**
	 * ACIDE - A Configurable IDE tool bar menu new tool bar menu item.
	 */
	private JMenuItem _newToolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar menu load tool bar menu item.
	 */
	private JMenuItem _loadToolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar menu modify tool bar menu item.
	 */
	private JMenuItem _modifyToolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar menu save tool bar menu item.
	 */
	private JMenuItem _saveToolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar menu save as tool menu item.
	 */
	private JMenuItem _saveAsToolBar;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar menu.
	 */
	public ToolBarMenu(){
				
		// MENU ITEM
		_newToolBar = new JMenuItem();
		_loadToolBar = new JMenuItem();	
		_modifyToolBar = new JMenuItem();
		_saveToolBar = new JMenuItem();
		_saveToolBar.setEnabled(false);
		_saveAsToolBar = new JMenuItem();
		
		setLanguageLabels();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE tool bar menu language labels.
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
		final ResourceBundle labels = language.getLabels();
		
		// NEW
		_newToolBar.setText(labels.getString("s280"));
		
		// LOAD
		_loadToolBar.setText(labels.getString("s281"));
		
		// MODIFY
		_modifyToolBar.setText(labels.getString("s282"));
		
		// SAVE
		_saveToolBar.setText(labels.getString("s283"));
		
		// SAVE AS
		_saveAsToolBar.setText(labels.getString("s284"));
	}
	
	/**
	 * Sets the ACIDE - A Configurable IDE tool bar menu menu item listeners.
	 */
	public void setListeners(){
		
		// NEW
		_newToolBar.addActionListener(new NewToolBarMenuItemListener());
		
		// LOAD
		_loadToolBar.addActionListener(new LoadToolBarMenuItemListener());
		
		// MODIFY
		_modifyToolBar.addActionListener(new ModifyToolBarMenuItemListener());	
		
		// SAVE
		_saveToolBar.addActionListener(new SaveToolBarMenuItemListener());
		
		// SAVE AS
		_saveAsToolBar.addActionListener(new SaveAsToolBaMenuItemrListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE tool bar menu.
	 */
	public void buildMenu(){
		
		removeAll();
		
		// NEW TOOL BAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_TOOLBAR_NAME))
			add(_newToolBar);
		
		// LOAD TOOL BAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LOAD_TOOLBAR_NAME))
			add(_loadToolBar);
		
		// MODIFY TOOL BAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(MODIFY_TOOLBAR_NAME))
			add(_modifyToolBar);
		
		// SAVE TOOL BAR MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_TOOLBAR_NAME))
			add(_saveToolBar);
		
		// SAVE AS TOOL BAR
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_TOOLBAR_AS_NAME))
			add(_saveAsToolBar);
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu load tool bar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu load tool bar menu item.
	 */
	public JMenuItem getLoadToolBar() {
		return _loadToolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar menu load tool bar menu item.
	 * 
	 * @param loadToolBar new value to set.
	 */
	public void setLoadToolBar(JMenuItem loadToolBar) {
		_loadToolBar = loadToolBar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu modify tool bar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu modify tool bar menu item.
	 */
	public JMenuItem getModifyToolBar() {
		return _modifyToolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar menu modify tool bar menu item.
	 * 
	 * @param modifyToolBar new value to set.
	 */
	public void setModifyToolBar(JMenuItem modifyToolBar) {
		_modifyToolBar = modifyToolBar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu new tool bar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu new tool bar menu item.
	 */
	public JMenuItem getNewToolBar() {
		return _newToolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar menu new tool bar menu item.
	 * 
	 * @param newToolBar new value to set.
	 */
	public void setNewToolBar(JMenuItem newToolBar) {
		_newToolBar = newToolBar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu save as tool bar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu save as tool bar menu item.
	 */
	public JMenuItem getSaveAsToolBar() {
		return _saveAsToolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar menu save as tool bar menu item
	 * 
	 * @param saveAsToolBar new value to set
	 */
	public void setSaveAsToolBar(JMenuItem saveAsToolBar) {
		_saveAsToolBar = saveAsToolBar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE tool bar menu save tool bar menu item
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar menu save tool bar menu item
	 */
	public JMenuItem getSaveToolBar() {
		return _saveToolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar menu save tool bar menu item.
	 * 
	 * @param saveToolBar new value to set.
	 */
	public void setSaveToolBar(JMenuItem saveToolBar) {
		_saveToolBar = saveToolBar;
	}
}