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
package gui.menuBar.configurationMenu.consoleMenu;

import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.configurationMenu.consoleMenu.listeners.ConfigureMenuItemListener;
import gui.menuBar.configurationMenu.consoleMenu.listeners.ExternalCommandMenuItemListener;
import gui.menuBar.configurationMenu.consoleMenu.listeners.ConsoleDisplayOptionsMenuItemListener;

/**																
 * ACIDE - A Configurable IDE console menu.											
 *					
 * @version 0.8	
 * @see JMenu																													
 */
public class ConsoleMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE console menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item name.
	 */
	public static final String CONFIGURE_NAME = "Configure";
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item name.
	 */
	public static final String EXTERNAL_COMMAND_NAME = "External Command";
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu item name.
	 */
	public static final String CONSOLE_DISPLAY_OPTIONS_NAME = "Console Display Options";
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item image icon.
	 */
	private final static ImageIcon CONFIGURE_IMAGE = new ImageIcon("./resources/icons/menu/configuration/console/configure.png");
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item image icon.
	 */
	private final static ImageIcon EXTERNAL_COMMAND_IMAGE = new ImageIcon("./resources/icons/menu/configuration/console/externalCommand.png");
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu item image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon("./resources/icons/menu/configuration/console/consoleDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item.
	 */
	private JMenuItem _configure;
	/**
	 * ACIDE - A Configurable IDE console menu external command item.
	 */
	private JMenuItem _externalCommand;
	/**
	 * ACIDE - A Configurable IDE console menu console display options.
	 */
	private JMenuItem _consoleDisplayOptions;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE console menu.
	 */
	public ConsoleMenu(){
				
		// MENU ITEM
		_configure = new JMenuItem(CONFIGURE_IMAGE);
		_externalCommand = new JMenuItem(EXTERNAL_COMMAND_IMAGE);
		_consoleDisplayOptions = new JMenuItem(CONSOLE_DISPLAY_OPTIONS_IMAGE);
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the ACIDE - A Configurable IDE console menu language labels.
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
		
		// CONFIGURE
		_configure.setText(labels.getString("s333"));
		
		// CONFIGURE
		_externalCommand.setText(labels.getString("s341"));
		
		// CONSOLE DISPLAY OPTIONS
		_consoleDisplayOptions.setText(labels.getString("s977"));
	}
	
	/**
	 * Builds the ACIDE - A Configurable IDE Console menu.
	 */
	public void buildMenu() {
		
		removeAll();
		
		// CONFIGURE MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(CONFIGURE_NAME))
			add(_configure);
		
		// EXTERNAL COMMAND
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EXTERNAL_COMMAND_NAME))
			add(_externalCommand);
		
		// CONSOLE DISPLAY OPTIONS
		if(AcideMenuConfiguration.getInstance().getIsDisplayed(CONSOLE_DISPLAY_OPTIONS_NAME))
			add(_consoleDisplayOptions);
	}
	
	/**
	 * Sets ACIDE - A Configurable IDE console menu item listeners.
	 */
	public void setListeners(){
		
		// CONFIGURE
		_configure.addActionListener(new ConfigureMenuItemListener());
		
		// EXTERNAL COMMAND
		_externalCommand.addActionListener(new ExternalCommandMenuItemListener());
		
		// CONSOLE DISPLAY OPTIONS
		_consoleDisplayOptions.addActionListener(new ConsoleDisplayOptionsMenuItemListener());
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE console menu external command menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu external command menu item.
	 */
	public JMenuItem getExternalCommand() {
		return _externalCommand;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console menu external command menu item.
	 * 
	 * @param externalCommand new value to set.
	 */ 
	public void setExternalCommand(JMenuItem externalCommand) {
		_externalCommand = externalCommand;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE console menu configure menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu configure menu item.
	 */
	public JMenuItem getConfigure() {
		return _configure;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console menu configure menu item.
	 * 
	 * @param configure new value to set.
	 */
	public void setConfigure(JMenuItem configure) {
		_configure = configure;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE console menu console display options menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu console display options menu item.
	 */
	public JMenuItem getConsoleDisplayOptions(){
		return _consoleDisplayOptions;
	}
	
	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console menu console display options menu item.
	 * 
	 * @param consoleDisplayOptions new value to set
	 */
	public void setConsoleDisplayOptions(JMenuItem consoleDisplayOptions) {
		_consoleDisplayOptions = consoleDisplayOptions;
	}
}