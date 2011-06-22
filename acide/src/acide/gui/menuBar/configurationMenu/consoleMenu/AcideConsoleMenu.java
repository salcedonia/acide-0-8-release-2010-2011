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
package acide.gui.menuBar.configurationMenu.consoleMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideCloseConsoleMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideConfigureMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideConsoleDisplayOptionsMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideDocumentConsoleLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideExternalCommandMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideSaveConsoleContentIntoFileMenuItemListener;
import acide.gui.menuBar.configurationMenu.consoleMenu.listeners.AcideSearchConsoleMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE console menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideConsoleMenu extends JMenu {

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
	 * ACIDE - A Configurable IDE console menu console display options menu item
	 * name.
	 */
	public static final String CONSOLE_DISPLAY_OPTIONS_NAME = "Console Display Options";
	/**
	 * ACIDE - A Configurable IDE console menu save console content into file
	 * menu item name.
	 */
	public static final String SAVE_CONSOLE_CONTENT_INTO_FILE_NAME = "Save Console Content Into File";
	/**
	 * ACIDE - A Configurable IDE console menu document console lexicon menu
	 * item name.
	 */
	public static final String DOCUMENT_CONSOLE_LEXICON_NAME = "Document Console Lexicon";
	/**
	 * ACIDE - A Configurable IDE console menu search console menu item name.
	 */
	public static final String SEARCH_CONSOLE_NAME = "Search Console";
	/**
	 * ACIDE - A Configurable IDE console menu close console menu item name.
	 */
	public static final String CLOSE_CONSOLE_NAME = "Close Console";
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item image icon.
	 */
	private final static ImageIcon CONFIGURE_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/configure.png");
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item image
	 * icon.
	 */
	private final static ImageIcon EXTERNAL_COMMAND_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/externalCommand.png");
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu item
	 * image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/consoleDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE console menu save console content into file
	 * menu item image icon.
	 */
	private final static ImageIcon SAVE_CONSOLE_CONTENT_INTO_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/saveContentIntoFile.png");
	/**
	 * ACIDE - A Configurable IDE console panel document console lexicon menu
	 * item image icon.
	 */
	private final static ImageIcon DOCUMENT_CONSOLE_LEXICON_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/documentLexicon.png");
	/**
	 * ACIDE - A Configurable IDE console panel popup menu search menu item
	 * image icon.
	 */
	private final static ImageIcon SEARCH_CONSOLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/search.png");
	/**
	 * ACIDE - A Configurable IDE console menu close console menu item image
	 * icon.
	 */
	private final static ImageIcon CLOSE_CONSOLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/closeConsole.png");
	/**
	 * ACIDE - A Configurable IDE console menu configure menu item.
	 */
	private JMenuItem _configureMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu external command menu item.
	 */
	private JMenuItem _externalCommandMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu console display options menu
	 * item.
	 */
	private JMenuItem _consoleDisplayOptionsMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu document lexicon menu item.
	 */
	private JMenuItem _documentLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu search console menu item.
	 */
	private JMenuItem _searchConsoleMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu close console menu item.
	 */
	private JMenuItem _closeConsoleMenuItem;
	/**
	 * ACIDE - A Configurable IDE console menu save console content into file
	 * menu item.
	 */
	private JMenuItem _saveConsoleContentIntoFile;
	/**
	 * ACIDE - A Configurable IDE console menu search close console separator.
	 */
	private JSeparator _searchCloseConsoleSeparator;
	/**
	 * ACIDE - A Configurable IDE console menu external command display options
	 * separator.
	 */
	private JSeparator _externalCommandDisplayOptionsSeparator;
	/**
	 * ACIDE - A Configurable IDE console menu document console lexicon search
	 * console separator.
	 */
	private JSeparator _documentConsoleLexiconSearchConsoleSeparator;

	/**
	 * Creates a new ACIDE - A Configurable IDE console menu.
	 */
	public AcideConsoleMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the console menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE console menu.
	 */
	private void addComponents() {

		// Adds the configure menu item
		add(_configureMenuItem);

		// Adds the external command menu item
		add(_externalCommandMenuItem);

		// Adds the external command display options separator
		add(_externalCommandDisplayOptionsSeparator);

		// Adds the console display options menu item
		add(_consoleDisplayOptionsMenuItem);

		// Adds the save console content into file menu item
		add(_saveConsoleContentIntoFile);

		// Adds the document lexicon menu item
		add(_documentLexiconMenuItem);

		// Adds the document console lexicon search console separator
		add(_documentConsoleLexiconSearchConsoleSeparator);

		// Adds the search console menu item
		add(_searchConsoleMenuItem);

		// Adds the separator
		add(_searchCloseConsoleSeparator);

		// Adds the close console menu item
		add(_closeConsoleMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE components.
	 */
	private void buildComponents() {

		// Creates the configure menu item
		_configureMenuItem = new JMenuItem(CONFIGURE_IMAGE);

		// Sets the configure menu item name
		_configureMenuItem.setName(CONFIGURE_NAME);

		// Creates the external command menu item
		_externalCommandMenuItem = new JMenuItem(EXTERNAL_COMMAND_IMAGE);

		// Sets the external command menu item name
		_externalCommandMenuItem.setName(EXTERNAL_COMMAND_NAME);

		// Creates the external command display options separator
		_externalCommandDisplayOptionsSeparator = new JSeparator();

		// Creates the console display options menu item
		_consoleDisplayOptionsMenuItem = new JMenuItem(
				CONSOLE_DISPLAY_OPTIONS_IMAGE);

		// Sets the console display options menu item name
		_consoleDisplayOptionsMenuItem.setName(CONSOLE_DISPLAY_OPTIONS_NAME);

		// Creates the save console content into file menu item
		_saveConsoleContentIntoFile = new JMenuItem(
				SAVE_CONSOLE_CONTENT_INTO_FILE_IMAGE);

		// Sets the save console content into file menu item name
		_saveConsoleContentIntoFile
				.setName(SAVE_CONSOLE_CONTENT_INTO_FILE_NAME);

		// Creates the document lexicon menu item
		_documentLexiconMenuItem = new JMenuItem(DOCUMENT_CONSOLE_LEXICON_IMAGE);

		// Sets the document lexicon menu item name
		_documentLexiconMenuItem.setName(DOCUMENT_CONSOLE_LEXICON_NAME);

		// Creates the display options search console separator
		_documentConsoleLexiconSearchConsoleSeparator = new JSeparator();

		// Creates the search console menu item
		_searchConsoleMenuItem = new JMenuItem(SEARCH_CONSOLE_IMAGE);

		// Sets the search console menu item name
		_searchConsoleMenuItem.setName(SEARCH_CONSOLE_NAME);

		// Sets the search console menu item accelerator
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_searchConsoleMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_F, ActionEvent.CTRL_MASK
							+ ActionEvent.SHIFT_MASK));
		else
			_searchConsoleMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_B, ActionEvent.CTRL_MASK
							+ ActionEvent.SHIFT_MASK));

		// Creates the search console separator
		_searchCloseConsoleSeparator = new JSeparator();

		// Creates the close console menu item
		_closeConsoleMenuItem = new JMenuItem(CLOSE_CONSOLE_IMAGE);

		// Sets the close console menu item
		_closeConsoleMenuItem.setName(CLOSE_CONSOLE_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE console menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the configure menu item text
		_configureMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s333"));

		// Sets the external command menu item text
		_externalCommandMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s341"));

		// Sets the console display options menu item text
		_consoleDisplayOptionsMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s977"));

		// Sets the save console content into file menu item text
		_saveConsoleContentIntoFile.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s2013"));

		// Sets the document lexicon menu item text
		_documentLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1093"));

		// Sets the search console menu item text
		_searchConsoleMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s556"));

		// Sets the close console menu item text
		_closeConsoleMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1099"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE console menu components visibility
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the configure menu item to visible or not visible
		_configureMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CONFIGURE_NAME));

		// Sets the external command menu item to visible or not visible
		_externalCommandMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(EXTERNAL_COMMAND_NAME));

		// Sets the external command display options separator to visible or not
		// visible
		_externalCommandDisplayOptionsSeparator
				.setVisible((AcideMenuConfiguration.getInstance()
						.getIsDisplayed(CONFIGURE_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(EXTERNAL_COMMAND_NAME))
						&& (AcideMenuConfiguration.getInstance()
								.getIsDisplayed(CONSOLE_DISPLAY_OPTIONS_NAME)
								|| AcideMenuConfiguration
										.getInstance()
										.getIsDisplayed(
												SAVE_CONSOLE_CONTENT_INTO_FILE_NAME) || AcideMenuConfiguration
								.getInstance().getIsDisplayed(
										DOCUMENT_CONSOLE_LEXICON_NAME)));

		// Sets the console display options menu item to visible or not visible
		_consoleDisplayOptionsMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(CONSOLE_DISPLAY_OPTIONS_NAME));

		// Sets the save console content into file menu item to visible or not
		// visible
		_saveConsoleContentIntoFile.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						SAVE_CONSOLE_CONTENT_INTO_FILE_NAME));

		// Sets the document lexicon menu item to visible or not visible
		_documentLexiconMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(DOCUMENT_CONSOLE_LEXICON_NAME));

		// Sets the document console lexicon search console separator to visible
		// or not visible
		_documentConsoleLexiconSearchConsoleSeparator
				.setVisible((AcideMenuConfiguration.getInstance()
						.getIsDisplayed(CONFIGURE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								EXTERNAL_COMMAND_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								CONSOLE_DISPLAY_OPTIONS_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								SAVE_CONSOLE_CONTENT_INTO_FILE_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(
								DOCUMENT_CONSOLE_LEXICON_NAME))
						&& AcideMenuConfiguration.getInstance().getIsDisplayed(
								SEARCH_CONSOLE_NAME));

		// Sets the search console menu item to visible or not visible
		_searchConsoleMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SEARCH_CONSOLE_NAME));

		// Sets the search close console separator to visible or not visible
		_searchCloseConsoleSeparator.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(CONFIGURE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						EXTERNAL_COMMAND_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						CONSOLE_DISPLAY_OPTIONS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_CONSOLE_CONTENT_INTO_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						DOCUMENT_CONSOLE_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SEARCH_CONSOLE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						CLOSE_CONSOLE_NAME));

		// Sets the close console menu item to visible or not visible
		_closeConsoleMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CLOSE_CONSOLE_NAME));
	}

	/**
	 * Sets ACIDE - A Configurable IDE console menu item listeners.
	 */
	public void setListeners() {

		// Sets the configure menu item action listener
		_configureMenuItem
				.addActionListener(new AcideConfigureMenuItemListener());

		// Sets the external command menu item action listener
		_externalCommandMenuItem
				.addActionListener(new AcideExternalCommandMenuItemListener());

		// Sets the console display options menu item action listener
		_consoleDisplayOptionsMenuItem
				.addActionListener(new AcideConsoleDisplayOptionsMenuItemListener());

		// Sets the save console content into file menu item action listener
		_saveConsoleContentIntoFile
				.addActionListener(new AcideSaveConsoleContentIntoFileMenuItemListener());

		// Sets the document lexicon menu item action listener
		_documentLexiconMenuItem
				.addActionListener(new AcideDocumentConsoleLexiconMenuItemListener());

		// Sets the search console menu item action listener
		_searchConsoleMenuItem
				.addActionListener(new AcideSearchConsoleMenuItemListener());

		// Sets the close console menu item action listener
		_closeConsoleMenuItem
				.addActionListener(new AcideCloseConsoleMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu external command menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu external command
	 *         menu. item
	 */
	public JMenuItem getExternalCommandMenuItem() {
		return _externalCommandMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu configure menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu configure menu item.
	 */
	public JMenuItem getConfigureMenuItem() {
		return _configureMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu console display
	 * options menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu console display
	 *         options menu item.
	 */
	public JMenuItem getConsoleDisplayOptionsMenuItem() {
		return _consoleDisplayOptionsMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu close console menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu close console menu
	 *         item.
	 */
	public JMenuItem getCloseConsoleMenuItem() {
		return _closeConsoleMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu search console menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu search console menu
	 *         item.
	 */
	public JMenuItem getSearchConsoleMenuItem() {
		return _searchConsoleMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu document console
	 * lexicon menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu document console
	 *         lexicon menu item.
	 */
	public JMenuItem getDocumentConsoleLexiconMenuItem() {
		return _documentLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console menu save console content
	 * into file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console menu save console content
	 *         into file menu item.
	 */
	public JMenuItem getSaveConsoleContentIntoFileMenuItem() {
		return _saveConsoleContentIntoFile;
	}
}