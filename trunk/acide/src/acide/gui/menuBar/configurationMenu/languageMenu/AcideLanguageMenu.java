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
package acide.gui.menuBar.configurationMenu.languageMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.languageMenu.listeners.AcideEnglishMenuItemListener;
import acide.gui.menuBar.configurationMenu.languageMenu.listeners.AcideSpanishMenuItemListener;
import acide.gui.menuBar.editMenu.gui.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.AcideSearchWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE language menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideLanguageMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE language menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE language menu Spanish menu item name.
	 */
	public static final String SPANISH_NAME = "Spanish";
	/**
	 * ACIDE - A Configurable IDE language menu English menu item name.
	 */
	public static final String ENGLISH_NAME = "English";
	/**
	 * ACIDE - A Configurable IDE language menu Spanish menu item image icon.
	 */
	private static final ImageIcon SPANISH_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/language/spanish.png");
	/**
	 * ACIDE - A Configurable IDE language menu English menu item image icon.
	 */
	private static final ImageIcon ENGLISH_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/language/english.png");
	/**
	 * ACIDE - A Configurable IDE language menu Spanish menu item.
	 */
	private JMenuItem _spanishMenuItem;
	/**
	 * ACIDE - A Configurable IDE language menu English menu item.
	 */
	private JMenuItem _englishMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE language menu.
	 */
	public AcideLanguageMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the language menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE language menu
	 */
	private void addComponents() {

		// Adds the Spanish menu item to the menu
		add(_spanishMenuItem);

		// Adds the English menu item to the menu
		add(_englishMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE language menu components.
	 */
	private void buildComponents() {

		// Creates the Spanish menu item
		_spanishMenuItem = new JMenuItem(SPANISH_IMAGE);

		// Sets the Spanish menu item name
		_spanishMenuItem.setName(SPANISH_NAME);

		// Creates the English menu item
		_englishMenuItem = new JMenuItem(ENGLISH_IMAGE);

		// Sets the English menu item name
		_englishMenuItem.setName(ENGLISH_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE language menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the Spanish menu item text
		_spanishMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s11"));

		// Sets the Spanish menu item accelerator
		_spanishMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK));

		// Sets the Spanish menu item text
		_englishMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s12"));

		// Creates the English menu item accelerator
		_englishMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.ALT_MASK));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE language menu components
	 * visibility with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the Spanish menu item to visible or not visible
		_spanishMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SPANISH_NAME));

		// Adds the English menu item to visible or not visible
		_englishMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(ENGLISH_NAME));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE language menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the Spanish menu item action listener
		_spanishMenuItem.addActionListener(new AcideSpanishMenuItemListener());

		// Sets the Spanish menu item action listener
		_englishMenuItem.addActionListener(new AcideEnglishMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language menu English menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE language menu English menu item.
	 */
	public JMenuItem getEnglishMenuItem() {
		return _englishMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language menu Spanish menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE language menu Spanish menu item.
	 */
	public JMenuItem getSpanishMenuItem() {
		return _spanishMenuItem;
	}

	/**
	 * Changes the language to display in the application and reset all the
	 * components with the new language.
	 * 
	 * @param selectedLanguage
	 *            new language to set.
	 */
	public void changeLanguage(String selectedLanguage) {

		// Applies the selected language in the ACIDE - A Configurable IDE main
		// window components
		applyLanguage(selectedLanguage);

		// Updates the menu
		AcideMainWindow.getInstance().getMenu().configure();

		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// The project configuration has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
		}
	}

	/**
	 * Applies the language to display in the application and reset all the
	 * components with the new language.
	 * 
	 * @param selectedLanguage
	 *            new language to set.
	 */
	public void applyLanguage(String selectedLanguage) {

		// Updates the ACIDE - A Configurable IDE language
		AcideResourceManager.getInstance().setProperty("language",
				selectedLanguage);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s100"));

		try {

			// Sets the ACIDE - A Configurable IDE language
			AcideLanguageManager.getInstance().setLanguage(
					AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Resets the text of the menu bar
		AcideMainWindow.getInstance().getMenu().setTextOfMenuComponents();

		// Updates the lexicon and grammar menu
		updateLexiconGrammarMenu();

		// Resets the tool bar panel
		AcideMainWindow.getInstance().buildToolBarPanel();

		// Updates the state of the file menu buttons
		AcideMainWindow.getInstance().getToolBarPanel().getMenuBarToolBar()
				.updateStateOfFileButtons();
		
		// Updates the popup menus
		updatesPopupMenus();

		// Updates the search and replace window
		updateSearchReplaceWindow();

		// Updates the status bar
		updateStatusBar();

		// Validates the changes in the main window
		AcideMainWindow.getInstance().validate();

		// Repaints the main window
		AcideMainWindow.getInstance().repaint();
	}

	/**
	 * Updates the ACIDE - A Configurable IDE lexicon and grammar menus.
	 */
	public void updateLexiconGrammarMenu() {

		// If there are opened file editors
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Enables the lexicon menu in the configuration menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLexiconMenu().enableMenu();

			// Enables the grammar menu in the configuration menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().enableMenu();

		} else {

			// Disables the lexicon menu in the configuration menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLexiconMenu().disableMenu();

			// Disables the grammar menu in the configuration menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().disableMenu();
		}
	}

	/**
	 * Updates the ACIDE - A Configurable IDE search and replace windows.
	 */
	public void updateSearchReplaceWindow() {

		// Resets the search window
		AcideSearchWindow.getInstance().initialize();

		// Validates the changes in the search window
		AcideSearchWindow.getInstance().validate();

		// Repaints the search window
		AcideSearchWindow.getInstance().repaint();

		// Resets the replace window
		AcideReplaceWindow.getInstance().initialize();

		// Validates the changes in the replace window
		AcideReplaceWindow.getInstance().validate();

		// Repaints the replace window
		AcideReplaceWindow.getInstance().repaint();
	}

	/**
	 * Updates the ACIDE - A Configurable IDE status bar.
	 */
	public void updateStatusBar() {

		// Gets the current lines number message
		String currentNumLinesMessage = AcideMainWindow.getInstance()
				.getStatusBar().getNumberOfLinesMessage();

		// Gets the number of lines
		int lastIndexOfDoubleColon = currentNumLinesMessage.lastIndexOf(":");

		// If contains the last index of double colon
		if (lastIndexOfDoubleColon != -1) {

			// Gets the number of lines
			String numLines = currentNumLinesMessage
					.substring(lastIndexOfDoubleColon + 1,
							currentNumLinesMessage.length());

			// Updates the number of lines in the status bar
			String numLinesMessage = AcideLanguageManager.getInstance()
					.getLabels().getString("s1001")
					+ numLines;

			// Updates the number of lines message in the status bar
			AcideMainWindow.getInstance().getStatusBar()
					.setNumberOfLinesMessage(numLinesMessage);
		}
	}

	/**
	 * Updates all the ACIDE - A Configurable IDE popup menus.
	 */
	public void updatesPopupMenus() {

		// Resets the explorer panel popup menu
		AcideMainWindow.getInstance().getExplorerPanel().buildPopupMenu();

		// Resets the console panel popup menu
		AcideMainWindow.getInstance().getConsolePanel().buildPopupMenu();

		// Resets the status bar popup menu
		AcideMainWindow.getInstance().getStatusBar().buildPopupMenu();

		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++)

			// Updates the file editor panel popup menus
			AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).buildPopupMenu();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE language menu.
	 */
	public void build() {

	}
}