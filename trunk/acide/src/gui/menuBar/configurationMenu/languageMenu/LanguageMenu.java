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
package gui.menuBar.configurationMenu.languageMenu;

import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.menu.AcideMenuConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.languageMenu.listeners.EnglishMenuItemListener;
import gui.menuBar.configurationMenu.languageMenu.listeners.SpanishMenuItemListener;
import gui.menuBar.editMenu.gui.replace.AcideReplaceWindow;
import gui.menuBar.editMenu.gui.search.AcideSearchWindow;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import operations.log.AcideLog;
import resources.AcideResourceManager;

import language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE language menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class LanguageMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE language menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE language menu spanish menu item name.
	 */
	public static final String SPANISH_NAME = "Spanish";
	/**
	 * ACIDE - A Configurable IDE language menu english menu item name.
	 */
	public static final String ENGLISH_NAME = "English";
	/**
	 * ACIDE - A Configurable IDE language menu spanish menu item image icon.
	 */
	private static final ImageIcon SPANISH_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/language/spanish.png");
	/**
	 * ACIDE - A Configurable IDE language menu english menu item image icon.
	 */
	private static final ImageIcon ENGLISH_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/language/english.png");
	/**
	 * ACIDE - A Configurable IDE language menu spanish menu item.
	 */
	private JMenuItem _spanish;
	/**
	 * ACIDE - A Configurable IDE language menu english menu item.
	 */
	private JMenuItem _english;

	/**
	 * Creates a new ACIDE - A Configurable IDE language menu.
	 */
	public LanguageMenu() {

		// MENU ITEM
		_spanish = new JMenuItem(SPANISH_IMAGE);
		_english = new JMenuItem(ENGLISH_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE language menu language labels.
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

		// SPANISH
		_spanish.setText(labels.getString("s11"));
		_spanish.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK));

		// ENGLISH
		_english.setText(labels.getString("s12"));
		_english.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.ALT_MASK));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE language menu.
	 */
	public void buildMenu() {

		removeAll();

		// SPANISH MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SPANISH_NAME))
			add(_spanish);

		// ENGLISH MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ENGLISH_NAME))
			add(_english);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE language menu menu item listeners.
	 */
	public void setListeners() {

		// SPANISH
		_spanish.addActionListener(new SpanishMenuItemListener());

		// ENGLISH
		_english.addActionListener(new EnglishMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language menu English menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE language menu English menu item.
	 */
	public JMenuItem getEnglish() {
		return _english;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE language menu English menu item.
	 * 
	 * @param english
	 *            new value to set.
	 */
	public void setEnglish(JMenuItem english) {
		_english = english;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language menu Spanish menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE language menu Spanish menu item.
	 */
	public JMenuItem getSpanish() {
		return _spanish;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE language menu Spanish menu item.
	 * 
	 * @param spanish
	 *            new value to set.
	 */
	public void setSpanish(JMenuItem spanish) {
		_spanish = spanish;
	}

	/**
	 * Changes the language to display in the application and reset all the
	 * components with the new language.
	 * 
	 * @param selectedLanguage
	 *            new language to set.
	 */
	public void changeLanguage(String selectedLanguage) {

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

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty("language", selectedLanguage);

		// Gets the labels
		ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();
		AcideLog.getLog().info(labels.getString("s100"));

		// Resets the main window
		MainWindow.getInstance().getMenu().setLanguageLabels();
		MainWindow.getInstance().buildToolBarPanel();

		// Updates the EXPLORER POPUP MENU
		MainWindow.getInstance().getExplorerPanel().buildPopupMenu();

		// Updates the OUTPUT POPUP MENU
		MainWindow.getInstance().getConsolePanel().buildPopupMenu();

		// Updates the STATUS BAR POPUP MENU
		MainWindow.getInstance().getStatusBar().buildPopupMenu();

		// Updates the EDITORS POPUP MENU
		int numEditors = MainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();
		for (int pos = 0; pos < numEditors; pos++)
			MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(pos).buildPopupMenu();

		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// Resets the SEARCH GUI
		AcideSearchWindow searchGUI = AcideSearchWindow.getInstance();
		searchGUI.inicialize();
		searchGUI.validate();
		searchGUI.repaint();

		// Resets the REPLACE GUI
		AcideReplaceWindow replaceGUI = AcideReplaceWindow.getInstance();
		replaceGUI.inicialize();
		replaceGUI.validate();
		replaceGUI.repaint();

		// Get the labels
		labels = AcideLanguageManager.getInstance().getLabels();

		// Updates the lexicon message in the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ AcideLexiconConfiguration.getInstance().getName());

		try {

			// Gets the name
			String currentGrammar = AcideResourceManager.getInstance().getProperty(
					"currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);

			// Updates the grammar message in the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							labels.getString("s248") + " " + grammarName);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}

		// Gets the current lines number message
		String currentNumLinesMessage = MainWindow.getInstance().getStatusBar()
				.getNumberOfLinesMessage();

		// Gets the number of lines
		int lastIndexOfDouble = currentNumLinesMessage.lastIndexOf(":");
		if (lastIndexOfDouble != -1) {
			String numLines = currentNumLinesMessage.substring(
					lastIndexOfDouble, currentNumLinesMessage.length());

			// Updates the number of lines in the status bar
			String numLinesMessage = labels.getString("s1001") + numLines;

			// Updates the number of lines message in the status bar
			MainWindow.getInstance().getStatusBar().setNumberOfLinesMessage(numLinesMessage);
		}
		
		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance()
				.isDefaultProject()) {

			// Enables the project menu
			MainWindow.getInstance().getMenu().enableProjectMenu();

			// Enables the open all files menu item
			MainWindow.getInstance().getMenu().getFile().getOpenAllFiles().setEnabled(true);		
	
			// The project configuration has been modified
			AcideProjectConfiguration.getInstance()
					.setIsModified(true);
		}

		// If there are opened editors
		if (MainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Enables the file menu
			MainWindow.getInstance().getMenu().enableFileMenu();

			// Enables the edit menu
			MainWindow.getInstance().getMenu().enableEditMenu();
		}
	}
}