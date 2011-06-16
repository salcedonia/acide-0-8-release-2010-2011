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
package acide.gui.menuBar.configurationMenu.lexiconMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideDocumentLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideModifyLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideNewLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideDefaultLexiconsMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE lexicon menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideLexiconMenu extends JMenu {

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
	 * ACIDE - A Configurable IDE lexicon menu document lexicon menu item name.
	 */
	public static final String DOCUMENT_LEXICON_NAME = "Document Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu default lexicons menu item name.
	 */
	public static final String DEFAULT_LEXICONS_NAME = "Default Lexicons";
	/**
	 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item.
	 */
	private JMenuItem _newLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu document lexicon menu item.
	 */
	private JMenuItem _documentLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu modify lexicon menu item.
	 */
	private JMenuItem _modifyLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon as Predetermine
	 * lexicon separator.
	 */
	private JSeparator _separator;
	/**
	 * ACIDE - A Configurable IDE lexicon menu default lexicons menu item.
	 */
	private JMenuItem _defaultLexiconsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon menu.
	 */
	public AcideLexiconMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the lexicon menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE lexicon menu.
	 */
	private void addComponents() {

		// Adds the new lexicon menu item to the menu
		add(_newLexiconMenuItem);

		// Adds the document lexicon menu item to the menu
		add(_documentLexiconMenuItem);

		// Adds the modify lexicon menu item to the menu
		add(_modifyLexiconMenuItem);

		// Adds the modify lexicon default lexicons separator
		add(_separator);

		// Adds the default lexicons menu item
		add(_defaultLexiconsMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon menu components.
	 */
	private void buildComponents() {

		// Creates the new lexicon menu item
		_newLexiconMenuItem = new JMenuItem();

		// Sets the new lexicon menu item name
		_newLexiconMenuItem.setName(NEW_LEXICON_NAME);

		// Creates the document lexicon menu item
		_documentLexiconMenuItem = new JMenuItem();

		// Sets the document lexicon menu item name
		_documentLexiconMenuItem.setName(DOCUMENT_LEXICON_NAME);

		// Creates the modify lexicon menu item
		_modifyLexiconMenuItem = new JMenuItem();

		// Sets the modify lexicon menu item name
		_modifyLexiconMenuItem.setName(MODIFY_LEXICON_NAME);

		// Creates the modify lexicon default lexicons separator
		_separator = new JSeparator();

		// Creates the default lexicons menu item
		_defaultLexiconsMenuItem = new JMenuItem();

		// Sets the default lexicons menu item name
		_defaultLexiconsMenuItem.setName(DEFAULT_LEXICONS_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE lexicon menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the new lexicon menu item text
		_newLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s249"));

		// Sets the document lexicon menu item text
		_documentLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1093"));

		// Sets the document lexicon menu item accelerator
		_documentLexiconMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_L, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the modify lexicon menu item text
		_modifyLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s29"));

		// Sets the modify lexicon menu item accelerator
		_modifyLexiconMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_X, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the default lexicons menu item text
		_defaultLexiconsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1080"));
	}

	/**
	 * Sets the lACIDE - A Configurable IDE lexicon menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the new lexicon menu item action listener
		_newLexiconMenuItem
				.addActionListener(new AcideNewLexiconMenuItemListener());

		// Sets the modify lexicon menu item action listener
		_modifyLexiconMenuItem
				.addActionListener(new AcideModifyLexiconMenuItemListener());

		// Sets the document lexicon menu item action listener
		_documentLexiconMenuItem
				.addActionListener(new AcideDocumentLexiconMenuItemListener());

		// Sets the default lexicons menu item action listener
		_defaultLexiconsMenuItem
				.addActionListener(new AcideDefaultLexiconsMenuItemListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void build() {

		// Sets the new lexicon menu item to visible or not visible
		_newLexiconMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(NEW_LEXICON_NAME));

		// Sets the document lexicon menu item to visible or not visible
		_documentLexiconMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(DOCUMENT_LEXICON_NAME));

		// Sets the modify lexicon menu item to visible or not visible
		_modifyLexiconMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(MODIFY_LEXICON_NAME));

		// Sets the save lexicon as default lexicons separator to visible or
		// not visible
		_separator.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						DOCUMENT_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						DEFAULT_LEXICONS_NAME));

		// Sets the default lexicons menu item to visible or not visible
		_defaultLexiconsMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(DEFAULT_LEXICONS_NAME));
	}

	/**
	 * Enables the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void enableMenu() {

		// Enables the new lexicon menu item
		_newLexiconMenuItem.setEnabled(true);

		// Enables the document lexicon menu item
		_documentLexiconMenuItem.setEnabled(true);

		// Enables the modify lexicon menu item
		_modifyLexiconMenuItem.setEnabled(true);

		// Enables the default lexicons menu item
		_defaultLexiconsMenuItem.setEnabled(true);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void disableMenu() {

		// Disables the new lexicon menu item
		_newLexiconMenuItem.setEnabled(false);

		// Disables the document lexicon menu item
		_documentLexiconMenuItem.setEnabled(false);

		// Disables the modify lexicon menu item
		_modifyLexiconMenuItem.setEnabled(false);

		// Enables the default lexicons menu item
		_defaultLexiconsMenuItem.setEnabled(true);
	}

	/**
	 * Documents the ACIDE - A Configurable IDE lexicon configuration in the
	 * selected file editor panel.
	 * 
	 * @param absolutePath new lexicon configuration to set.
	 */
	public void documentLexicon(String absolutePath) {

		// Loads the lexicon configuration
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.load(absolutePath);

		// Resets the selected file editor text edition area
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().resetStyledDocument();

		// Updates the lexicon message status bar
		AcideMainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s449")
								+ " "
								+ AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getLexiconConfiguration().getName());
	}

	/**
	 * Configures the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void configure(){
		
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Enables the lexicon menu
			enableMenu();

		} else {

			// Disables the lexicon menu
			disableMenu();
		}
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu new lexicon menu item
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu new lexicon menu item
	 */
	public JMenuItem getNewLexiconMenuItem() {
		return _newLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu document lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu document lexicon menu
	 *         item.
	 */
	public JMenuItem getDocumentLexiconMenuItem() {
		return _documentLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu modify lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu modify lexicon menu
	 *         item.
	 */
	public JMenuItem getModifyLexiconMenuItem() {
		return _modifyLexiconMenuItem;
	}
}