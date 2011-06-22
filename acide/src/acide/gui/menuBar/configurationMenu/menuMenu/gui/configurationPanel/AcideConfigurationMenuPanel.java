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
package acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel;

import java.awt.BorderLayout;
import java.util.ArrayList;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.menu.AcideMenuItemInformation;
import acide.gui.menuBar.configurationMenu.AcideConfigurationMenu;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.consolePanel.AcideConsoleMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.fileEditorPanel.AcideFileEditorMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.grammarPanel.AcideGrammarMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.language.AcideLanguageMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.lexiconPanel.AcideLexiconMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.menuPanel.AcideMenuMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.toolBarPanel.AcideToolBarMenuPanel;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE configuration menu panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideConfigurationMenuPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE configuration menu panel serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon menu panel.
	 */
	private AcideLexiconMenuPanel _lexiconMenuPanel;
	/**
	 * ACIDE - A Configurable IDE grammar menu panel.
	 */
	private AcideGrammarMenuPanel _grammarMenuPanel;
	/**
	 * ACIDE - A Configurable IDE file editor menu panel.
	 */
	private AcideFileEditorMenuPanel _fileEditorMenuPanel;
	/**
	 * ACIDE - A Configurable IDE console menu panel.
	 */
	private AcideConsoleMenuPanel _consoleMenuPanel;
	/**
	 * ACIDE - A Configurable IDE language menu panel.
	 */
	private AcideLanguageMenuPanel _languageMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu menu panel.
	 */
	private AcideMenuMenuPanel _menuMenuPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar menu panel.
	 */
	private AcideToolBarMenuPanel _toolBarMenuPanel;
	/**
	 * ACIDE - A Configurable IDE configuration menu panel compiler check box.
	 */
	private JCheckBox _compilerCheckBox;
	/**
	 * ACIDE - A Configurable IDE configuration menu panel tabbed pane.
	 */
	private JTabbedPane _tabbedPane;

	/**
	 * Creates a new ACIDE - A Configurable IDE configuration menu panel.
	 */
	public AcideConfigurationMenuPanel() {

		super(new BorderLayout());

		// Builds the panel components
		buildComponents();

		// Sets the listeners of the panel components
		setListeners();

		// Adds the components to the panel
		addComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE configuration menu
	 * panel.
	 */
	public void addComponents() {

		// Adds the compiler check box to the panel
		add(_compilerCheckBox, BorderLayout.NORTH);

		// Adds the tabbed to the panel
		add(_tabbedPane, BorderLayout.CENTER);
	}

	/**
	 * Sets the listener for the ACIDE - A Configurable IDE configuration menu
	 * panel components.
	 */
	public void setListeners() {

	}

	/**
	 * Creates the ACIDE - A Configurable IDE configuration menu panel
	 * components.
	 */
	public void buildComponents() {

		// Creates the tabbed pane
		_tabbedPane = new JTabbedPane();

		// Creates the compiler check box
		_compilerCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s240"));
		
		// Creates the lexicon menu panel
		_lexiconMenuPanel = new AcideLexiconMenuPanel();

		// Adds the lexicon menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s224"), _lexiconMenuPanel);

		// Creates the grammar menu panel
		_grammarMenuPanel = new AcideGrammarMenuPanel();

		// Adds the grammar menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s225"), _grammarMenuPanel);

		// Creates the file editor menu panel
		_fileEditorMenuPanel = new AcideFileEditorMenuPanel();

		// Adds the file editor menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s1045"), _fileEditorMenuPanel);

		// Creates the console menu panel
		_consoleMenuPanel = new AcideConsoleMenuPanel();

		// Adds the console menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s332"), _consoleMenuPanel);

		// Creates language menu panel
		_languageMenuPanel = new AcideLanguageMenuPanel();

		// Adds the language menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s6"), _languageMenuPanel);

		// Creates the menu menu panel
		_menuMenuPanel = new AcideMenuMenuPanel();

		// Adds the menu menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s34"), _menuMenuPanel);

		// Creates the tool bar menu panel
		_toolBarMenuPanel = new AcideToolBarMenuPanel();

		// Adds the tool bar menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s169"), _toolBarMenuPanel);
	}

	/**
	 * Sets the check box values from the menu item list of the menu
	 * configuration.
	 */
	public void setCheckBoxesFromMenuItemList() {

		// Updates the lexicon menu panel check boxes state
		_lexiconMenuPanel.setCheckBoxesFromMenuItemList();

		// Updates the grammar menu panel check boxes state
		_grammarMenuPanel.setCheckBoxesFromMenuItemList();
		
		// Updates the compiler check box state
		_compilerCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideConfigurationMenu.COMPILER_NAME));

		// Updates the file editor menu panel check boxes state
		_fileEditorMenuPanel.setCheckBoxesFromMenuItemList();

		// Updates the console menu panel check boxes state
		_consoleMenuPanel.setCheckBoxesFromMenuItemList();

		// Updates the language menu panel check boxes state
		_languageMenuPanel.setCheckBoxesFromMenuItemList();

		// Updates the menu menu panel check boxes state
		_menuMenuPanel.setCheckBoxesFromMenuItemList();

		// Updates the tool bar menu panel check boxes state
		_toolBarMenuPanel.setCheckBoxesFromMenuItemList();
	}

	/**
	 * Adds the configuration menu information to the menu item list, based on
	 * the window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addConfigurationMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		// Adds the lexicon menu panel components menu information to the menu
		// item list
		_lexiconMenuPanel.addLexiconMenuInformation(menuItemList);

		// Adds the grammar menu panel components menu information to the menu
		// item list
		_grammarMenuPanel.addGrammarMenuInformation(menuItemList);

		// Adds the compiler menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(
				AcideConfigurationMenu.COMPILER_NAME, _compilerCheckBox
						.isSelected()));

		// Adds the file editor menu panel components menu information to the
		// menu item list
		_fileEditorMenuPanel.addFileEditorMenuInformation(menuItemList);

		// Adds the console menu panel components menu information to the menu
		// item list
		_consoleMenuPanel.addConsoleMenuInformation(menuItemList);

		// Adds the language menu panel components menu information to the menu
		// item list
		_languageMenuPanel.addLanguageMenuInformation(menuItemList);

		// Adds the menu menu panel components menu information to the menu item
		// list
		_menuMenuPanel.addMenuMenuInformation(menuItemList);

		// Adds the tool bar menu panel components menu information to the menu
		// item list
		_toolBarMenuPanel.addToolBarMenuInformation(menuItemList);
	}

	/**
	 * Marks as selected all the ACIDE - A Configurable IDE configuration menu
	 * panel components.
	 */
	public void selectAll() {

		// Sets the lexicon menu panel as not selected
		_lexiconMenuPanel.selectAll();

		// Sets the grammar menu panel as not selected
		_grammarMenuPanel.selectAll();

		// Sets the compiler check box as selected
		_compilerCheckBox.setSelected(true);

		// Sets the file editor menu panel as not selected
		_fileEditorMenuPanel.selectAll();

		// Sets the console menu panel as not selected
		_consoleMenuPanel.selectAll();

		// Sets the language menu panel as not selected
		_languageMenuPanel.selectAll();

		// Sets the menu menu panel as not selected
		_menuMenuPanel.selectAll();

		// Sets the tool bar menu panel as not selected
		_toolBarMenuPanel.selectAll();
	}

	/**
	 * Marks as selected none of the ACIDE - A Configurable IDE configuration
	 * menu panel components.
	 */
	public void selectNone() {

		// Sets the lexicon menu panel as selected
		_lexiconMenuPanel.selectNone();

		// Sets the grammar menu panel as selected
		_grammarMenuPanel.selectNone();

		// Sets the compiler check box as selected
		_compilerCheckBox.setSelected(false);

		// Sets the file editor menu panel as selected
		_fileEditorMenuPanel.selectNone();

		// Sets the console menu panel as selected
		_consoleMenuPanel.selectNone();

		// Sets the language menu panel as selected
		_languageMenuPanel.selectNone();

		// Sets the menu menu panel as selected
		_menuMenuPanel.selectNone();

		// Sets the tool bar menu panel as selected
		_toolBarMenuPanel.selectNone();
	}
}
