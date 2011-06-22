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
package acide.factory.gui;

import javax.swing.JFrame;

import acide.gui.consolePanel.AcideConsolePanel;
import acide.gui.explorerPanel.AcideExplorerPanel;
import acide.gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.AcideMenuBar;
import acide.gui.menuBar.configurationMenu.consoleMenu.gui.AcideConsoleDisplayOptionsWindow;
import acide.gui.menuBar.configurationMenu.consoleMenu.gui.AcideExternalCommandConfigurationWindow;
import acide.gui.menuBar.configurationMenu.consoleMenu.gui.AcideConsoleConfigurationWindow;
import acide.gui.menuBar.configurationMenu.fileEditor.gui.AcideFileEditorDisplayOptionsWindow;
import acide.gui.menuBar.configurationMenu.grammarMenu.gui.AcideGrammarConfigurationWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.AcideLexiconConfigurationWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.defaultLexiconsWindow.AcideDefaultLexiconsWindow;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;
import acide.gui.menuBar.fileMenu.gui.AcidePrintConfigurationWindow;
import acide.gui.menuBar.helpMenu.gui.AcideAboutUsWindow;
import acide.gui.menuBar.projectMenu.gui.compilerWindow.AcideCompilerConfigurationWindow;
import acide.gui.menuBar.projectMenu.gui.executionWindow.AcideExecutionConfigurationWindow;
import acide.gui.menuBar.viewMenu.utils.AcideLogTab;
import acide.gui.statusBarPanel.AcideStatusBar;
import acide.gui.toolBarPanel.AcideToolBarPanel;

/**
 * ACIDE - A Configurable IDE class which creates the GUI components of the
 * application.
 * 
 * @version 0.8
 */
public class AcideGUIFactory {

	/**
	 * ACIDE - A Configurable IDE GUI factory unique class instance.
	 */
	private static AcideGUIFactory _instance;

	/**
	 * Returns the unique ACIDE - A Configurable IDE GUI factory class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE GUI factory class instance.
	 */
	public static AcideGUIFactory getInstance() {
		if (_instance == null)
			_instance = new AcideGUIFactory();
		return _instance;
	}

	/**
	 * Builds the ACIDE - A Configurable IDE main window.
	 * 
	 * @return the ACIDE - A Configurable IDE main window.
	 * @see AcideMainWindow
	 */
	public AcideMainWindow buildAcideMainWindow() {
		return new AcideMainWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu.
	 * @see AcideMenuBar
	 */
	public AcideMenuBar buildAcideMenu() {
		return new AcideMenuBar();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE about us window.
	 * 
	 * @return the ACIDE - A Configurable IDE about us window.
	 * @see AcideAboutUsWindow
	 */
	public AcideAboutUsWindow buildAcideAboutUsWindow() {
		return new AcideAboutUsWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE tool bar panel.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar panel.
	 * @see AcideToolBarPanel
	 */
	public AcideToolBarPanel buildAcideToolBarPanel() {
		return new AcideToolBarPanel();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console panel initially modifiable
	 * by default.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel.
	 * @see AcideConsolePanel
	 */
	public AcideConsolePanel buildAcideConsolePanel() {
		return new AcideConsolePanel(true);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE explorer panel.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel.
	 * @see AcideExplorerPanel
	 */
	public AcideExplorerPanel buildAcideExplorerPanel() {
		return new AcideExplorerPanel();
	}

	/**
	 * Build the ACIDE - A Configurable IDE menu configuration window.
	 * 
	 * @param forModify
	 *            indicates if the menu configuration window has to be used to
	 *            modify the menu configuration or not.
	 * @return the ACIDE - A Configurable IDE menu configuration window.
	 * @see AcideMenuConfigurationWindow
	 */
	public AcideMenuConfigurationWindow buildAcideMenuConfigurationWindow(
			boolean forModify) {
		return new AcideMenuConfigurationWindow(forModify);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE grammar configuration window.
	 * 
	 * @param forModify
	 *            indicates if the grammar configuration window has to be used
	 *            to modify the grammar configuration or not.
	 * @return the ACIDE - A Configurable IDE grammar configuration window.
	 * @see AcideGrammarConfigurationWindow
	 */
	public AcideGrammarConfigurationWindow buildAcideGrammarConfigurationWindow(
			boolean forModify) {
		return new AcideGrammarConfigurationWindow(forModify);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon configuration window.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration window.
	 * @see AcideLexiconConfigurationWindow
	 */
	public AcideLexiconConfigurationWindow buildAcideLexiconConfigurationWindow() {
		return new AcideLexiconConfigurationWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window.
	 * 
	 * @return the ACIDE - A Configurable IDE default lexicons window.
	 * @see AcideDefaultLexiconsWindow
	 */
	public AcideDefaultLexiconsWindow buildAcideDefaultLexiconsWindow() {
		return new AcideDefaultLexiconsWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE file editor manager.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager.
	 */
	public AcideFileEditorManager buildAcideFileEditorManager() {
		return new AcideFileEditorManager();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE log tab to be displayed on the file
	 * editor.
	 * 
	 * @return the ACIDE - A Configurable IDE log tab to be displayed on the
	 *         file editor.
	 * @see AcideLogTab
	 */
	public AcideLogTab buildAcideLogTab() {
		return new AcideLogTab();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console configuration window.
	 * 
	 * @param parent
	 *            parent window.
	 * 
	 * @return the ACIDE - A Configurable IDE console configuration window.
	 * @see JFrame
	 * @see AcideConsoleConfigurationWindow
	 */
	public AcideConsoleConfigurationWindow buildAcideConsoleConfigurationWindow(
			JFrame parent) {
		return new AcideConsoleConfigurationWindow(parent);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE external command configuration
	 * window.
	 * 
	 * @return the ACIDE - A Configurable IDE externalCommand configuration
	 *         window.
	 * @see AcideExternalCommandConfigurationWindow
	 */
	public AcideExternalCommandConfigurationWindow buildAcideExternalCommandConfigurationWindow() {
		return new AcideExternalCommandConfigurationWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar.
	 * @see AcideStatusBar
	 */
	public AcideStatusBar buildAcideStatusBar() {
		return new AcideStatusBar();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE print configuration window.
	 * 
	 * @return the ACIDE - A Configurable IDE print configuration window.
	 * @see AcidePrintConfigurationWindow
	 */
	public AcidePrintConfigurationWindow buildPrintConfigurationWindow() {
		return new AcidePrintConfigurationWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE execution configuration window.
	 * 
	 * @return the ACIDE - A Configurable IDE execution configuration window.
	 * @see AcideExecutionConfigurationWindow
	 */
	public AcideExecutionConfigurationWindow buildAcideExecutionConfigurationWindow() {
		return new AcideExecutionConfigurationWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE compiler configuration window.
	 * 
	 * @param parent
	 *            parent window.
	 * 
	 * @return the ACIDE - A Configurable IDE compiler configuration window.
	 * @see JFrame
	 * @see AcideCompilerConfigurationWindow
	 */
	public AcideCompilerConfigurationWindow buildAcideCompilerConfigurationWindow(
			JFrame parent) {
		return new AcideCompilerConfigurationWindow(parent);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console display options window.
	 * 
	 * @return the ACIDE - A Configurable IDE console display options window.
	 * @see AcideConsoleDisplayOptionsWindow
	 */
	public AcideConsoleDisplayOptionsWindow buildAcideConsoleDisplayOptionsWindow() {
		return new AcideConsoleDisplayOptionsWindow();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE file editor display options window.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor display options
	 *         window.
	 * @see AcideFileEditorDisplayOptionsWindow
	 */
	public AcideFileEditorDisplayOptionsWindow buildAcideFileEditorDisplayOptionsWindow() {
		return new AcideFileEditorDisplayOptionsWindow();
	}
}
