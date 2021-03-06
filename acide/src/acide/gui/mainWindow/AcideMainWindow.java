/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.gui.mainWindow;

import java.awt.BorderLayout;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JSplitPane;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.factory.gui.AcideGUIFactory;
import acide.gui.consolePanel.AcideConsolePanel;
import acide.gui.explorerPanel.AcideExplorerPanel;
import acide.gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import acide.gui.mainWindow.listeners.AcideMainWindowWindowListener;
import acide.gui.mainWindow.utils.AcideLastElementOnFocus;
import acide.gui.menuBar.AcideMenuBar;
import acide.gui.statusBarPanel.AcideStatusBar;
import acide.gui.toolBarPanel.AcideToolBarPanel;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE main window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideMainWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE main window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE main window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE main window unique class instance.
	 */
	private static AcideMainWindow _instance;
	/**
	 * ACIDE - A Configurable IDE main window file editor manager.
	 */
	private AcideFileEditorManager _fileEditorManager;
	/**
	 * ACIDE - A Configurable IDE main window menu.
	 */
	private AcideMenuBar _menu;
	/**
	 * ACIDE - A Configurable IDE main window console panel.
	 */
	private AcideConsolePanel _consolePanel;
	/**
	 * ACIDE - A Configurable IDE main window status bar.
	 */
	private AcideStatusBar _statusBar;
	/**
	 * ACIDE - A Configurable IDE main window explorer panel.
	 */
	private AcideExplorerPanel _explorerPanel;
	/**
	 * ACIDE - A Configurable IDE main window tool bar panel.
	 */
	private AcideToolBarPanel _toolBarPanel;
	/**
	 * ACIDE - A Configurable IDE main window vertical split panel.
	 */
	private JSplitPane _verticalSplitPanel;
	/**
	 * ACIDE - A Configurable IDE main window horizontal split panel.
	 */
	private JSplitPane _horizontalSplitPanel;
	/**
	 * ACIDE - A Configurable IDE main window last element on focus.
	 */
	private AcideLastElementOnFocus _lastElementOnFocus;

	/**
	 * Returns the unique ACIDE - A Configurable IDE main window class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE main window class instance.
	 */
	public static AcideMainWindow getInstance() {
		if (_instance == null)
			_instance = new AcideMainWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE main window.
	 */
	public AcideMainWindow() {

		// Builds the window components
		buildComponents();

		// Adds the components to the window
		addComponents();

		// Sets the listeners
		setListeners();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE main window.
	 */
	private void addComponents() {

		// Sets the menu bar
		setJMenuBar(_menu);

		// Adds the horizontal split panel to the window
		add(_horizontalSplitPanel, BorderLayout.CENTER);

		// Adds the status bar to the window
		add(_statusBar, BorderLayout.SOUTH);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE main window components.
	 */
	private void buildComponents() {

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s67"));

		// Builds the menu
		_menu = AcideGUIFactory.getInstance().buildAcideMenu();

		// Sets the menu listeners
		setMenuListeners();

		// Builds the explorer panel
		_explorerPanel = AcideGUIFactory.getInstance()
				.buildAcideExplorerPanel();

		// Builds the file editor manager
		_fileEditorManager = AcideGUIFactory.getInstance()
				.buildAcideFileEditorManager();

		// Builds the console panel
		_consolePanel = AcideGUIFactory.getInstance().buildAcideConsolePanel();

		// Builds the status bar
		_statusBar = AcideGUIFactory.getInstance().buildAcideStatusBar();

		// Builds the tool bar panel
		buildToolBarPanel();

		// Creates the vertical split pane with the explorer and the file editor
		// manager tabbed pane
		_verticalSplitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				_explorerPanel, _fileEditorManager.getTabbedPane());

		// Sets its resize weight to 0.05
		_verticalSplitPanel.setResizeWeight(0.05);

		// Displays its components when the user is resizing the vertical split
		// pane
		_verticalSplitPanel.setContinuousLayout(true);

		// Creates the horizontal split pane with the vertical split panel and
		// the console panel
		_horizontalSplitPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				_verticalSplitPanel, _consolePanel);

		// Sets its resize weight to 0.9
		_horizontalSplitPanel.setResizeWeight(0.9);

		// Displays its components when the user is resizing the horizontal
		// split
		// pane
		_horizontalSplitPanel.setContinuousLayout(true);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE main window window configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s425"));

		// Sets the window icon
		setIconImage(ICON.getImage());

		// Do not close automatically
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s66"));
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE main window.
	 */
	private void setListeners() {

		// Adds the window listener to the ACIDE - A Configurable IDE main
		// window
		addWindowListener(new AcideMainWindowWindowListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE main window tool bar panel with the
	 * different types of tool bars in the application.
	 */
	public void buildToolBarPanel() {

		if (_toolBarPanel != null)
			remove(_toolBarPanel);

		// Creates the tool bar
		_toolBarPanel = new AcideToolBarPanel();

		// Builds the tool bar panel
		_toolBarPanel.buildAcideToolBarPanel();

		// Adds the tool bar panel to the main window
		add(_toolBarPanel, BorderLayout.NORTH);
	}

	/**
	 * Shows the main window, once the ACIDE - A Configurable IDE workbench
	 * configuration has been loaded.
	 */
	public void showAcideMainWindow() {

		// The workbench has been loaded
		AcideWorkbenchConfiguration.getInstance().setWorkbenchLoaded(true);

		// Shows the main window
		AcideMainWindow.getInstance().setVisible(true);
	}

	/**
	 * <p>
	 * Performs the ACIDE - A Configurable IDE closing operation.
	 * </p>
	 * <p>
	 * Asks for saving the changes, if any, in the project configuration.
	 * Besides if there are any modified file editor opened, asks for saving
	 * them to the user as well.
	 * </p>
	 * <p>
	 * Once the two previous processes are done, the workbench manager saves its
	 * configuration.
	 * </p>
	 */
	public void closeAcideMainWindow() {

		// If it is the default project
		if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Saves the file editor configuration
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.askForSavingModifiedFiles()) {

				// Saves the default configuration
				AcideWorkbenchConfiguration.getInstance()
						.saveDefaultConfiguration();

				// Save the rest of the workbench configuration
				AcideWorkbenchConfiguration.getInstance()
						.saveComponentsConfiguration();

				// Saves the workbench configuration into its configuration
				// file
				AcideWorkbenchConfiguration.getInstance().save();

				// Closes the main window
				System.exit(0);
			}

		} else {

			// Asks for saving the project configuration
			if (AcideProjectConfiguration.getInstance()
					.askForSavingProjectConfiguration()) {

				// Saves the file editor configuration
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.askForSavingModifiedFiles()) {

					// Save the rest of the workbench configuration
					AcideWorkbenchConfiguration.getInstance()
							.saveComponentsConfiguration();

					// Saves the workbench configuration into its configuration
					// file
					AcideWorkbenchConfiguration.getInstance().save();

					// Closes the main window
					System.exit(0);
				}
			}
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window console panel.
	 * 
	 * @return the ACIDE - A Configurable IDE main window console panel.
	 */
	public AcideConsolePanel getConsolePanel() {
		return _consolePanel;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window menu.
	 * 
	 * @return the ACIDE - A Configurable IDE main window menu.
	 */
	public AcideMenuBar getMenu() {
		return _menu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window file editor manager.
	 * 
	 * @return the ACIDE - A Configurable IDE main window file editor manager.
	 */
	public AcideFileEditorManager getFileEditorManager() {
		return _fileEditorManager;
	}

	/**
	 * Set the ACIDE - A Configurable IDE main window listeners.
	 */
	public void setMenuListeners() {
		_menu.setListeners();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window explorer panel.
	 * 
	 * @return the ACIDE - A Configurable IDE main window explorer panel.
	 */
	public AcideExplorerPanel getExplorerPanel() {
		return _explorerPanel;
	}

	/**
	 * Set a new value for the ACIDE - A Configurable IDE main window explorer
	 * panel.
	 * 
	 * @param explorerPanel
	 *            new value to set.
	 */
	public void setExplorerPanel(AcideExplorerPanel explorerPanel) {
		_explorerPanel = explorerPanel;
	}

	/**
	 * Returns the the ACIDE - A Configurable IDE main window status bar.
	 * 
	 * @return the the ACIDE - A Configurable IDE main window status bar.
	 */
	public AcideStatusBar getStatusBar() {
		return _statusBar;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window horizontal split pane.
	 * 
	 * @return the ACIDE - A Configurable IDE main window horizontal split pane.
	 */
	public JSplitPane getHorizontalSplitPane() {
		return _horizontalSplitPanel;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE main window
	 * horizontal split pane.
	 * 
	 * @param horizontalSplitPane
	 *            new value to set.
	 */
	public void setHorizontalSplitPane(JSplitPane horizontalSplitPane) {
		_horizontalSplitPanel = horizontalSplitPane;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE main window explorer
	 * panel size.
	 * 
	 * @param size
	 *            new value to set.
	 */
	public void setExplorerPanelSize(int size) {
		_explorerPanel.setExplorerSize(size);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window vertical split panel.
	 * 
	 * @return the ACIDE - A Configurable IDE main window vertical split panel.
	 */
	public JSplitPane getVerticalSplitPane() {
		return _verticalSplitPanel;
	}

	/**
	 * Set a new value for the ACIDE - A Configurable IDE main window vertical
	 * split panel.
	 * 
	 * @param verticalSplitPane
	 *            new value to set.
	 */
	public void setVerticalSplitPane(JSplitPane verticalSplitPane) {
		_verticalSplitPanel = verticalSplitPane;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window tool bar panel.
	 * 
	 * @return the ACIDE - A Configurable IDE main window tool bar panel.
	 */
	public AcideToolBarPanel getToolBarPanel() {
		return _toolBarPanel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE main window tool bar
	 * panel.
	 * 
	 * @param toolBar
	 *            new value to set.
	 */
	public void setToolBarPanel(AcideToolBarPanel toolBar) {
		_toolBarPanel = toolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE main window last
	 * element on focus.
	 * 
	 * @param lastElementOnFocus
	 *            new value to set.
	 */
	public void setLastElementOnFocus(AcideLastElementOnFocus lastElementOnFocus) {
		_lastElementOnFocus = lastElementOnFocus;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE main window last element on focus.
	 * If the last element on focus is null then returns the console panel by
	 * default.
	 * 
	 * @return returns the ACIDE - A Configurable IDE main window last element
	 *         on focus. If the last element on focus is null then returns the
	 *         console panel by default.
	 */
	public AcideLastElementOnFocus getLastElementOnFocus() {

		if (_lastElementOnFocus != null)
			return _lastElementOnFocus;
		return AcideLastElementOnFocus.CONSOLE_PANEL;
	}
}
