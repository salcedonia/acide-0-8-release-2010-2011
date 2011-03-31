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
package acide.gui.mainWindow;

import acide.configuration.fileEditor.AcideFileEditorConfiguration;
import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommandConfiguration;
import acide.configuration.workbench.AcideWorkbenchManager;
import acide.factory.gui.AcideGUIFactory;
import acide.gui.consolePanel.AcideConsolePanel;
import acide.gui.explorerPanel.AcideExplorerPanel;
import acide.gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import acide.gui.mainWindow.listeners.AcideMainWindowWindowListener;
import acide.gui.menuBar.AcideMenuBar;
import acide.gui.menuBar.projectMenu.gui.AcideNewProjectConfigurationWindow;
import acide.gui.statusBarPanel.AcideStatusBar;
import acide.gui.toolBarPanel.AcideToolBarPanel;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.HeadlessException;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

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
	 * ACIDE - A Configurable IDE new project configuration window.
	 */
	private AcideNewProjectConfigurationWindow _newProjectConfigurationWindow;
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
	private Component _lastElementOnFocus;

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

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s67"));

		// Builds the menu
		_menu = AcideGUIFactory.getInstance().buildAcideMenu();

		// Sets the menu bar
		setJMenuBar(_menu);

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

		// Adds the horizontal split panel to the window
		add(_horizontalSplitPanel, BorderLayout.CENTER);

		// Adds the status bar to the window
		add(_statusBar, BorderLayout.SOUTH);

		// Adds the Listeners
		addWindowListener(new AcideMainWindowWindowListener());

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
	 * Builds the ACIDE - A Configurable IDE main window tool bar panel with the
	 * different types of tool bars in the application.
	 */
	public void buildToolBarPanel() {

		String currentToolBarConfiguration = null;

		try {

			// Clears the console command configuration lists
			AcideConsoleCommandConfiguration.getInstance().clear();

			// Gets the ACIDE - A Configurable IDE current tool bar
			// configuration
			currentToolBarConfiguration = AcideResourceManager.getInstance()
					.getProperty("currentToolBarConfiguration");

			// Loads the console command final list from the current tool bar
			// configuration
			AcideConsoleCommandConfiguration.getInstance().loadFinalList(
					currentToolBarConfiguration);

			// Updates the ACIDE - A Configurable IDE current tool bar
			// configuration
			AcideResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Gets the grammar name
			String name;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");
			name = "./configuration/toolbar/"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {

				// Load the console command configuration final list
				AcideConsoleCommandConfiguration.getInstance().loadFinalList(
						name);

				// Information message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s958")
						+ currentToolBarConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957") + name);

				// Updates the ACIDE - A Configurable IDE current tool bar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", name);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s127"));

				try {

					// Loads the console command configuration final list by
					// default
					AcideConsoleCommandConfiguration.getInstance()
							.loadFinalList(
									"./configuration/toolbar/default.TBcfg");

					// Information message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s958")
							+ currentToolBarConfiguration
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s959"));

					// Updates the the ACIDE - A Configurable IDE current tool
					// bar configuration
					AcideResourceManager.getInstance().setProperty(
							"currentToolBarConfiguration",
							"./configuration/toolbar/default.TBcfg");
				} catch (HeadlessException exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}
			}

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s127"));
		}

		// Updates the tool bar panel
		updateToolBarPanel();
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
	 * Returns the ACIDE - A Configurable IDE new project configuration window.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window.
	 */
	public AcideNewProjectConfigurationWindow getNewProjectWindowConfiguration() {
		return _newProjectConfigurationWindow;
	}

	/**
	 * Sets a new value for the main window projectGUI.
	 * 
	 * @param newProjectConfigurationWindow
	 *            new value to set.
	 */
	public void setNewProjectConfigurationWindow(
			AcideNewProjectConfigurationWindow newProjectConfigurationWindow) {
		_newProjectConfigurationWindow = newProjectConfigurationWindow;
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
	 * Updates the ACIDE - A Configurable IDE main window tool bar panel.
	 * Removes the tool bar panel if exists, and builds it, adding it to the
	 * main window afterwards.
	 */
	public void updateToolBarPanel() {

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
	 * Sets a new value to the ACIDE - A Configurable IDE main window last
	 * element on focus.
	 * 
	 * @param lastElementOnFocus
	 *            new value to set.
	 */
	public void setLastElementOnFocus(Component lastElementOnFocus) {
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
	public Component getLastElementOnFocus() {

		if (_lastElementOnFocus != null)
			return _lastElementOnFocus;
		return getConsolePanel().getTextPane();
	}

	/**
	 * Shows the main window, once the workbech configuration has been loaded.
	 * As the main window is already visible, it is possible to paint the caret
	 * in the selected editor. It also closes the splash screen window and sets
	 * the workbench configuration loaded attribute to true.
	 */
	public void showAcideMainWindow() {

		// Centers the window
		setLocationRelativeTo(null);

		// Shows the main window
		AcideMainWindow.getInstance().setVisible(true);

		// The workbench has been loaded
		AcideWorkbenchManager.getInstance().setWorkbenchLoaded(true);

		// If there are files opened
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Sets the selected editor
			if (AcideFileEditorConfiguration.getInstance()
					.getSelectedFileEditorPanelIndex() != -1) {

				// If the selected file editor panel is inside the bounds
				if (AcideFileEditorConfiguration.getInstance()
						.getSelectedFileEditorPanelIndex() < AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPane()
						.getTabCount())

					// Puts the following into the DispatchEventThread
					SwingUtilities.invokeLater(new Runnable() {

						@Override
						public void run() {
							// Sets the selected file editor from the file
							// editor
							// configuration
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.updateRelatedComponentsAt(
											AcideFileEditorConfiguration
													.getInstance()
													.getSelectedFileEditorPanelIndex());
						}
					});

				else

					// Puts the following into the DispatchEventThread
					SwingUtilities.invokeLater(new Runnable() {

						@Override
						public void run() {
							// Sets the selected file editor as the last tab in
							// the file
							// editor
							// Updates the selected file editor index
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.updateRelatedComponentsAt(
											AcideMainWindow.getInstance()
													.getFileEditorManager()
													.getTabbedPane()
													.getTabCount() - 1);
						}
					});
			}
		} else {

			// Selects the explorer tree root node
			AcideMainWindow.getInstance().getExplorerPanel().getTree()
					.setSelectionInterval(0, 0);

			// Sets the focus in the explorer panel
			AcideMainWindow.getInstance().getExplorerPanel()
					.requestFocusInWindow();
		}
	}
}
