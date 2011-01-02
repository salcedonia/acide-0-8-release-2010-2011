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
package gui.mainWindow;

import es.configuration.fileEditor.AcideFileEditorConfiguration;
import es.configuration.project.workbench.AcideWorkbenchManager;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommandList;
import gui.consolePanel.AcideConsolePanel;
import gui.explorerPanel.AcideExplorerPanel;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.listeners.MainWindowWindowListener;
import gui.menuBar.Menu;
import gui.menuBar.projectMenu.gui.AcideNewProjectConfigurationWindow;
import gui.statusBarPanel.AcideStatusBar;
import gui.toolBarPanel.AcideToolBarPanel;

import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

import language.AcideLanguageManager;
import operations.factory.AcideGUIFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE main window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class MainWindow extends JFrame {

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
	private static MainWindow _instance;
	/**
	 * ACIDE - A Configurable IDE main window file editor manager.
	 */
	private AcideFileEditorManager _fileEditorManager;
	/**
	 * ACIDE - A Configurable IDE main window menu.
	 */
	private Menu _menu;
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
	 * Returns the unique ACIDE - A Configurable IDE main window class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE main window class instance.
	 */
	public static MainWindow getInstance() {
		if (_instance == null)
			_instance = new MainWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE main window.
	 */
	public MainWindow() {

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

		// Sets the window icon
		setIconImage(ICON.getImage());

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s67"));

		// Sets the title
		setTitle(labels.getString("s425"));

		// MENU
		_menu = AcideGUIFactory.getInstance().buildAcideMenu();

		// MENU BAR
		setJMenuBar(_menu.getMenuBar());

		// EXPLORER
		_explorerPanel = AcideGUIFactory.getInstance()
				.buildAcideExplorerPanel();

		// EDITOR
		_fileEditorManager = AcideGUIFactory.getInstance()
				.buildAcideFileEditorManager();

		// CONSOLE
		_consolePanel = AcideGUIFactory.getInstance().buildAcideConsolePanel();

		// STATUS BAR
		_statusBar = AcideGUIFactory.getInstance().buildAcideStatusBar();
		add(_statusBar, BorderLayout.SOUTH);

		// TOOLBAR
		buildToolBarPanel();

		// VERTICAL SPLIT PANEL
		_verticalSplitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				_explorerPanel, _fileEditorManager.getTabbedPane());
		_verticalSplitPanel.setResizeWeight(0.05);
		_verticalSplitPanel.setContinuousLayout(true);

		// HORIZONTAL SPLIT PANEL
		_horizontalSplitPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				_verticalSplitPanel, _consolePanel);
		_horizontalSplitPanel.setResizeWeight(0.9);
		_horizontalSplitPanel.setContinuousLayout(true);
		add(_horizontalSplitPanel, BorderLayout.CENTER);

		// Do not close automatically
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		setLocationRelativeTo(null);

		// Adds the Listeners
		addWindowListener(new MainWindowWindowListener());

		// Sets the menu listeners
		setMenuListeners();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s66"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE main window tool bar panel with the
	 * different types of tool bars in the application.
	 */
	public void buildToolBarPanel() {

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

		String currentToolBarConfiguration = null;

		try {

			ConsoleCommandList.clear();
			currentToolBarConfiguration = AcideResourceManager.getInstance()
					.getProperty("currentToolBarConfiguration");
			ConsoleCommandList.loadFinalList(currentToolBarConfiguration);

			// Updates the RESOURCE MANAGER
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

				// Load the command list
				ConsoleCommandList.loadFinalList(name);

				// Information message
				JOptionPane.showMessageDialog(null,
						labels.getString("s958") + currentToolBarConfiguration
								+ labels.getString("s957") + name);

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", name);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(labels.getString("s127"));

				try {

					// Loads the default grammar configuration
					ConsoleCommandList
							.loadFinalList("./configuration/toolbar/default.TBcfg");

					// Information message
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s958")
									+ currentToolBarConfiguration
									+ labels.getString("s959"));

					// Updates the RESOURCE MANAGER
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
			AcideLog.getLog().error(labels.getString("s127"));
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
	public Menu getMenu() {
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
	public AcideNewProjectConfigurationWindow getProjectWindowConfiguration() {
		return _newProjectConfigurationWindow;
	}

	/**
	 * Sets a new value for the main window projectGUI.
	 * 
	 * @param projectGUI
	 *            new value to set.
	 */
	public void setProjectGUI(AcideNewProjectConfigurationWindow projectGUI) {
		_newProjectConfigurationWindow = projectGUI;
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
	 * Shows the main window, once the workbech configuration has been loaded.
	 * As the main window is already visible, it is possible to paint the 
	 * caret in the selected editor.
	 * It also closes the splash screen window and sets the workbench configuration
	 * loaded attribute to true.
	 */
	public void showAcideMainWindow() {

		// Waits until all the repaints are done to show the main window
		// and closing the splash screen window
		SwingUtilities.invokeLater(new Runnable() {

			@Override
			public void run() {
	
				// Shows the main window
				MainWindow.getInstance().setVisible(true);

				// The workbench has been loaded
				AcideWorkbenchManager.getInstance().setWorkbenchLoaded(true);
				
				// Sets the selected editor
				if (AcideFileEditorConfiguration.getInstance().getSelectedFileEditorPanelIndex() != -1) {

					// Sets the selected file editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									AcideFileEditorConfiguration.getInstance().
											getSelectedFileEditorPanelIndex());

					// Sets the focus in the edition area
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getActiveTextEditionArea()
							.requestFocusInWindow();

					// Sets the caret visible
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getActiveTextEditionArea()
							.getCaret().setVisible(true);

					// Selects the tree node
					MainWindow.getInstance().getExplorerPanel()
							.selectTreeNodeFromFileEditor();

					// Updates the status bar with the selected editor
					MainWindow.getInstance().getStatusBar()
							.updatesStatusBarFromFileEditor();
				}
			}
		});	
	}
}
