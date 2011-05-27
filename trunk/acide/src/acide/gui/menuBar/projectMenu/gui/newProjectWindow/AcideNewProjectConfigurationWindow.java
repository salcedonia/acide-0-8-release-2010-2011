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
package acide.gui.menuBar.projectMenu.gui.newProjectWindow;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultMutableTreeNode;

import acide.configuration.project.AcideProjectConfiguration;
import acide.factory.gui.AcideGUIFactory;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE new project configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideNewProjectConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE new project configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE new project configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE new project configuration window unique class
	 * instance.
	 */
	private static AcideNewProjectConfigurationWindow _instance;
	/**
	 * ACIDE - A Configurable IDE new project configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window compiler
	 * panel.
	 */
	private JPanel _compilerPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window name text
	 * field.
	 */
	private JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace
	 * text field.
	 */
	private JTextField _workspaceTextField;
	/**
	 * ACIDE - A Configurable IDE new project configuration window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace
	 * label.
	 */
	private JLabel _workspaceLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window accept
	 * button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window cancel
	 * button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace
	 * button.
	 */
	private JButton _workspaceButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window compiler
	 * button.
	 */
	private JButton _compilerButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window output
	 * button.
	 */
	private JButton _outputButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace
	 * path string
	 */
	private String _workspacePath;
	/**
	 * ACIDE - A Configurable IDE new project configuration window flag that
	 * indicates if the compiler paths are defined or not.
	 */
	private boolean _areCompilerPathsDefined;
	/**
	 * ACIDE - A Configurable IDE new project configuration window flag that
	 * indicates if the shell paths are defined or not.
	 */
	private boolean _areShellPathsDefined;

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         unique class instance.
	 */
	public static AcideNewProjectConfigurationWindow getInstance() {
		if (_instance == null)
			_instance = new AcideNewProjectConfigurationWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE new project configuration
	 * window.
	 */
	public AcideNewProjectConfigurationWindow() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s587"));

		// Builds the window components
		buildComponents();

		// Sets the listener for the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE new project configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s588"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s613"));
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE new project
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipady = 10;

		// Adds the name label to the main panel
		_mainPanel.add(_nameLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 250;
		constraints.ipady = 0;

		// Adds the name text field to the main panel
		_mainPanel.add(_nameTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;

		// Adds the workspace label to the main panel
		_mainPanel.add(_workspaceLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;

		// Adds the workspace text field to the main panel
		_mainPanel.add(_workspaceTextField, constraints);

		constraints.gridx = 2;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;

		// Adds the workspace button to the main panel
		_mainPanel.add(_workspaceButton, constraints);

		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the main panel to the window
		add(_mainPanel, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the compiler button to the compiler panel
		_compilerPanel.add(_compilerButton, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 1;
		constraints.gridy = 0;

		// Adds the output button to the compiler panel
		_compilerPanel.add(_outputButton, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 2;

		// Adds the compiler panel to the window
		add(_compilerPanel, constraints);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 3;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE new project configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Sets the main panel border
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s589"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the compiler panel
		_compilerPanel = new JPanel(new GridBagLayout());

		// Sets the compiler panel border
		_compilerPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s591"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the name label
		_nameLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s592"));

		// Creates the name text field
		_nameTextField = new JTextField();

		// Sets the name text field tool tip text
		_nameTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s593"));

		// Creates the output button
		_outputButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s637"));

		// Sets the output button horizontal alignment as center
		_outputButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the output button tool tip text
		_outputButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s637"));

		// Creates the compiler button
		_compilerButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s636"));

		// Sets the compiler button horizontal alignment as center
		_compilerButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the compiler button tool tip text
		_compilerButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s636"));

		// Creates the workspace button
		_workspaceButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s948"));

		// Sets the workspace button horizontal alignment as right
		_workspaceButton.setHorizontalAlignment(JButton.RIGHT);

		// Creates the workspace text field
		_workspaceTextField = new JTextField("");

		// Creates the workspace label
		_workspaceLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s949"));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// Sets the accept button horizontal alignment as center
		_acceptButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the accept button tool tip text
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s611"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Sets the cancel button horizontal alignment as center
		_cancelButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s612"));

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE new project
	 * configuration window components.
	 */
	public void setListeners() {

		// Sets the workspace button action listener
		_workspaceButton.addActionListener(new WorkspaceButtonButtonAction());

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonButtonAction());

		// Sets the output button action listener
		_outputButton.addActionListener(new OutputButtonButtonAction());

		// Sets the compiler button action listener
		_compilerButton.addActionListener(new CompilerButtonButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * Displays the ACIDE - A Configurable IDE new project configuration window.
	 */
	public void showWindow() {

		// The compiler paths are not defined yet
		_areCompilerPathsDefined = false;

		// The shell paths are not defined yet
		_areShellPathsDefined = false;

		// Empties the name text field
		_nameTextField.setText("");

		// Empties the workspace text field
		_workspaceTextField.setText("");

		// Displays the window
		setVisible(true);
	}

	/**
	 * Closes the ACIDE - A Configurable IDE new project configuration window.
	 */

	public void closeWindow() {

		// Enables the main window again
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		setVisible(false);

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * Updates the ACIDE - A Configurable IDE explorer panel.
	 */
	public void updateExplorerPanel() {

		// Removes the previous files
		AcideProjectConfiguration.getInstance().removeFiles();

		// Removes all the children in the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().getRoot()
				.removeAllChildren();

		// Creates the root node in the explorer tree
		AcideProjectFile rootProjectFile = new AcideProjectFile();

		// Sets its absolute path
		rootProjectFile.setAbsolutePath(_nameTextField.getText());

		// Sets its name
		rootProjectFile.setName(_nameTextField.getText());

		// It has no parent in the tree
		rootProjectFile.setParent(null);

		// It is a directory
		rootProjectFile.setIsDirectory(true);

		// Creates the root node from the root project file info
		DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode(
				rootProjectFile);

		// Allows children below it
		rootNode.setAllowsChildren(true);

		// Adds the root node to the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().getRoot()
				.add(rootNode);

		// Adds all the editors currently opened
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			// Except the NEW FILE and the LOG tab
			if (!AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).isNewFile()
					&& !AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).isLogFile()) {

				// Creates the new project file
				AcideProjectFile newProjectFile = new AcideProjectFile();

				// Sets its is main file
				newProjectFile.setIsMainFile(AcideMainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.isMainFile());

				// Sets its is compilable file
				newProjectFile.setIsCompilableFile(AcideMainWindow
						.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).isCompilableFile());

				// Sets its absolute path
				newProjectFile.setAbsolutePath(AcideMainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.getAbsolutePath());

				// Sets its parent, in this case the root node
				newProjectFile.setParent(rootNode.toString());

				// Adds the file to the project configuration
				AcideProjectConfiguration.getInstance().addFile(newProjectFile);

				// Gets the file editor panel absolute path
				String fileAbsolutePath = AcideMainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.getAbsolutePath();

				// Gets the last index of slash
				int lastIndexOfSlash = fileAbsolutePath.lastIndexOf("\\");
				if (lastIndexOfSlash == -1)
					lastIndexOfSlash = fileAbsolutePath.lastIndexOf("/");

				// Gets the file name
				String fileName = fileAbsolutePath.substring(
						lastIndexOfSlash + 1, fileAbsolutePath.length());

				// Sets its name
				newProjectFile.setName(fileName);

				// Creates the node from the new project file info
				DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
						newProjectFile);

				// As it is a file, it does not allow to have children
				newNode.setAllowsChildren(false);

				// Adds the new node to the root node
				rootNode.add(newNode);
			}
		}
	}

	/**
	 * Updates the ACIDE - A Configurable IDE file editor.
	 * 
	 * Closes and opens all the opened files so it can preserve their document
	 * listeners and UndoManagers.
	 */
	public void updateFileEditor() {

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels();

		// Removes all the tabs in the tabbed pane
		for (int index = 0; index < numberOfFileEditorPanels; index++) {

			// Sets the selected file editor panel at 0
			AcideMainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(0);

			// Removes it
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().remove(0);

			// Validates the changes in the tabbed pane
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().validate();
		}

		// Open all the files once again in the file editor
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getOpenAllFilesMenuItem().doClick();
	}

	/**
	 * Updates the ACIDE - A Configurable IDE project configuration.
	 */
	public void updateProjectConfiguration() {

		try {

			// Gets the last index of slash
			String separator = "\\";
			int lastIndexOfSlash = _workspaceTextField.getText().lastIndexOf(
					separator);
			if (lastIndexOfSlash == -1)
				separator = "/";

			// Builds the file path
			String filePath = _workspaceTextField.getText() + separator
					+ _nameTextField.getText();

			// If the file path does not content the extension
			if (!filePath.contains(".acideProject"))

				// Adds it to the file path
				filePath = filePath + ".acideProject";

			// Gets the previous project name
			String previousProjectName = AcideProjectConfiguration
					.getInstance().getName();

			// Sets the project name in the ACIDE - A Configurable IDE
			// project configuration
			AcideProjectConfiguration.getInstance().setName(
					_nameTextField.getText());

			// Sets the project path in the ACIDE - A
			// Configurable IDE project configuration
			AcideProjectConfiguration.getInstance().setProjectPath(filePath);

			// If the compiler paths are not defined
			if (!_areCompilerPathsDefined) {

				// Sets the compiler path in the configuration as
				// null
				AcideProjectConfiguration.getInstance().setCompilerPath(null);

				// Sets the compiler arguments in the configuration
				// as null
				AcideProjectConfiguration.getInstance().setCompilerArguments(
						null);
			} else {

				// Sets the compiler path in the configuration as
				// null
				if (AcideProjectConfiguration.getInstance().getCompilerPath()
						.equals(""))
					AcideProjectConfiguration.getInstance().setCompilerPath(
							null);

				// Sets the compiler arguments in the configuration
				// as null
				if (AcideProjectConfiguration.getInstance()
						.getCompilerArguments().equals(""))
					AcideProjectConfiguration.getInstance()
							.setCompilerArguments(null);
			}

			// Updates the file list
			AcideProjectConfiguration.getInstance()
					.updateFileListProjectConfiguration(previousProjectName,
							_nameTextField.getText());

			// Saves the content in the project configuration file
			String fileContent = AcideProjectConfiguration.getInstance().save();

			// Saves the file content into the project file
			AcideFileManager.getInstance().write(
					AcideProjectConfiguration.getInstance().getProjectPath(),
					fileContent);

			// Updates the ACIDE - A Configurable IDE project configuration
			AcideResourceManager.getInstance().setProperty(
					"projectConfiguration", filePath);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the ACIDE - A Configurable IDE tool bar configuration.
	 */
	public void updateToolBarConfiguration() {

		try {
			// Gets the ACIDE - A Configurable IDE current tool bar
			// configuration
			String currentToolBarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");

			// Sets the ACIDE - A Configurable IDE tool bar configuration
			AcideProjectConfiguration.getInstance().setToolBarConfiguration(
					currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the menu configuration.
	 */
	public void updateMenuConfiguration() {

		try {
			// Gets the ACIDE - A Configurable IDE current menu configuration
			String currentMenuConfiguration = AcideResourceManager
					.getInstance().getProperty("currentMenuConfiguration");

			// Sets the ACIDE - A Configurable IDE current menu configuration
			AcideProjectConfiguration.getInstance().setMenuConfiguration(
					currentMenuConfiguration);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the ACIDE - A Configurable IDE console panel.
	 */
	public void updateConsolePanel() {

		// Exits the console
		AcideMainWindow.getInstance().getConsolePanel().executeExitCommand();

		// Sets the shell directory in the resource manager from the
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.shellDirectory",
				AcideProjectConfiguration.getInstance().getShellDirectory());

		// Sets the shell path in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.shellPath",
				AcideProjectConfiguration.getInstance().getShellPath());

		// Sets the echo command in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.isEchoCommand",
				String.valueOf(AcideProjectConfiguration.getInstance()
						.getIsEchoCommand()));

		// Sets the exit command in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.exitCommand",
				AcideProjectConfiguration.getInstance().getExitCommand());

		// Sets the font name in the resource manager
		AcideResourceManager.getInstance().setProperty("consolePanel.fontName",
				AcideProjectConfiguration.getInstance().getFontName());

		// Sets the font style in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.fontStyle",
				String.valueOf(AcideProjectConfiguration.getInstance()
						.getFontStyle()));

		// Sets the font size in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.fontSize",
				String.valueOf(AcideProjectConfiguration.getInstance()
						.getFontSize()));

		// Sets the foreground color in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.foregroundColor",
				Integer.toString(AcideProjectConfiguration.getInstance()
						.getForegroundColor().getRGB()));

		// Sets the background color in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.backgroundColor",
				Integer.toString(AcideProjectConfiguration.getInstance()
						.getBackgroundColor().getRGB()));

		// Resets the console panel
		AcideMainWindow.getInstance().getConsolePanel().resetConsole();

		// Updates the look and feel
		AcideMainWindow.getInstance().getConsolePanel().setLookAndFeel();
	}

	/**
	 * Updates the ACIDE - A Configurable IDE main window configuration.
	 */
	public void updateMainWindowConfiguration() {

		// Updates the main window title
		AcideMainWindow.getInstance().setTitle(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s425")
						+ " - " + _nameTextField.getText());

		// Sets the is explorer panel showed flag as true
		AcideProjectConfiguration.getInstance().setIsExplorerPanelShowed(true);

		// Sets the is console panel showed flag as true
		AcideProjectConfiguration.getInstance().setIsConsolePanelShowed(true);

		// Sets the window width
		AcideProjectConfiguration.getInstance().setWindowWidth(
				AcideMainWindow.getInstance().getWidth());

		// Sets the window height
		AcideProjectConfiguration.getInstance().setWindowHeight(
				AcideMainWindow.getInstance().getHeight());

		// Sets the window x coordinate
		AcideProjectConfiguration.getInstance().setXCoordinate(
				AcideMainWindow.getInstance().getX());

		// Sets the window y coordinate
		AcideProjectConfiguration.getInstance().setYCoordinate(
				AcideMainWindow.getInstance().getY());

		// Sets the vertical split pane divider location
		AcideProjectConfiguration.getInstance()
				.setVerticalSplitPaneDividerLocation(
						AcideMainWindow.getInstance().getExplorerPanel()
								.getWidth());

		// Sets the horizontal split pane divider location
		AcideProjectConfiguration.getInstance()
				.setHorizontalSplitPaneDividerLocation(
						AcideMainWindow.getInstance().getConsolePanel()
								.getHeight());

		// Enables the add file menu item in the explorer panel
		// popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getAddFileMenuItem().setEnabled(true);

		// Enables the save project menu item in the explorer
		// panel popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getSaveProjectMenuItem().setEnabled(true);

		// Enables the save project button in the menu bar tool
		// bar
		AcideMainWindow.getInstance().getToolBarPanel().getMenuBarToolBar()
				.getSaveProjectButton().setEnabled(true);

		// Notifies to the model about the changes
		AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
				.reload();

		// Expands the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().expandTree();

		// Sets the show explorer panel check box menu item as
		// selected
		AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowExplorerPanelCheckBoxMenuItem().setSelected(true);

		// Sets the show console panel check box menu item as
		// selected
		AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowConsolePanelCheckBoxMenuItem().setSelected(true);

		// Enables the project menu
		AcideMainWindow.getInstance().getMenu().getProjectMenu().enableMenu();

		// Enables the open all files menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getOpenAllFilesMenuItem().setEnabled(true);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * workspace path.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         workspace path.
	 */
	public String getWorkspacePath() {
		return _workspacePath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window workspace path.
	 * 
	 * @param workspacePath
	 *            new value to set.
	 */
	public void setWorkspacePath(String workspacePath) {
		_workspacePath = workspacePath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window are shell paths defined flag.
	 * 
	 * @param areShellPathsDefined
	 *            new value to set.
	 */
	public void setAreShellPathsDefined(boolean areShellPathsDefined) {
		_areShellPathsDefined = areShellPathsDefined;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * are shell paths defined flag.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         are shell paths defined flag.
	 */
	public boolean getAreShellPathsDefined() {
		return _areShellPathsDefined;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window are compiler paths defined flag.
	 * 
	 * @param areCompilerPathsDefined
	 *            new value to set.
	 */
	public void setAreCompilerPathsDefined(boolean areCompilerPathsDefined) {
		_areCompilerPathsDefined = areCompilerPathsDefined;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * are compiler paths defined flag.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         are compiler paths defined flag.
	 */
	public boolean getAreCompilerPathsDefined() {
		return _areCompilerPathsDefined;
	}

	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class WorkspaceButtonButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			File selectedFile = null;
			String absolutePath = null;
			String lastPath = null;

			// Creates the file chooser
			JFileChooser _fileChooser = new JFileChooser();

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedProjectDirectory");

				// Sets the current directory to the default path
				_fileChooser.setCurrentDirectory(new File(lastPath));

				// Disables the multiple selection of files
				_fileChooser.setMultiSelectionEnabled(false);

				// Sets only directories
				_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

				// Ask for the file to the user
				int returnValue = _fileChooser.showOpenDialog(null);

				// If it is ok
				if (returnValue == JFileChooser.APPROVE_OPTION) {

					// Gets the selected file
					selectedFile = _fileChooser.getSelectedFile();

					// Gets the absolute path
					absolutePath = selectedFile.getAbsolutePath();

					if (absolutePath != null) {

						// Sets the workspace path
						_workspacePath = absolutePath;

						// Updates the workspace text field
						_workspaceTextField.setText(_workspacePath);

						// Validates the changes in the workspace text field
						_workspaceTextField.validate();

						// Repaints the workspace text field
						_workspaceTextField.repaint();

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s300")
										+ absolutePath);

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedProjectDirectory", absolutePath);
					}
				} else if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels the selection
					_fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s302"));
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE new project configuration window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If the required parameters are set
			if (!_nameTextField.getText().equals("")) {

				// Gets the project name to check if we are overwriting it
				String filePath = "";
				String separator = "\\";
				int lastIndexOfSlash = _workspaceTextField.getText()
						.lastIndexOf(separator);
				if (lastIndexOfSlash == -1)
					separator = "/";

				// Compounds the file path
				filePath = _workspaceTextField.getText() + separator
						+ _nameTextField.getText();

				// If it does not contain the extension
				if (!filePath.contains(".acideProject"))

					// Adds it to the name
					filePath = filePath + ".acideProject";

				// Checks if the project already exists
				File fileProject = new File(filePath);

				// If the file exists
				if (fileProject.exists()) {

					// Ask to the user if he wants to save it
					int resultValueSaving = JOptionPane.showConfirmDialog(null,
							AcideLanguageManager.getInstance().getLabels()
									.getString("s955"), AcideLanguageManager
									.getInstance().getLabels()
									.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					// If it yes
					if (resultValueSaving == JOptionPane.YES_OPTION) {

						// Overwrites the file
						AcideMainWindow.getInstance().getMenu()
								.getProjectMenu().saveProjectAs(filePath);

						// Closes the window
						closeWindow();
					}
				} else {

					// Updates the main window with the configuration

					// Updates the project configuration
					updateProjectConfiguration();

					// Updates the menu configuration
					updateMenuConfiguration();

					// Updates the tool bar configuration
					updateToolBarConfiguration();

					// Updates the console panel
					updateConsolePanel();

					// Updates the explorer panel
					updateExplorerPanel();

					// Updates the file editor
					updateFileEditor();

					// Updates the main window configuration
					updateMainWindowConfiguration();

					// Sets the is not the first save flag in the ACIDE - A
					// Configurable IDE project configuration
					AcideProjectConfiguration.getInstance()
							.setIsFirstSave(true);

					// The ACIDE - A Configurable IDE project configuration has
					// not
					// been modified yet
					AcideProjectConfiguration.getInstance()
							.setIsModified(false);

					// Closes the window
					closeWindow();
				}

			} else {

				// If the project name is empty
				if (_nameTextField.getText().equals(""))
					// Shows a warning message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s973"),
							AcideLanguageManager.getInstance().getLabels()
									.getString("s972"),
							JOptionPane.WARNING_MESSAGE);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE new project configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s614"));

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE new project configuration window output button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class OutputButtonButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Builds the console configuration window
			AcideGUIFactory.getInstance()
					.buildAcideConsoleConfigurationWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE new project configuration window compiler
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CompilerButtonButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Builds the compiler configuration window
			AcideGUIFactory.getInstance()
					.buildAcideCompilerConfigurationWindow();
		}
	}
}
