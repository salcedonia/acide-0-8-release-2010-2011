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
package acide.gui.menuBar.projectMenu.gui;

import acide.configuration.console.AcideConsoleConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.window.AcideWindowConfiguration;
import acide.factory.gui.AcideGUIFactory;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.toolBarPanel.staticToolBar.AcideStaticToolBar;

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
	 * Creates a new ACIDE - A Configurable IDE new project configuration
	 * window.
	 */
	public AcideNewProjectConfigurationWindow() {

		// Initializes the variables
		_areCompilerPathsDefined = false;
		_areShellPathsDefined = false;

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s587"));

		// Sets the layout
		setLayout(new GridBagLayout());

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

		// Sets the listener for the window components
		setListeners();

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
		
		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);
		
		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
		
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 3;
		
		// Adds the button panel to the window
		add(_buttonPanel, constraints);

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

		// Displays the window
		setVisible(true);
		
		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s613"));
	}

	/**
	 * Sets the listeners for the window components.
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
	}

	/**
	 * Updates the explorer panel.
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
	 * Updates the file editor.
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
	 * Updates the project configuration.
	 */
	public void updateProjectConfiguration() {

		// Sets the project name
		AcideProjectConfiguration.getInstance().setName(
				_nameTextField.getText());

		// Is not the first save
		AcideProjectConfiguration.getInstance().setFirstSave(false);

		// If the compiler paths are not defined
		if (!_areCompilerPathsDefined) {

			// Sets the compiler path in the configuration as
			// null
			AcideProjectConfiguration.getInstance().setCompilerPath(null);

			// Sets the compiler arguments in the configuration
			// as null
			AcideProjectConfiguration.getInstance().setCompilerArguments(null);
		} else {

			// Sets the compiler path in the configuration as
			// null
			if (AcideProjectConfiguration.getInstance().getCompilerPath()
					.equals(""))
				AcideProjectConfiguration.getInstance().setCompilerPath(null);

			// Sets the compiler arguments in the configuration
			// as null
			if (AcideProjectConfiguration.getInstance().getCompilerArguments()
					.equals(""))
				AcideProjectConfiguration.getInstance().setCompilerArguments(
						null);
		}

		try {

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Adds the extension
				String[] askExtension = new String[] { "acidePrj" };
				AcideFileManager
						.getInstance()
						.getFileChooser()
						.addChoosableFileFilter(
								new AcideFileExtensionFilterManager(
										askExtension, AcideLanguageManager
												.getInstance().getLabels()
												.getString("s328")));

				// Gets the last index of slash
				String separator = "\\";
				int lastIndexOfSlash = _workspaceTextField.getText()
						.lastIndexOf(separator);
				if (lastIndexOfSlash == -1)
					separator = "/";

				// Builds the name
				String fileName = _workspaceTextField.getText() + separator
						+ _nameTextField.getText();

				// If the name does not content the extension
				if (!fileName.contains(".acidePrj"))

					// Adds it
					fileName = fileName + ".acidePrj";

				// Sets the project path in the project configuration
				AcideProjectConfiguration.getInstance().setPath(fileName);

				// Saves the content in the project configuration file
				String fileContent = AcideProjectConfiguration.getInstance()
						.save();

				// Saves the file
				AcideFileManager.getInstance().write(
						AcideProjectConfiguration.getInstance()
								.getProjectPath(), fileContent);

				// It is the first time the project has been saved
				AcideProjectConfiguration.getInstance().setFirstSave(true);

				// Updates the ACIDE - A Configurable IDE project configuration
				AcideResourceManager.getInstance().setProperty(
						"projectConfiguration", fileName);

				// Updates the RESOURCE MANAGER default path
				AcideResourceManager.getInstance().setProperty("defaultPath",
						fileName);

				// The project configuration has not been
				// modified yet
				AcideProjectConfiguration.getInstance().setIsModified(false);
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the tool bar configuration.
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
	 * Updates the main window configuration.
	 */
	public void updateMainWindowConfiguration() {

		// Updates the main window title
		AcideMainWindow.getInstance().setTitle(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s425")
						+ " - " + _nameTextField.getText());

		// Sets the is explorer panel showed flag as true
		AcideWindowConfiguration.getInstance().setIsExplorerPanelShowed(true);

		// Sets the is console panel showed flag as true
		AcideWindowConfiguration.getInstance().setIsConsolePanelShowed(true);

		// Sets the window width
		AcideWindowConfiguration.getInstance().setWindowWidth(
				AcideMainWindow.getInstance().getWidth());

		// Sets the window height
		AcideWindowConfiguration.getInstance().setWindowHeight(
				AcideMainWindow.getInstance().getHeight());

		// Sets the window x coordinate
		AcideWindowConfiguration.getInstance().setXCoordinate(
				AcideMainWindow.getInstance().getX());

		// Sets the window y coordinate
		AcideWindowConfiguration.getInstance().setYCoordinate(
				AcideMainWindow.getInstance().getY());

		// Sets the vertical split pane divider location
		AcideWindowConfiguration.getInstance()
				.setVerticalSplitPaneDividerLocation(
						AcideMainWindow.getInstance().getExplorerPanel()
								.getWidth());

		// Sets the horizontal split pane divider location
		AcideWindowConfiguration.getInstance()
				.setHorizontalSplitPaneDividerLocation(
						AcideMainWindow.getInstance().getConsolePanel()
								.getHeight());

		// Validates the changes in the main window
		AcideMainWindow.getInstance().validate();

		// Repaints the main window
		AcideMainWindow.getInstance().repaint();

		// Enables the add file menu item in the explorer panel
		// popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getAddFileMenuItem().setEnabled(true);

		// Enables the save project menu item in the explorer
		// panel popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getSaveProjectMenuItem().setEnabled(true);

		// Enables the save project button in the static tool
		// bar
		AcideStaticToolBar.getInstance().getSaveProjectButton()
				.setEnabled(true);

		// Notifies to the model about the changes
		AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
				.reload();

		// Expands the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().expandTree();

		// Enables the main window
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);
		
		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
		
		// If the show explorer panel menu item is not selected
		if (!AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowExplorerPanelCheckBoxMenuItem().isSelected())

			// Shows the explorer panel
			AcideMainWindow.getInstance().getExplorerPanel()
					.showExplorerPanel();

		// Sets the show explorer panel check box menu item as
		// selected
		AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowExplorerPanelCheckBoxMenuItem().setSelected(true);

		// Sets the show console panel check box menu item as
		// selected
		AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowConsolePanelCheckBoxMenuItem().setSelected(true);

		// Enables the project menu
		AcideMainWindow.getInstance().getMenu().enableProjectMenu();

		// Enables the open all files menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getOpenAllFilesMenuItem().setEnabled(true);
	}

	/**
	 * Updates the console configuration.
	 */
	public void updateConsoleConfiguration() {

		// Sets the shell path in the configuration
		// as null
		AcideConsoleConfiguration.getInstance().setShellPath(
				AcideConsoleConfiguration.getInstance().getShellPath());

		// Sets the shell directory in the configuration
		// as null
		AcideConsoleConfiguration.getInstance().setShellDirectory(
				AcideConsoleConfiguration.getInstance().getShellDirectory());

		// Sets the exit command by default
		AcideConsoleConfiguration.getInstance().setExitCommand(
				AcideConsoleConfiguration.getInstance().getExitCommand());

		// Sets the console echo command as false
		AcideConsoleConfiguration.getInstance().setEchoCommand(
				AcideConsoleConfiguration.getInstance().getIsEchoCommand());

		// Sets the console panel background color
		AcideConsoleConfiguration.getInstance().setBackgroundColor(
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getBackground());

		// Sets the console panel foreground color
		AcideConsoleConfiguration.getInstance().setForegroundColor(
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getForeground());

		// Sets the console panel font name
		AcideConsoleConfiguration.getInstance().setFontName(
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getFont().getFontName());

		// Sets the console panel font style
		AcideConsoleConfiguration.getInstance().setFontStyle(
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getFont().getStyle());

		// Sets the console panel font size
		AcideConsoleConfiguration.getInstance().setFontSize(
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getFont().getSize());
	}

	/**
	 * Gets the project name and if it exists ask to the user for saving the
	 * changes. If the user says YES, then we are overwriting the project.
	 * 
	 * @return true if the user said yes to the saving action if the project
	 *         already existed and false in other case.
	 */
	private boolean overwriteProject() {

		// Gets the project name to check if we are overwriting it
		String txtFile = "";
		String separator = "\\";

		int lastIndexOfSlash = _workspaceTextField.getText().lastIndexOf(
				separator);
		if (lastIndexOfSlash == -1)
			separator = "/";

		txtFile = _workspaceTextField.getText() + separator
				+ _nameTextField.getText();

		if (!txtFile.contains(".acidePrj"))
			txtFile = txtFile + ".acidePrj";

		File fileProject = new File(txtFile);

		// If the file exists
		if (fileProject.exists()) {

			// Ask to the user if he wants to save it
			int resultValueSaving = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s955"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_OPTION);

			// If NO
			if (resultValueSaving == JOptionPane.NO_OPTION)
				return false;
		}

		return true;
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

			// Shows a file chooser for only directories
			JFileChooser fileChooser = new JFileChooser();
			fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			int resultValue = fileChooser.showOpenDialog(null);

			// If OK
			if (resultValue == JFileChooser.APPROVE_OPTION)

				// Sets the workspace path
				_workspacePath = fileChooser.getSelectedFile()
						.getAbsolutePath();

			// Updates the workspace text field
			_workspaceTextField.setText(_workspacePath);

			// Validates the changes in the workspace text field
			_workspaceTextField.validate();

			// Repaints the workspace text field
			_workspaceTextField.repaint();
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

				// If we are overwriting
				if (overwriteProject()) {

					// Updates the console configuration
					updateConsoleConfiguration();

					// Updates the menu configuration
					updateMenuConfiguration();

					// Updates the tool bar configuration
					updateToolBarConfiguration();

					// Updates the project configuration
					updateProjectConfiguration();

					// Updates the explorer panel
					updateExplorerPanel();

					// Updates the file editor
					updateFileEditor();

					// Updates the main window configuration
					updateMainWindowConfiguration();
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

			// Enables the main window again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
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
