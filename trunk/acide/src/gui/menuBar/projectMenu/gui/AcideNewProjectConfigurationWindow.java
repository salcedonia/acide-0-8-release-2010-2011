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
package gui.menuBar.projectMenu.gui;

import es.configuration.console.AcideConsoleConfiguration;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.configuration.window.AcideWindowConfiguration;
import es.project.AcideProjectFile;
import es.text.AcideFileExtensionFilterManager;
import es.text.AcideFileManager;
import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;
import gui.toolBarPanel.staticToolBar.AcideStaticToolBar;

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
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguageManager;
import operations.factory.AcideGUIFactory;
import operations.lexicon.Remarks;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenTypeList;
import operations.log.AcideLog;
import resources.AcideResourceManager;

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
	 * ACIDE - A Configurable IDE new project configuration window lexicon file
	 * by default.
	 */
	private static final String DEFAULT_LEXICON_FILE = "./configuration/lexicon/default.xml";
	/**
	 * ACIDE - A Configurable IDE new project configuration window grammar file
	 * by default.
	 */
	private static final String DEFAULT_GRAMMAR_FILE = "./configuration/grammars/bytes.jar";
	/**
	 * ACIDE - A Configurable IDE new project configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window language
	 * configuration panel.
	 */
	private JPanel _languageConfigurationPanel;
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
	 * ACIDE - A Configurable IDE new project configuration window lexicon
	 * configuration label.
	 */
	private JLabel _lexiconConfigurationNameLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window grammar
	 * configuration label.
	 */
	private JLabel _grammarConfigurationNameLabel;
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
	 * ACIDE - A Configurable IDE new project configuration window create
	 * grammar button.
	 */
	private JButton _createGrammarButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window load grammar
	 * button.
	 */
	private JButton _loadGrammarButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window create
	 * lexicon button.
	 */
	private JButton _createLexiconButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window load lexicon
	 * button.
	 */
	private JButton _loadLexiconButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window lexicon
	 * configuration name string.
	 */
	private String _lexiconConfigurationName;
	/**
	 * ACIDE - A Configurable IDE new project configuration window grammar
	 * configuration name string
	 */
	private String _grammarConfigurationName;
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

		_areCompilerPathsDefined = false;
		_areShellPathsDefined = false;

		// DISABLE THE MAIN WINDOW
		MainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s587"));

		// Set the layout
		setLayout(new GridBagLayout());

		// GENERAL PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s589"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_mainPanel.setLayout(new GridBagLayout());

		// LANGUAGE CONFIGURATION PANEL
		_languageConfigurationPanel = new JPanel();
		_languageConfigurationPanel.setBorder(BorderFactory.createTitledBorder(
				null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s590"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_languageConfigurationPanel.setLayout(new GridBagLayout());

		// COMPILER PANEL
		_compilerPanel = new JPanel();
		_compilerPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s591"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_compilerPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// PROJECT NAME
		_nameLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s592"));
		_nameTextField = new JTextField();
		_nameTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s593"));

		// LEXICON CONFIGURATION
		_lexiconConfigurationName = AcideLanguageManager.getInstance()
				.getLabels().getString("s598");
		_lexiconConfigurationNameLabel = new JLabel(AcideLanguageManager
				.getInstance().getLabels().getString("s599")
				+ " " + _lexiconConfigurationName);

		// GRAMMAR CONFIGURATION
		_grammarConfigurationName = AcideLanguageManager.getInstance()
				.getLabels().getString("s598");
		_grammarConfigurationNameLabel = new JLabel(AcideLanguageManager
				.getInstance().getLabels().getString("s642")
				+ " " + _grammarConfigurationName);

		// CREATE LEXICON BUTTON
		_createLexiconButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s600"));
		_createLexiconButton.setHorizontalAlignment(JButton.CENTER);
		_createLexiconButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s601"));

		// LOAD LEXICON BUTTON
		_loadLexiconButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s602"));
		_loadLexiconButton.setHorizontalAlignment(JButton.CENTER);
		_loadLexiconButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s603"));

		// CREATE GRAMMAR BUTTON
		_createGrammarButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s600"));
		_createGrammarButton.setHorizontalAlignment(JButton.CENTER);
		_createGrammarButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s601"));

		// LOAD GRAMMAR BUTTON
		_loadGrammarButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s602"));
		_loadGrammarButton.setHorizontalAlignment(JButton.CENTER);
		_loadGrammarButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s603"));

		// OUTPUT BUTTON
		_outputButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s637"));
		_outputButton.setHorizontalAlignment(JButton.CENTER);
		_outputButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s637"));

		// COMPILER BUTTON
		_compilerButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s636"));
		_compilerButton.setHorizontalAlignment(JButton.CENTER);
		_compilerButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s636"));

		// WORKSPACE
		_workspaceButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s948"));
		_workspaceButton.setHorizontalAlignment(JButton.RIGHT);
		_workspaceTextField = new JTextField("");
		_workspaceLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s949"));

		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s611"));

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s612"));

		// Sets the listener for the window components
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// MAIN PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipady = 10;
		_mainPanel.add(_nameLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 250;
		constraints.ipady = 0;
		_mainPanel.add(_nameTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_mainPanel.add(_workspaceLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_mainPanel.add(_workspaceTextField, constraints);
		constraints.gridx = 2;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_mainPanel.add(_workspaceButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_mainPanel, constraints);

		// LEXICON CONFIGURATION PANEL
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_lexiconConfigurationNameLabel,
				constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_languageConfigurationPanel.add(_createLexiconButton, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_loadLexiconButton, constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_grammarConfigurationNameLabel,
				constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		_languageConfigurationPanel.add(_createGrammarButton, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 3;
		_languageConfigurationPanel.add(_loadGrammarButton, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 1;
		// add(_languageConfigurationPanel, constraints);

		// COMPILER PANEL
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_compilerPanel.add(_compilerButton, constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 1;
		constraints.gridy = 0;
		_compilerPanel.add(_outputButton, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_compilerPanel, constraints);
		add(new JSeparator());

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 3;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s588"));
		setIconImage(ICON.getImage());
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s613"));

		// Adds the window listener
		addWindowListener(new AcideWindowListener());
	}

	/**
	 * Sets the listeners for the window components.
	 */
	public void setListeners() {

		// CREATE LEXICON BUTTON
		_createLexiconButton.addActionListener(new CreateLexiconButtonAction());

		// LOAD LEXICON BUTTON
		_loadLexiconButton.addActionListener(new LoadLexiconButtonAction());

		// CREATE GRAMMAR BUTTON
		_createGrammarButton.addActionListener(new CreateGrammarButtonAction());

		// LOAD GRAMMAR BUTTON
		_loadGrammarButton.addActionListener(new LoadGrammarButtonAction());

		// WORKSPACE BUTTON
		_workspaceButton.addActionListener(new WorkspaceButtonButtonAction());

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonButtonAction());

		// OUTPUT BUTTON
		_outputButton.addActionListener(new OutputButtonButtonAction());

		// COMPILER BUTTON
		_compilerButton.addActionListener(new CompilerButtonButtonAction());
	}

	/**
	 * Updates the explorer panel.
	 */
	public void updateExplorerPanel() {

		// Removes the previous files
		AcideProjectConfiguration.getInstance().removeFiles();

		// Removes all the children in the explorer tree
		MainWindow.getInstance().getExplorerPanel().getRoot()
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
		MainWindow.getInstance().getExplorerPanel().getRoot().add(rootNode);

		// Adds all the editors currently opened
		for (int index = 0; index < MainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			// Except the NEW FILE and the LOG tab
			if (!MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).isNewFile()
					&& !MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).isLogFile()) {

				// Creates the new project file
				AcideProjectFile newProjectFile = new AcideProjectFile();

				// Sets its is main file
				newProjectFile.setIsMainFile(MainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.isMainFile());

				// Sets its is compilable file
				newProjectFile.setIsCompilableFile(MainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.isCompilableFile());

				// Sets its absolute path
				newProjectFile.setAbsolutePath(MainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.getAbsolutePath());

				// Sets its parent, in this case the root node
				newProjectFile.setParent(rootNode.toString());

				// Adds the file to the project configuration
				AcideProjectConfiguration.getInstance().addFile(newProjectFile);

				// Gets the file editor panel absolute path
				String fileAbsolutePath = MainWindow.getInstance()
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

		/*
		 * IMPORTANT: If the file editors are not closed in the procedure, the
		 * UndoManager and the document listener do not work anymore
		 */

		// Adds all the editors currently opened
		for (int index = 0; index < MainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++)
			// Close the tab
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.removeTabAt(index);

		// Open all the files once again in the file editor
		MainWindow.getInstance().getMenu().getFile().getOpenAllFiles()
				.doClick();
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

				// Updates the RESOURCE MANAGER default ACIDE - A Configurable
				// IDE project
				AcideResourceManager.getInstance().setProperty(
						"defaultAcideProject", fileName);

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
			// Gets the current tool bar configuration
			String currentToolBarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");

			// Sets the tool bar configuration
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
			// Gets the current menu configuration
			String currentMenuConfiguration = AcideResourceManager
					.getInstance().getProperty("currentMenuConfiguration");

			// Sets the menu configuration
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
		MainWindow.getInstance().setTitle(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s425")
						+ " - " + _nameTextField.getText());

		// Sets the is explorer panel showed flag as true
		AcideWindowConfiguration.getInstance().setIsExplorerPanelShowed(true);

		// Sets the is console panel showed flag as true
		AcideWindowConfiguration.getInstance().setIsConsolePanelShowed(true);

		// Sets the window width
		AcideWindowConfiguration.getInstance().setWindowWidth(
				MainWindow.getInstance().getWidth());

		// Sets the window height
		AcideWindowConfiguration.getInstance().setWindowHeight(
				MainWindow.getInstance().getHeight());

		// Sets the window x coordinate
		AcideWindowConfiguration.getInstance().setXCoordinate(
				MainWindow.getInstance().getX());

		// Sets the window y coordinate
		AcideWindowConfiguration.getInstance().setYCoordinate(
				MainWindow.getInstance().getY());

		// Sets the vertical split pane divider location
		AcideWindowConfiguration.getInstance()
				.setVerticalSplitPaneDividerLocation(
						MainWindow.getInstance().getExplorerPanel().getWidth());

		// Sets the horizontal split pane divider location
		AcideWindowConfiguration.getInstance()
				.setHorizontalSplitPaneDividerLocation(
						MainWindow.getInstance().getConsolePanel().getHeight());

		// Validates the changes in the main window
		MainWindow.getInstance().validate();

		// Repaints the main window
		MainWindow.getInstance().repaint();

		// Enables the add file menu item in the explorer panel
		// popup menu
		MainWindow.getInstance().getExplorerPanel().getPopupMenu().getAddFile()
				.setEnabled(true);

		// Enables the save project menu item in the explorer
		// panel popup menu
		MainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getSaveProject().setEnabled(true);

		// Enables the save project button in the static tool
		// bar
		AcideStaticToolBar.getInstance().getSaveProjectButton()
				.setEnabled(true);

		// Notifies to the model about the changes
		MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

		// Expands the explorer tree
		MainWindow.getInstance().getExplorerPanel().expandTree();

		// Enables the main window
		MainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// If the show explorer panel menu item is not selected
		if (!MainWindow.getInstance().getMenu().getView()
				.getShowExplorerPanel().isSelected())

			// Shows the explorer panel
			MainWindow.getInstance().getExplorerPanel().showExplorerPanel();

		// Sets the show explorer panel check box menu item as
		// selected
		MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
				.setSelected(true);

		// Sets the show console panel check box menu item as
		// selected
		MainWindow.getInstance().getMenu().getView().getShowConsolePanel()
				.setSelected(true);

		// Enables the project menu
		MainWindow.getInstance().getMenu().enableProjectMenu();

		// Enables the open all files menu item
		MainWindow.getInstance().getMenu().getFile().getOpenAllFiles()
				.setEnabled(true);
	}

	/**
	 * Updates the lexicon configuration.
	 */
	public void updateLexiconConfiguration() {

		// Sets the lexicon configuration
		AcideProjectConfiguration.getInstance().setLexiconConfiguration(
				DEFAULT_LEXICON_FILE);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s615")
						+ _nameTextField.getText());
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
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.getBackground());

		// Sets the console panel foreground color
		AcideConsoleConfiguration.getInstance().setForegroundColor(
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.getForeground());

		// Sets the console panel font name
		AcideConsoleConfiguration.getInstance().setFontName(
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.getFont().getFontName());

		// Sets the console panel font style
		AcideConsoleConfiguration.getInstance().setFontStyle(
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.getFont().getStyle());

		// Sets the console panel font size
		AcideConsoleConfiguration.getInstance().setFontSize(
				MainWindow.getInstance().getConsolePanel().getTextPane()
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
	 * Updates the grammar configuration with a default grammar file.
	 */
	private void updateGrammarConfiguration() {

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty("currentGrammar",
				DEFAULT_GRAMMAR_FILE);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s243")
						+ " " + DEFAULT_GRAMMAR_FILE);

		// Gets the last index of the slash
		int lastIndexOfSlash = DEFAULT_GRAMMAR_FILE.lastIndexOf("\\");
		if (lastIndexOfSlash == -1)
			lastIndexOfSlash = DEFAULT_GRAMMAR_FILE.lastIndexOf("/");

		// Gets the grammar name
		String grammarName = DEFAULT_GRAMMAR_FILE.substring(
				lastIndexOfSlash + 1, DEFAULT_GRAMMAR_FILE.length() - 4);

		// Updates the grammar message in the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setGrammarMessage(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s248")
								+ " " + grammarName);

		// Updates the grammar configuration
		AcideProjectConfiguration.getInstance().setGrammarConfiguration(
				DEFAULT_GRAMMAR_FILE);

		// Validates the changes in the main window
		MainWindow.getInstance().validate();

		// Repaint the main window
		MainWindow.getInstance().repaint();

		// Disables the save grammar menu option
		MainWindow.getInstance().getMenu().getConfiguration().getGrammar()
				.getSaveGrammar().setEnabled(false);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * lexicon configuration name label.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         lexicon configuration name label.
	 */
	public JLabel getLexiconConfigurationNameLabel() {
		return _lexiconConfigurationNameLabel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window lexicon configuration name label.
	 * 
	 * @param lexiconConfigurationLabel
	 *            new value to set.
	 */
	public void setLexiconConfigurationNameLabel(
			String lexiconConfigurationLabel) {
		_lexiconConfigurationNameLabel.setText(lexiconConfigurationLabel);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * lexicon configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         lexicon configuration name.
	 */
	public String getLexiconConfigurationName() {
		return _lexiconConfigurationName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window lexicon configuration name.
	 * 
	 * @param lexiconConfigurationName
	 *            new value to set.
	 */
	public void setLexiconConfigurationName(String lexiconConfigurationName) {
		_lexiconConfigurationName = lexiconConfigurationName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * accept button.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         accept button.
	 */
	public JButton getAcceptButton() {
		return _acceptButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * grammar configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window
	 *         grammar configuration name.
	 */
	public String getGrammarConfigurationName() {
		return _grammarConfigurationName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window grammar configuration name.
	 * 
	 * @param grammarConfigurationName
	 *            new value to set.
	 */
	public void setGrammarConfigurationName(String grammarConfigurationName) {
		_grammarConfigurationName = grammarConfigurationName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window
	 * grammar configuration name label.
	 * 
	 * @return the grammar configuration name label.
	 */
	public JLabel getGrammarConfigurationNameLabel() {
		return _grammarConfigurationNameLabel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project
	 * configuration window grammar configuration name label.
	 * 
	 * @param grammarConfigurationNameLabel
	 *            new value to set.
	 */
	public void setNombreConfLabelGram(JLabel grammarConfigurationNameLabel) {
		_grammarConfigurationNameLabel = grammarConfigurationNameLabel;
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
	 * Create lexicon button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CreateLexiconButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Resets the token type list
			TokenTypeList.getInstance().reset();

			// Resets the delimiters list
			DelimiterList.getInstance().reset();

			// Resets the remarks
			Remarks.getInstance().reset();

			// Builds the new lexicon configuration window
			AcideGUIFactory.getInstance()
					.buildAcideNewLexiconConfigurationWindow();
		}
	}

	/**
	 * Load lexicon button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class LoadLexiconButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Loads the parameters
			MainWindow.getInstance().getMenu().getConfiguration().getLexicon()
					.getLoadParameters().doClick();

			// Gets the current project lexicon configuration
			_lexiconConfigurationName = AcideProjectConfiguration.getInstance()
					.getLexiconConfiguration();

			// Gets the current lexicon configuration name
			String lexiconConfigurationName = AcideLexiconConfiguration
					.getInstance().getName();

			// Updates the lexicon configuration name label
			_lexiconConfigurationNameLabel.setText(AcideLanguageManager
					.getInstance().getLabels().getString("s599")
					+ " " + lexiconConfigurationName);
		}
	}

	/**
	 * Create grammar button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CreateGrammarButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Creates the new grammar
			MainWindow.getInstance().getMenu().getConfiguration().getGrammar()
					.getNewGrammar().doClick();

			// Gets the last index of the slash
			int lastIndexOfSlash = AcideProjectConfiguration.getInstance()
					.getGrammarConfiguration().lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = AcideProjectConfiguration.getInstance()
						.getGrammarConfiguration().lastIndexOf("/");

			// Gets the grammar name
			String grammarName = AcideProjectConfiguration
					.getInstance()
					.getGrammarConfiguration()
					.substring(
							lastIndexOfSlash + 1,
							AcideProjectConfiguration.getInstance()
									.getGrammarConfiguration().length() - 4);

			// Updates the grammar configuration name label
			_grammarConfigurationNameLabel.setText(AcideLanguageManager
					.getInstance().getLabels().getString("s642")
					+ " " + grammarName);

			// Validates the changes in the grammar configuration name label
			_grammarConfigurationNameLabel.validate();

			// Repaint the grammar configuration name label
			_grammarConfigurationNameLabel.repaint();
		}
	}

	/**
	 * Load grammar button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class LoadGrammarButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Loads the grammar
			MainWindow.getInstance().getMenu().getConfiguration().getGrammar()
					.getLoadGrammar().doClick();

			// Gets the last index of the slash
			int lastIndexOfSlash = AcideProjectConfiguration.getInstance()
					.getGrammarConfiguration().lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = AcideProjectConfiguration.getInstance()
						.getGrammarConfiguration().lastIndexOf("/");

			// Gets the grammar name
			String grammarName = AcideProjectConfiguration
					.getInstance()
					.getGrammarConfiguration()
					.substring(
							lastIndexOfSlash + 1,
							AcideProjectConfiguration.getInstance()
									.getGrammarConfiguration().length() - 4);

			// Updates the grammar configuration name label
			_grammarConfigurationNameLabel.setText(AcideLanguageManager
					.getInstance().getLabels().getString("s642")
					+ " " + grammarName);

			// Validates the changes in the grammar configuration name label
			_grammarConfigurationNameLabel.validate();

			// Repaints the grammar configuration name label
			_grammarConfigurationNameLabel.repaint();
		}
	}

	/**
	 * Workspace button action listener.
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
	 * Accept button action listener.
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
				// && !_lexiconConfigurationNameLabel.getText().contains(
				// AcideLanguageManager.getInstance().getLabels()
				// .getString("s598"))) {
				// && !_grammarConfigurationNameLabel.getText().contains(
				// labels.getString("s598"))) {

				// If we are overwriting
				if (overwriteProject()) {

					SwingUtilities.invokeLater(new Runnable() {

						/*
						 * (non-Javadoc)
						 * 
						 * @see java.lang.Runnable#run()
						 */
						@Override
						public void run() {

							// Updates the console configuration
							updateConsoleConfiguration();

							// Updates the lexicon configuration
							updateLexiconConfiguration();

							// Updates the grammar configuration
							updateGrammarConfiguration();

							// Updates the menu configuration
							updateMenuConfiguration();

							// Updates the tool bar configuration
							updateToolBarConfiguration();

							// Updates the project configuration
							updateProjectConfiguration();

							// Updates the explorer panel
							updateExplorerPanel();

							// Updates the main window configuration
							updateMainWindowConfiguration();
						}
					});
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

				// If the lexicon configuration name is empty
				/*
				 * if (_lexiconConfigurationNameLabel.getText().contains(
				 * AcideLanguageManager.getInstance().getLabels()
				 * .getString("s598"))) // Shows a warning message
				 * JOptionPane.showMessageDialog(null,
				 * AcideLanguageManager.getInstance().getLabels()
				 * .getString("s974"),
				 * AcideLanguageManager.getInstance().getLabels()
				 * .getString("s972"), JOptionPane.WARNING_MESSAGE);
				 */
				/*
				 * if (_grammarConfigurationNameLabel.getText().contains(
				 * labels.getString("s598"))) // Shows a warning message
				 * JOptionPane.showMessageDialog(null, labels.getString("s975"),
				 * labels.getString("s972"), JOptionPane.WARNING_MESSAGE);
				 */
			}
		}
	}

	/**
	 * Cancel button action listener.
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
			MainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
		}
	}

	/**
	 * Output button action listener.
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
	 * Compiler button action listener.
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
