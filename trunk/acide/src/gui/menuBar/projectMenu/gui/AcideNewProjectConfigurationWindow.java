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
import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

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
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguageManager;
import operations.factory.AcideGUIFactory;
import operations.factory.AcideIOFactory;
import operations.lexicon.Remark;
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
	 * ACIDE - A Configurable IDE new project configuration window class serial version UID.
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
	 * ACIDE - A Configurable IDE new project configuration window language configuration panel.
	 */
	private JPanel _languageConfigurationPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window compiler panel.
	 */
	private JPanel _compilerPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window name text field.
	 */
	private JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace text field.
	 */
	private JTextField _workspaceTextField;
	/**
	 * ACIDE - A Configurable IDE new project configuration window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window lexicon configuration label.
	 */
	private JLabel _lexiconConfigurationNameLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window grammar configuration label.
	 */
	private JLabel _grammarConfigurationNameLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace label.
	 */
	private JLabel _workspaceLabel;
	/**
	 * ACIDE - A Configurable IDE new project configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace button.
	 */
	private JButton _workspaceButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window compiler button.
	 */
	private JButton _compilerButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window output button.
	 */
	private JButton _outputButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window create grammar button.
	 */
	private JButton _createGrammarButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window load grammar button.
	 */
	private JButton _loadGrammarButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window create lexicon button.
	 */
	private JButton _createLexiconButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window load lexicon button.
	 */
	private JButton _loadLexiconButton;
	/**
	 * ACIDE - A Configurable IDE new project configuration window lexicon configuration name string.
	 */
	private String _lexiconConfigurationName;
	/**
	 * ACIDE - A Configurable IDE new project configuration window grammar configuration name string
	 */
	private String _grammarConfigurationName;
	/**
	 * ACIDE - A Configurable IDE new project configuration window workspace path string
	 */
	private String _workspacePath;
	/**
	 * ACIDE - A Configurable IDE new project configuration window flag that indicates if the compiler paths are defined or not.
	 */
	private boolean _areCompilerPathsDefined;
	/**
	 * ACIDE - A Configurable IDE new project configuration window flag that indicates if the shell paths are defined or not.
	 */
	private boolean _areShellPathsDefined;

	/**
	 * Creates a new ACIDE - A Configurable IDE new project configuration window.
	 */
	public AcideNewProjectConfigurationWindow() {

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

		_areCompilerPathsDefined = false;
		_areShellPathsDefined = false;

		// DISABLE THE MAIN WINDOW
		MainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s587"));

		// Set the layout
		setLayout(new GridBagLayout());

		// GENERAL PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s589"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_mainPanel.setLayout(new GridBagLayout());

		// LANGUAGE CONFIGURATION PANEL
		_languageConfigurationPanel = new JPanel();
		_languageConfigurationPanel.setBorder(BorderFactory.createTitledBorder(
				null, labels.getString("s590"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_languageConfigurationPanel.setLayout(new GridBagLayout());

		// COMPILER PANEL
		_compilerPanel = new JPanel();
		_compilerPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s591"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_compilerPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// PROJECT NAME
		_nameLabel = new JLabel(labels.getString("s592"));
		_nameTextField = new JTextField();
		_nameTextField.setToolTipText(labels.getString("s593"));

		// LEXICON CONFIGURATION
		_lexiconConfigurationName = labels.getString("s598");
		_lexiconConfigurationNameLabel = new JLabel(labels.getString("s599")
				+ " " + _lexiconConfigurationName);

		// GRAMMAR CONFIGURATION
		_grammarConfigurationName = labels.getString("s598");
		_grammarConfigurationNameLabel = new JLabel(labels.getString("s642")
				+ " " + _grammarConfigurationName);

		// CREATE LEXICON BUTTON
		_createLexiconButton = new JButton(labels.getString("s600"));
		_createLexiconButton.setHorizontalAlignment(JButton.CENTER);
		_createLexiconButton.setToolTipText(labels.getString("s601"));

		// LOAD LEXICON BUTTON
		_loadLexiconButton = new JButton(labels.getString("s602"));
		_loadLexiconButton.setHorizontalAlignment(JButton.CENTER);
		_loadLexiconButton.setToolTipText(labels.getString("s603"));

		// CREATE GRAMMAR BUTTON
		_createGrammarButton = new JButton(labels.getString("s600"));
		_createGrammarButton.setHorizontalAlignment(JButton.CENTER);
		_createGrammarButton.setToolTipText(labels.getString("s601"));

		// LOAD GRAMMAR BUTTON
		_loadGrammarButton = new JButton(labels.getString("s602"));
		_loadGrammarButton.setHorizontalAlignment(JButton.CENTER);
		_loadGrammarButton.setToolTipText(labels.getString("s603"));

		// OUTPUT BUTTON
		_outputButton = new JButton(labels.getString("s637"));
		_outputButton.setHorizontalAlignment(JButton.CENTER);
		_outputButton.setToolTipText(labels.getString("s637"));

		// COMPILER BUTTON
		_compilerButton = new JButton(labels.getString("s636"));
		_compilerButton.setHorizontalAlignment(JButton.CENTER);
		_compilerButton.setToolTipText(labels.getString("s636"));

		// WORKSPACE
		_workspaceButton = new JButton(labels.getString("s948"));
		_workspaceButton.setHorizontalAlignment(JButton.RIGHT);
		_workspaceTextField = new JTextField("");
		_workspaceLabel = new JLabel(labels.getString("s949"));

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s154"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		_acceptButton.setToolTipText(labels.getString("s611"));

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(labels.getString("s612"));

		// Listeners

		// CREATE LEXICON BUTTON
		_createLexiconButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				TokenTypeList.getInstance().reset();
				DelimiterList.getInstance().reset();
				Remark.getInstance().reset();
				AcideGUIFactory.getInstance()
						.buildAcideNewLexiconConfigurationWindow();
			}
		});

		// LOAD LEXICON BUTTON
		_loadLexiconButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager.getInstance();
				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				// Loads the parameters
				MainWindow.getInstance().getMenu().getConfiguration()
						.getLexicon().getLoadParameters().doClick();

				// Gets the current project lexicon configuration
				_lexiconConfigurationName = AcideProjectConfiguration.getInstance().getLexiconConfiguration();

				// Gets the current lexicon configuration name
				String lexiconConfigurationName = AcideLexiconConfiguration
						.getInstance().getName();

				// Updates the lexicon configuration name label
				_lexiconConfigurationNameLabel.setText(labels.getString("s599")
						+ " " + lexiconConfigurationName);
			}
		});

		// CREATE GRAMMAR BUTTON
		_createGrammarButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager.getInstance();
				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				MainWindow.getInstance().getMenu().getConfiguration()
						.getGrammar().getNewGrammar().doClick();

				int index = AcideProjectConfiguration.getInstance()
						.getGrammarConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = AcideProjectConfiguration.getInstance()
							.getGrammarConfiguration().lastIndexOf("/");

				String grammarName = AcideProjectConfiguration.getInstance()
						.getGrammarConfiguration()
						.substring(
								index + 1,
								AcideProjectConfiguration.getInstance()
										.getGrammarConfiguration().length() - 4);
				_grammarConfigurationNameLabel.setText(labels.getString("s642")
						+ " " + grammarName);
				_grammarConfigurationNameLabel.validate();
				_grammarConfigurationNameLabel.repaint();
			}
		});

		// LOAD GRAMMAR BUTTON
		_loadGrammarButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager.getInstance();
				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				MainWindow.getInstance().getMenu().getConfiguration()
						.getGrammar().getLoadGrammar().doClick();
				int index = AcideProjectConfiguration.getInstance()
						.getGrammarConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = AcideProjectConfiguration.getInstance()
							.getGrammarConfiguration().lastIndexOf("/");
				String grammarName = AcideProjectConfiguration.getInstance()
						.getGrammarConfiguration()
						.substring(
								index + 1,
								AcideProjectConfiguration.getInstance()
										.getGrammarConfiguration().length() - 4);
				_grammarConfigurationNameLabel.setText(labels.getString("s642")
						+ " " + grammarName);
				_grammarConfigurationNameLabel.validate();
				_grammarConfigurationNameLabel.repaint();
			}
		});

		// WORKSPACE BUTTON
		_workspaceButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				JFileChooser fileChooser = new JFileChooser();
				fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int option = fileChooser.showOpenDialog(null);

				if (option == JFileChooser.APPROVE_OPTION)
					_workspacePath = fileChooser.getSelectedFile()
							.getAbsolutePath();

				_workspaceTextField.setText(_workspacePath);
				_workspaceTextField.validate();
				_workspaceTextField.repaint();
			}
		});

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager.getInstance();
				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				// If the required parameters are set
				if (!_nameTextField.getText().equals("")
						&& !_lexiconConfigurationNameLabel.getText().contains(
								labels.getString("s598"))) {
					// && !_grammarConfigurationNameLabel.getText().contains(
					// labels.getString("s598"))) {

					boolean overwrite = true;

					// Gets the project name to check if we are overwriting it
					String txtFile = "";
					String separator = "\\";

					int slashLastIndex = _workspaceTextField.getText()
							.lastIndexOf(separator);
					if (slashLastIndex == -1)
						separator = "/";

					txtFile = _workspaceTextField.getText() + separator
							+ _nameTextField.getText();

					if (!txtFile.contains(".acidePrj"))
						txtFile = txtFile + ".acidePrj";

					File fileProject = new File(txtFile);

					// If the file exists
					if (fileProject.exists()) {

						// Ask to the user if he wants to save it
						int choosenOption = JOptionPane.showConfirmDialog(null,
								labels.getString("s955"),
								labels.getString("s953"),
								JOptionPane.YES_NO_OPTION);

						// If he chooses no
						if (choosenOption == JOptionPane.NO_OPTION)
							// We are not overwriting
							overwrite = false;
					}

					// If we are overwriting
					if (overwrite) {

						// If the compiler paths are not defined
						if (!_areCompilerPathsDefined) {

							// Sets the compiler path in the configuration as
							// null
							AcideProjectConfiguration.getInstance()
									.setCompilerPath(null);

							// Sets the compiler arguments in the configuration
							// as null
							AcideProjectConfiguration.getInstance()
									.setCompilerArguments(null);
						} else {

							// Sets the compiler path in the configuration as
							// null
							if (AcideProjectConfiguration.getInstance()
									.getCompilerPath().equals(""))
								AcideProjectConfiguration.getInstance()
										.setCompilerPath(null);

							// Sets the compiler arguments in the configuration
							// as null
							if (AcideProjectConfiguration.getInstance()
									.getCompilerArguments().equals(""))
								AcideProjectConfiguration.getInstance()
										.setCompilerArguments(null);
						}

						// If the shell paths are not defined
						if (!_areShellPathsDefined) {
							
							// Sets the shell path in the configuration
							// as null
							AcideConsoleConfiguration.getInstance()
									.setShellPath(null);
							
							// Sets the shell directory in the configuration
							// as null
							AcideConsoleConfiguration.getInstance()
									.setShellDirectory(null);
						} else {

							if (AcideConsoleConfiguration.getInstance()
									.getShellPath().equals(""))
								// Sets the shell path in the configuration
								// as null
								AcideConsoleConfiguration.getInstance().setShellPath(
										null);

							if (AcideConsoleConfiguration.getInstance()
									.getShellDirectory().equals(""))
								// Sets the shell directory in the configuration
								// as null
								AcideConsoleConfiguration.getInstance()
										.setShellDirectory(null);
						}

						// CONSOLE EXIT COMMAND
						AcideConsoleConfiguration.getInstance().setExitCommand("null");

						// CONSOLE ECHO COMMAND
						AcideConsoleConfiguration.getInstance().setEchoCommand(false);

						// CONSOLE PANEL BACKGROUND COLOR
						AcideConsoleConfiguration.getInstance().setBackgroundColor(
								MainWindow.getInstance().getConsolePanel()
										.getTextPane().getBackground());

						// CONSOLE PANEL FOREGROUND COLOR
						AcideConsoleConfiguration.getInstance().setForegroundColor(
								MainWindow.getInstance().getConsolePanel()
										.getTextPane().getForeground());

						// CONSOLE PANEL FONT NAME
						AcideConsoleConfiguration.getInstance().setFontName(
								MainWindow.getInstance().getConsolePanel()
										.getTextPane().getFont()
										.getFontName());

						// Parse the font style from int to string
						String fontStyleString = "";
						switch (MainWindow.getInstance().getConsolePanel()
								.getTextPane().getFont().getStyle()) {
						case Font.PLAIN:
							fontStyleString = "Font.PLAIN";
							break;
						case Font.BOLD:
							fontStyleString = "Font.BOLD";
							break;
						case Font.ITALIC:
							fontStyleString = "Font.ITALIC";
							break;
						case Font.BOLD + Font.ITALIC:
							fontStyleString = "Font.BOLD + Font.ITALIC";
							break;
						}

						// CONSOLE PANEL FONT STYLE
						AcideConsoleConfiguration.getInstance().setFontStyle(
								fontStyleString);

						// CONSOLE PANEL FONT SIZE
						AcideConsoleConfiguration.getInstance()
								.setFontSize(
										MainWindow.getInstance().getConsolePanel()
												.getTextPane().getFont()
												.getSize());

						// Removes the previous files
						AcideProjectConfiguration.getInstance()
								.removeFiles();
						
						// Sets the project name
						AcideProjectConfiguration.getInstance()
								.setName(_nameTextField.getText());
						
						if (_lexiconConfigurationName.equals(labels
								.getString("s598"))) {
							try {
								_lexiconConfigurationName = AcideResourceManager
										.getInstance().getProperty(
												"languagePath");
							} catch (Exception exception) {

								// Updates the log
								AcideLog.getLog().error(exception.getMessage());
								exception.printStackTrace();
							}
						}

						// Sets the lexicon configuration
						AcideProjectConfiguration.getInstance()
								.setLexiconConfiguration(
										_lexiconConfigurationName);

						// Is not the first save
						AcideProjectConfiguration.getInstance()
								.setFirstSave(false);

						// Updates the log
						AcideLog.getLog().info(
								labels.getString("s615")
										+ _nameTextField.getText());

						// Builds the explorer
						MainWindow.getInstance().getExplorerPanel().getRoot()
								.removeAllChildren();

						AcideProjectFile projectFile = new AcideProjectFile();
						projectFile.setAbsolutePath(_nameTextField.getText());
						projectFile.setName(_nameTextField.getText());
						projectFile.setParent(null);
						projectFile.setIsDirectory(true);

						DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
								projectFile);
						defaultMutableTreeNode.setAllowsChildren(true);

						try {

							TextFile projectTextFile = AcideIOFactory
									.getInstance().buildFile();

							// Not default project
							if (!AcideProjectConfiguration.getInstance()
									.isDefaultProject()) {

								String[] askExtension = new String[] { "acidePrj" };
								projectTextFile
										.getFileChooser()
										.addChoosableFileFilter(
												new ExtensionFilter(
														askExtension,
														labels.getString("s328")));

								String file = "";
								separator = "\\";
								int index = _workspaceTextField.getText()
										.lastIndexOf(separator);
								if (index == -1)
									separator = "/";

								file = _workspaceTextField.getText()
										+ separator + _nameTextField.getText();

								// MENU CONFIGURATION
								String currentMenu = AcideResourceManager
										.getInstance().getProperty(
												"currentMenuConfiguration");
								AcideProjectConfiguration.getInstance()
										.setMenu(currentMenu);

								// TOOLBAR CONFIGURATION
								String currentToolBar = AcideResourceManager
										.getInstance().getProperty(
												"currentToolBarConfiguration");
								AcideProjectConfiguration.getInstance()
										.setToolBar(currentToolBar);

								if (!file.contains(".acidePrj"))
									file = file + ".acidePrj";

								AcideProjectConfiguration.getInstance()
										.setPath(file);
								String cad = AcideProjectConfiguration.getInstance().save();
								projectTextFile.save(AcideProjectConfiguration.getInstance()
										.getProjectPath(), cad);
								AcideProjectConfiguration.getInstance()
										.setFirstSave(true);

								// Updates the RESOURCE MANAGER
								AcideResourceManager.getInstance().setProperty(
										"defaultAcideProject", file);

								// Updates the RESOURCE MANAGER
								AcideResourceManager.getInstance().setProperty(
										"defaultPath", file);

								// The project configuration has not been modified yet
								AcideProjectConfiguration.getInstance()
										.setIsModified(false);
							}
						} catch (Exception exception) {

							// Updates the log
							AcideLog.getLog().error(exception.getMessage());
							exception.printStackTrace();
						}

						// Builds the EXPLORER
						MainWindow.getInstance().getExplorerPanel().getRoot()
								.add(defaultMutableTreeNode);
						MainWindow.getInstance().setTitle(
								labels.getString("s425") + " - "
										+ _nameTextField.getText());

						// Adds all the editors currently opened
						for (int i = 0; i < MainWindow.getInstance()
								.getFileEditorManager()
								.getNumFileEditorPanels(); i++) {

							// Except the NEW FILE and the LOG tab
							if (!MainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(i).isNewFile()
									&& !MainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(i)
											.isLogFile()) {

								projectFile = new AcideProjectFile();
								projectFile.setIsMainFile(MainWindow
										.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).isMainFile());
								projectFile.setIsCompilableFile(MainWindow
										.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i)
										.isCompilableFile());
								projectFile.setAbsolutePath(MainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(i)
										.getAbsolutePath());
								projectFile.setParent(defaultMutableTreeNode
										.toString());
								AcideProjectConfiguration.getInstance()
										.addFile(projectFile);
								String file = MainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(i)
										.getAbsolutePath();

								String fileName = "";
								int index = file.lastIndexOf("\\");
								if (index == -1)
									index = file.lastIndexOf("/");
								fileName = file.substring(index + 1,
										file.length());

								projectFile.setName(fileName);
								DefaultMutableTreeNode de = new DefaultMutableTreeNode(
										projectFile);
								de.setAllowsChildren(false);
								defaultMutableTreeNode.add(de);
							}
						}

						// Sets IS EXPLORER PANEL SHOWED flag
						AcideWindowConfiguration.getInstance()
								.setIsExplorerPanelShowed(true);

						// Sets IS CONSOLE PANEL SHOWED flag
						AcideWindowConfiguration.getInstance()
								.setIsConsolePanelShowed(true);

						// Sets WINDOW WIDTH
						AcideWindowConfiguration.getInstance()
								.setWindowWidth(
										MainWindow.getInstance().getWidth());
						
						// Sets WINDOW HEIGHT
						AcideWindowConfiguration.getInstance()
								.setWindowHeight(
										MainWindow.getInstance().getHeight());
						
						// Sets WINDOW X COORDINATE
						AcideWindowConfiguration.getInstance()
								.setXCoordinate(MainWindow.getInstance().getX());
						
						// Sets WINDOW Y COORDINATE
						AcideWindowConfiguration.getInstance()
								.setYCoordinate(MainWindow.getInstance().getY());
						
						// Sets VERTICAL SPLIT PANE DIVIDER LOCATION
						AcideWindowConfiguration.getInstance()
								.setVerticalSplitPaneDividerLocation(
										MainWindow.getInstance().getExplorerPanel()
												.getWidth());
						
						// Sets HORIZONTAL SPLIT PANE DIVIDER LOCATION
						AcideWindowConfiguration.getInstance()
								.setHorizontalSplitPaneDividerLocation(
										MainWindow.getInstance().getConsolePanel()
												.getHeight());

						// Updates the changes in the MAIN WINDOW
						MainWindow.getInstance().validate();
						
						// Repaints the MAIN WINDOW
						MainWindow.getInstance().repaint();
						
						// Enables the add file menu item in the explorer panel popup menu
						MainWindow.getInstance().getExplorerPanel().getPopupMenu()
								.getAddFile().setEnabled(true);
						
						// Enables the save project menu item in the explorer panel popup menu
						MainWindow.getInstance().getExplorerPanel().getPopupMenu()
								.getSaveProject().setEnabled(true);
						
						// Notifies to the model about the changes
						MainWindow.getInstance().getExplorerPanel().getTreeModel()
								.reload();
						
						// Expands the explorer tree
						MainWindow.getInstance().getExplorerPanel().expandTree();
						
						// Enables the MAIN WINDOW
						MainWindow.getInstance().setEnabled(true);

						// Closes the window
						dispose();

						// If the show explorer panel menu item is not selected
						if (!MainWindow.getInstance().getMenu().getView()
								.getShowExplorerPanel().isSelected())
							
							// Shows the explorer panel
							MainWindow.getInstance().getExplorerPanel()
									.showExplorerPanel();

						// Sets the show explorer panel check box menu item as selected
						MainWindow.getInstance().getMenu().getView()
								.getShowExplorerPanel().setSelected(true);
						
						// Sets the show console panel check box menu item as selected
						MainWindow.getInstance().getMenu().getView()
								.getShowConsolePanel().setSelected(true);

						// Enables the project menu
						MainWindow.getInstance().getMenu().enableProjectMenu();
						
						// Enables the open all files menu item
						MainWindow.getInstance().getMenu().getFile().getOpenAllFiles().setEnabled(true);		
					}
				} else {

					if (_nameTextField.getText().equals(""))
						// Shows a warning message
						JOptionPane.showMessageDialog(null,
								labels.getString("s973"),
								labels.getString("s972"),
								JOptionPane.WARNING_MESSAGE);

					if (_lexiconConfigurationNameLabel.getText().contains(
							labels.getString("s598")))
						// Shows a warning message
						JOptionPane.showMessageDialog(null,
								labels.getString("s974"),
								labels.getString("s972"),
								JOptionPane.WARNING_MESSAGE);

					if (_grammarConfigurationNameLabel.getText().contains(
							labels.getString("s598")))
						// Shows a warning message
						JOptionPane.showMessageDialog(null,
								labels.getString("s975"),
								labels.getString("s972"),
								JOptionPane.WARNING_MESSAGE);
				}
			}
		});

		// CANCEL BUTTON
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager.getInstance();
				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				// Updates the log
				AcideLog.getLog().info(labels.getString("s614"));

				// Enables the main window again
				MainWindow.getInstance().setEnabled(true);

				// Closes the window
				dispose();
			}
		});

		// OUTPUT BUTTON
		_outputButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				AcideGUIFactory.getInstance().buildAcideConsoleConfigurationWindow();
			}
		});

		// COMPILER BUTTON
		_compilerButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				AcideGUIFactory.getInstance()
						.buildAcideCompilerConfigurationWindow();
			}
		});

		// SET THE COMPONENTS WITH LAYOUT
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
		// _languageConfigurationPanel.add(_grammarConfigurationNameLabel,
		// constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		// _languageConfigurationPanel.add(_createGrammarButton, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 3;
		// _languageConfigurationPanel.add(_loadGrammarButton, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_languageConfigurationPanel, constraints);

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
		setTitle(labels.getString("s588"));
		setIconImage(ICON.getImage());
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s613"));

		addWindowListener(new AcideWindowListener());

		// TODO: As the grammar configuration does not work
		// we load a default file to configure it
		loadDefaultGrammar(labels);
	}

	/**
	 * Loads the default grammar.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	private void loadDefaultGrammar(ResourceBundle labels) {

		String grammarFile = "./configuration/grammars/bytes.jar";

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance()
				.setProperty("currentGrammar", grammarFile);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s243") + " " + grammarFile);

		// GET THE GRAMMAR NAME
		int index = grammarFile.lastIndexOf("\\");
		if (index == -1)
			index = grammarFile.lastIndexOf("/");
		String grammarName = grammarFile.substring(index + 1,
				grammarFile.length() - 4);

		// Updates the status bar
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.getStatusBar().setGrammarMessage(
				labels.getString("s248") + " " + grammarName);

		// UPDATES THE CONFIGURATION
		AcideProjectConfiguration.getInstance().setGrammarConfiguration(
				grammarFile);
		mainWindow.validate();
		mainWindow.repaint();

		// UPDATES THE MENU
		mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar()
				.setEnabled(false);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window lexicon configuration name label.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window lexicon configuration name label.
	 */
	public JLabel getLexiconConfigurationNameLabel() {
		return _lexiconConfigurationNameLabel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window lexicon configuration name label.
	 * 
	 * @param lexiconConfigurationLabel
	 *            new value to set.
	 */
	public void setLexiconConfigurationNameLabel(
			String lexiconConfigurationLabel) {
		_lexiconConfigurationNameLabel.setText(lexiconConfigurationLabel);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window lexicon configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window lexicon configuration name.
	 */
	public String getLexiconConfigurationName() {
		return _lexiconConfigurationName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window lexicon configuration name.
	 * 
	 * @param lexiconConfigurationName
	 *            new value to set.
	 */
	public void setLexiconConfigurationName(String lexiconConfigurationName) {
		_lexiconConfigurationName = lexiconConfigurationName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window accept button.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window accept button.
	 */
	public JButton getAcceptButton() {
		return _acceptButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window grammar configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window grammar configuration name.
	 */
	public String getGrammarConfigurationName() {
		return _grammarConfigurationName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window grammar configuration name.
	 * 
	 * @param grammarConfigurationName
	 *            new value to set.
	 */
	public void setGrammarConfigurationName(String grammarConfigurationName) {
		_grammarConfigurationName = grammarConfigurationName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window grammar configuration name label.
	 * 
	 * @return the grammar configuration name label.
	 */
	public JLabel getGrammarConfigurationNameLabel() {
		return _grammarConfigurationNameLabel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window grammar configuration name label.
	 * 
	 * @param grammarConfigurationNameLabel
	 *            new value to set.
	 */
	public void setNombreConfLabelGram(JLabel grammarConfigurationNameLabel) {
		_grammarConfigurationNameLabel = grammarConfigurationNameLabel;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE new project configuration window workspace path.
	 * 
	 * @return the ACIDE - A Configurable IDE new project configuration window workspace path.
	 */
	public String getWorkspacePath() {
		return _workspacePath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window workspace path.
	 * 
	 * @param workspacePath
	 *            new value to set.
	 */
	public void setWorkspacePath(String workspacePath) {
		_workspacePath = workspacePath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window are shell paths defined flag.
	 * 
	 * @param areShellPathsDefined
	 *            new value to set.
	 */
	public void setAreShellPathsDefined(boolean areShellPathsDefined) {
		_areShellPathsDefined = areShellPathsDefined;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE new project configuration window are compiler paths defined flag.
	 * 
	 * @param areCompilerPathsDefined
	 *            new value to set.
	 */
	public void setAreCompilerPathsDefined(boolean areCompilerPathsDefined) {
		_areCompilerPathsDefined = areCompilerPathsDefined;
	}
}
