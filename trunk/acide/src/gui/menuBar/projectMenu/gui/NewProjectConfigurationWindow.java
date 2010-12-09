package gui.menuBar.projectMenu.gui;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.output.OutputConfiguration;
import es.explorer.ExplorerFile;
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

import language.AcideLanguage;
import operations.factory.AcideGUIFactory;
import operations.factory.AcideIOFactory;
import operations.lexicon.Comments;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenTypeList;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * New project configuration window of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan Jos� Ortiz S�nchez
 *         </ul>
 *         <ul>
 *         Delf�n Rup�rez Ca�as
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Mart�n L�zaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo G�mez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see JFrame
 ***********************************************************************/
public class NewProjectConfigurationWindow extends JFrame {

	/**
	 * New project configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New project configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * New project configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * New project configuration window language configuration panel.
	 */
	private JPanel _languageConfigurationPanel;
	/**
	 * New project configuration window compiler panel.
	 */
	private JPanel _compilerPanel;
	/**
	 * New project configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * New project configuration window name text field.
	 */
	private JTextField _nameTextField;
	/**
	 * New project configuration window workspace text field.
	 */
	private JTextField _workspaceTextField;
	/**
	 * New project configuration window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * New project configuration window lexical configuration label.
	 */
	private JLabel _lexicalConfigurationNameLabel;
	/**
	 * New project configuration window grammar configuration label.
	 */
	private JLabel _grammarConfigurationNameLabel;
	/**
	 * New project configuration window workspace label.
	 */
	private JLabel _workspaceLabel;
	/**
	 * New project configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * New project configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * New project configuration window workspace button.
	 */
	private JButton _workspaceButton;
	/**
	 * New project configuration window compiler button.
	 */
	private JButton _compilerButton;
	/**
	 * New project configuration window output button.
	 */
	private JButton _outputButton;
	/**
	 * New project configuration window create grammar button.
	 */
	private JButton _createGrammarButton;
	/**
	 * New project configuration window load grammar button.
	 */
	private JButton _loadGrammarButton;
	/**
	 * New project configuration window create lexical button.
	 */
	private JButton _createLexicalButton;
	/**
	 * New project configuration window load lexical button.
	 */
	private JButton _loadLexicalButton;
	/**
	 * Lexical configuration name string.
	 */
	private String _lexicalConfigurationName;
	/**
	 * Grammar configuration name string
	 */
	private String _grammarConfigurationName;
	/**
	 * Workspace path string
	 */
	private String _workspacePath;
	/**
	 * Flag that indicates if the compiler paths are defined or not.
	 */
	private boolean _areCompilerPathsDefined;
	/**
	 * Flag that indicates if the shell paths are defined or not.
	 */
	private boolean _areShellPathsDefined;

	/**
	 * Creates a new new project configuration window.
	 */
	public NewProjectConfigurationWindow() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
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

		// LEXICAL CONFIGURATION
		_lexicalConfigurationName = labels.getString("s598");
		_lexicalConfigurationNameLabel = new JLabel(labels.getString("s599")
				+ " " + _lexicalConfigurationName);

		// GRAMMAR CONFIGURATION
		_grammarConfigurationName = labels.getString("s598");
		_grammarConfigurationNameLabel = new JLabel(labels.getString("s642")
				+ " " + _grammarConfigurationName);

		// CREATE LEXICAL BUTTON
		_createLexicalButton = new JButton(labels.getString("s600"));
		_createLexicalButton.setHorizontalAlignment(JButton.CENTER);
		_createLexicalButton.setToolTipText(labels.getString("s601"));

		// LOAD LEXICAL BUTTON
		_loadLexicalButton = new JButton(labels.getString("s602"));
		_loadLexicalButton.setHorizontalAlignment(JButton.CENTER);
		_loadLexicalButton.setToolTipText(labels.getString("s603"));

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

		// CREATE LEXICAL BUTTON
		_createLexicalButton.addActionListener(new ActionListener() {
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
				Comments.getInstance().reset();
				AcideGUIFactory.getInstance()
						.buildNewLexiconConfigurationWindow();
			}
		});

		// LOAD LEXICAL BUTTON
		_loadLexicalButton.addActionListener(new ActionListener() {
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
				AcideLanguage language = AcideLanguage.getInstance();
				try {
					language.getLanguage(ResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				// LOAD THE PARAMETERS
				MainWindow.getInstance().getMenu().getConfiguration()
						.getLexicon().getLoadParameters().doClick();

				// GET THE CURRENT LEXICAL CONFIGURATION OF THE PROJECT
				_lexicalConfigurationName = MainWindow.getInstance()
						.getProjectConfiguration().getLexicalConfiguration();

				// GET THE NAME OF THE CURRENT LEXICAL CONFIGURATION
				String lexicalConfigurationName = LexiconConfiguration
						.getInstance().getName();

				_lexicalConfigurationNameLabel.setText(labels.getString("s599")
						+ " " + lexicalConfigurationName);
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
				AcideLanguage language = AcideLanguage.getInstance();
				try {
					language.getLanguage(ResourceManager.getInstance()
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

				int index = MainWindow.getInstance().getProjectConfiguration()
						.getSyntacticConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = MainWindow.getInstance().getProjectConfiguration()
							.getSyntacticConfiguration().lastIndexOf("/");

				String grammarName = MainWindow
						.getInstance()
						.getProjectConfiguration()
						.getSyntacticConfiguration()
						.substring(
								index + 1,
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getSyntacticConfiguration().length() - 4);
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
				AcideLanguage language = AcideLanguage.getInstance();
				try {
					language.getLanguage(ResourceManager.getInstance()
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
				int index = MainWindow.getInstance().getProjectConfiguration()
						.getSyntacticConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = MainWindow.getInstance().getProjectConfiguration()
							.getSyntacticConfiguration().lastIndexOf("/");
				String grammarName = MainWindow
						.getInstance()
						.getProjectConfiguration()
						.getSyntacticConfiguration()
						.substring(
								index + 1,
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getSyntacticConfiguration().length() - 4);
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
				AcideLanguage language = AcideLanguage.getInstance();
				try {
					language.getLanguage(ResourceManager.getInstance()
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
						&& !_lexicalConfigurationNameLabel.getText().contains(
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
							MainWindow.getInstance().getProjectConfiguration()
									.setCompilerPath(null);

							// Sets the compiler arguments in the configuration
							// as null
							MainWindow.getInstance().getProjectConfiguration()
									.setCompilerArguments(null);
						} else {

							// Sets the compiler path in the configuration as
							// null
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getCompilerPath().equals(""))
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setCompilerPath(null);

							// Sets the compiler arguments in the configuration
							// as null
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getCompilerArguments().equals(""))
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setCompilerArguments(null);
						}

						// If the shell paths are not defined
						if (!_areShellPathsDefined) {
							
							// Sets the shell path in the configuration
							// as null
							OutputConfiguration.getInstance()
									.setShellPath(null);
							
							// Sets the shell directory in the configuration
							// as null
							OutputConfiguration.getInstance()
									.setShellDirectory(null);
						} else {

							if (OutputConfiguration.getInstance()
									.getShellPath().equals(""))
								// Sets the shell path in the configuration
								// as null
								OutputConfiguration.getInstance().setShellPath(
										null);

							if (OutputConfiguration.getInstance()
									.getShellDirectory().equals(""))
								// Sets the shell directory in the configuration
								// as null
								OutputConfiguration.getInstance()
										.setShellDirectory(null);
						}

						// SHELL EXIT COMMAND
						OutputConfiguration.getInstance().setExitCommand("null");

						// SHELL ECHO COMMAND
						OutputConfiguration.getInstance().setEchoCommand(false);

						// SHELL BACKGROUND COLOR
						OutputConfiguration.getInstance().setBackgroundColor(
								MainWindow.getInstance().getOutput()
										.getTextComponent().getBackground());

						// SHELL FOREGROUND COLOR
						OutputConfiguration.getInstance().setForegroundColor(
								MainWindow.getInstance().getOutput()
										.getTextComponent().getForeground());

						// SHELL FONT NAME
						OutputConfiguration.getInstance().setFontName(
								MainWindow.getInstance().getOutput()
										.getTextComponent().getFont()
										.getFontName());

						// Parse the font style from int to string
						String fontStyleString = "";
						switch (MainWindow.getInstance().getOutput()
								.getTextComponent().getFont().getStyle()) {
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

						// SHELL FONT STYLE
						OutputConfiguration.getInstance().setFontStyle(
								fontStyleString);

						// SHELL FONT SIZE
						OutputConfiguration.getInstance()
								.setFontSize(
										MainWindow.getInstance().getOutput()
												.getTextComponent().getFont()
												.getSize());

						// LEXICAL CONFIGURATION
						MainWindow.getInstance().getProjectConfiguration()
								.removeFiles();
						MainWindow.getInstance().getProjectConfiguration()
								.setName(_nameTextField.getText());
						if (_lexicalConfigurationName.equals(labels
								.getString("s598"))) {
							try {
								_lexicalConfigurationName = ResourceManager
										.getInstance().getProperty(
												"languagePath");
							} catch (Exception exception) {

								// Updates the log
								AcideLog.getLog().error(exception.getMessage());
								exception.printStackTrace();
							}
						}

						// Sets the project configuration
						MainWindow
								.getInstance()
								.getProjectConfiguration()
								.setLexicalConfiguration(
										_lexicalConfigurationName);

						// Is not the first save
						MainWindow.getInstance().getProjectConfiguration()
								.setFirstSave(false);

						// Updates the log
						AcideLog.getLog().info(
								labels.getString("s615")
										+ _nameTextField.getText());

						// Builds the explorer
						MainWindow.getInstance().getExplorer().getRoot()
								.removeAllChildren();

						ExplorerFile explorerFile = new ExplorerFile();
						explorerFile.setPath(_nameTextField.getText());
						explorerFile.setName(_nameTextField.getText());
						explorerFile.setParent(null);
						explorerFile.setIsDirectory(true);

						DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
								explorerFile);
						defaultMutableTreeNode.setAllowsChildren(true);

						try {

							TextFile projectTextFile = AcideIOFactory
									.getInstance().buildFile();

							// Not default project
							if (!MainWindow.getInstance()
									.getProjectConfiguration()
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
								String currentMenu = ResourceManager
										.getInstance().getProperty(
												"currentMenuConfiguration");
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setMenu(currentMenu);

								// TOOLBAR CONFIGURATION
								String currentToolBar = ResourceManager
										.getInstance().getProperty(
												"currentToolBarConfiguration");
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setToolBar(currentToolBar);

								if (!file.contains(".acidePrj"))
									file = file + ".acidePrj";

								MainWindow.getInstance()
										.getProjectConfiguration()
										.setPath(file);
								String cad = MainWindow.getInstance()
										.getProjectConfiguration().save();
								projectTextFile.save(MainWindow.getInstance()
										.getProjectConfiguration()
										.getProjectPath(), cad);
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setFirstSave(true);

								// Updates the RESOURCE MANAGER
								ResourceManager.getInstance().setProperty(
										"defaultAcideProject", file);

								// Updates the RESOURCE MANAGER
								ResourceManager.getInstance().setProperty(
										"defaultPath", file);

								// The project configuration has not been modified yet
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setIsModified(false);
							}
						} catch (Exception exception) {

							// Updates the log
							AcideLog.getLog().error(exception.getMessage());
							exception.printStackTrace();
						}

						// Builds the EXPLORER
						MainWindow.getInstance().getExplorer().getRoot()
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

								explorerFile = new ExplorerFile();
								explorerFile.setIsMainFile(MainWindow
										.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).isMainFile());
								explorerFile.setIsCompilableFile(MainWindow
										.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i)
										.isCompilerFile());
								explorerFile.setPath(MainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(i)
										.getAbsolutePath());
								explorerFile.setParent(defaultMutableTreeNode
										.toString());
								MainWindow.getInstance()
										.getProjectConfiguration()
										.addFile(explorerFile);
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

								explorerFile.setName(fileName);
								DefaultMutableTreeNode de = new DefaultMutableTreeNode(
										explorerFile);
								de.setAllowsChildren(false);
								defaultMutableTreeNode.add(de);
							}
						}

						// SET IS EXPLORER SHOWED
						MainWindow.getInstance().getProjectConfiguration()
								.setIsExplorerShowed(true);

						// SET IS SHELL SHOWED
						MainWindow.getInstance().getProjectConfiguration()
								.setIsShellShowed(true);

						// SET WINDOW PROPERTIES
						MainWindow
								.getInstance()
								.getProjectConfiguration()
								.setWindowWidth(
										MainWindow.getInstance().getWidth());
						MainWindow
								.getInstance()
								.getProjectConfiguration()
								.setWindowHeight(
										MainWindow.getInstance().getHeight());
						MainWindow.getInstance().getProjectConfiguration()
								.setPosX(MainWindow.getInstance().getX());
						MainWindow.getInstance().getProjectConfiguration()
								.setPosY(MainWindow.getInstance().getY());
						MainWindow
								.getInstance()
								.getProjectConfiguration()
								.setSplitPaneVerticalDividerLocation(
										MainWindow.getInstance().getExplorer()
												.getWidth());
						MainWindow
								.getInstance()
								.getProjectConfiguration()
								.setSplitPaneHorizontalDividerLocation(
										MainWindow.getInstance().getOutput()
												.getHeight());

						MainWindow.getInstance().validate();
						MainWindow.getInstance().repaint();
						MainWindow.getInstance().getExplorer().getPopupMenu()
								.getAddFile().setEnabled(true);
						MainWindow.getInstance().getExplorer().getPopupMenu()
								.getSaveProject().setEnabled(true);
						MainWindow.getInstance().getExplorer().getTreeModel()
								.reload();
						MainWindow.getInstance().getExplorer().expandTree();
						MainWindow.getInstance().setEnabled(true);

						// Closes the window
						dispose();

						// Shows the explorer if it has to do it
						if (!MainWindow.getInstance().getMenu().getView()
								.getShowExplorerPanel().isSelected())
							MainWindow.getInstance().getExplorer()
									.showExplorer();

						// Updates the view options
						MainWindow.getInstance().getMenu().getView()
								.getShowExplorerPanel().setSelected(true);
						MainWindow.getInstance().getMenu().getView()
								.getShowShellWindow().setSelected(true);

						// Enables the project menu
						MainWindow.getInstance().getMenu().enableProjectMenu();
					}
				} else {

					if (_nameTextField.getText().equals(""))
						// Shows a warning message
						JOptionPane.showMessageDialog(null,
								labels.getString("s973"),
								labels.getString("s972"),
								JOptionPane.WARNING_MESSAGE);

					if (_lexicalConfigurationNameLabel.getText().contains(
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
				AcideLanguage language = AcideLanguage.getInstance();
				try {
					language.getLanguage(ResourceManager.getInstance()
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
				AcideGUIFactory.getInstance().buildOutputConfigurationWindow();
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
						.buildCompilerConfigurationWindow();
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

		// LEXICAL CONFIGURATION PANEL
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_lexicalConfigurationNameLabel,
				constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_languageConfigurationPanel.add(_createLexicalButton, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_loadLexicalButton, constraints);
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
		ResourceManager.getInstance()
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
		mainWindow.getProjectConfiguration().setSyntacticConfiguration(
				grammarFile);
		mainWindow.validate();
		mainWindow.repaint();

		// UPDATES THE MENU
		mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar()
				.setEnabled(false);
	}

	/**
	 * Returns the lexical configuration name label.
	 * 
	 * @return the lexical configuration name label.
	 */
	public JLabel getLexicalConfigurationNameLabel() {
		return _lexicalConfigurationNameLabel;
	}

	/**
	 * Sets a new value to the lexical configuration name label.
	 * 
	 * @param lexicalConfigurationLabel
	 *            new value to set.
	 */
	public void setLexicalConfigurationNameLabel(
			String lexicalConfigurationLabel) {
		_lexicalConfigurationNameLabel.setText(lexicalConfigurationLabel);
	}

	/**
	 * Returns the lexical configuration name.
	 * 
	 * @return the lexical configuration name.
	 */
	public String getLexicalConfigurationName() {
		return _lexicalConfigurationName;
	}

	/**
	 * Sets a new value to lexical configuration name.
	 * 
	 * @param lexicalConfigurationName
	 *            new value to set.
	 */
	public void setLexicalConfigurationName(String lexicalConfigurationName) {
		_lexicalConfigurationName = lexicalConfigurationName;
	}

	/**
	 * Returns the accept button.
	 * 
	 * @return the accept button.
	 */
	public JButton getAcceptButton() {
		return _acceptButton;
	}

	/**
	 * Returns the grammar configuration name.
	 * 
	 * @return the grammar configuration name.
	 */
	public String getGrammarConfigurationName() {
		return _grammarConfigurationName;
	}

	/**
	 * Sets a new value to the grammar configuration name.
	 * 
	 * @param grammarConfigurationName
	 *            new value to set.
	 */
	public void setGrammarConfigurationName(String grammarConfigurationName) {
		_grammarConfigurationName = grammarConfigurationName;
	}

	/**
	 * Returns the grammar configuration name label.
	 * 
	 * @return the grammar configuration name label.
	 */
	public JLabel getGrammarConfigurationNameLabel() {
		return _grammarConfigurationNameLabel;
	}

	/**
	 * Sets a new value to the grammar configuration name label.
	 * 
	 * @param grammarConfigurationNameLabel
	 *            new value to set.
	 */
	public void setNombreConfLabelGram(JLabel grammarConfigurationNameLabel) {
		_grammarConfigurationNameLabel = grammarConfigurationNameLabel;
	}

	/**
	 * Returns the workspace path.
	 * 
	 * @return the workspace path.
	 */
	public String getWorkspacePath() {
		return _workspacePath;
	}

	/**
	 * Sets a new value to the workspace path.
	 * 
	 * @param workspacePath
	 *            new value to set.
	 */
	public void setWorkspacePath(String workspacePath) {
		_workspacePath = workspacePath;
	}

	/**
	 * Sets a new value to the are shell paths defined flag.
	 * 
	 * @param areShellPathsDefined
	 *            new value to set.
	 */
	public void setAreShellPathsDefined(boolean areShellPathsDefined) {
		_areShellPathsDefined = areShellPathsDefined;
	}

	/**
	 * Sets a new value to the are compiler paths defined flag.
	 * 
	 * @param areCompilerPathsDefined
	 *            new value to set.
	 */
	public void setAreCompilerPathsDefined(boolean areCompilerPathsDefined) {
		_areCompilerPathsDefined = areCompilerPathsDefined;
	}
}
