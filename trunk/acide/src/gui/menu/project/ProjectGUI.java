package gui.menu.project;

import java.awt.Color;
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
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;
import javax.swing.tree.DefaultMutableTreeNode;

import language.Language;
import operations.lexicon.Comments;
import operations.lexicon.DelimiterList;
import operations.factory.IOFactory;
import operations.factory.GUIFactory;
import operations.lexicon.TokenTypeList;
import operations.listeners.AcideWindowListener;
import operations.log.Log;
import org.apache.log4j.Logger;
import properties.PropertiesManager;
import es.configuration.lexicon.LexiconConfiguration;
import es.explorer.ExplorerFile;
import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.MainWindow;

/**
 * Project GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ProjectGUI extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
	/**
	 * Main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * Language configuration panel.
	 */
	private JPanel _languageConfigurationPanel;
	/**
	 * Compiler panel.
	 */
	private JPanel _compilerPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Name text field.
	 */
	private JTextField _nameTextField;
	/**
	 * Workspace text field.
	 */
	private JTextField _workspaceTextField;
	/**
	 * Name label.
	 */
	private JLabel _nameLabel;
	/**
	 * Lexical configuration label.
	 */
	private JLabel _lexicalConfigurationNameLabel;
	/**
	 * Grammar configuration label.
	 */
	private JLabel _grammarConfigurationNameLabel;
	/**
	 * Workspace label.
	 */
	private JLabel _workspaceLabel;
	/**
	 * Accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Workspace button.
	 */
	private JButton _workspaceButton;
	/**
	 * Compiler button.
	 */
	private JButton _compilerButton;
	/**
	 * Output button.
	 */
	private JButton _outputButton;
	/**
	 * Create grammar button.
	 */
	private JButton _createGrammarButton;
	/**
	 * Load grammar button.
	 */
	private JButton _loadGrammarButton;
	/**
	 * Create lexical button.
	 */
	private JButton _createLexicalButton;
	/**
	 * Load lexical button.
	 */
	private JButton _loadLexicalButton;
	/**
	 * Lexical configuration name string.
	 */
	private String _lexicalConfigurationName;
	/**
	 * Grammar configuration name string.
	 */
	private String _grammarConfigurationName;
	/**
	 * Workspace path string.
	 */
	private String _workspacePath;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	/**
	 * Labels to display in the selected language.
	 */
	private ResourceBundle _labels;
	/**
	 * Flag that indicates if the compiler paths are defined or not.
	 */
	private boolean _areCompilerPathsDefined;
	/**
	 * Flag that indicates if the shell paths are defined or not.
	 */
	private boolean _areShellPathsDefined;

	/**
	 * Constructor of the class.
	 */
	public ProjectGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		_labels = language.getLabels();

		_areCompilerPathsDefined = false;
		_areShellPathsDefined = false;

		// DISABLE THE MAIN WINDOW
		MainWindow.getInstance().setEnabled(false);

		_logger.info(_labels.getString("s587"));

		// FRAME
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s588"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());

		// GENERAL PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s589"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_mainPanel.setLayout(new GridBagLayout());

		// LANGUAGE CONFIGURATION PANEL
		_languageConfigurationPanel = new JPanel();
		_languageConfigurationPanel.setBorder(BorderFactory
				.createTitledBorder(BorderFactory.createTitledBorder(null, _labels
						.getString("s590"),TitledBorder.LEADING,
						TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
						new Color(0, 0, 0))));
		_languageConfigurationPanel.setLayout(new GridBagLayout());

		// COMPILER PANEL
		_compilerPanel = new JPanel();
		_compilerPanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s591"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_compilerPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());

		// PROJECT NAME
		_nameLabel = new JLabel(_labels.getString("s592"));
		_nameTextField = new JTextField();
		_nameTextField.setToolTipText(_labels.getString("s593"));

		// LEXICAL CONFIGURATION
		_lexicalConfigurationName = _labels.getString("s598");
		_lexicalConfigurationNameLabel = new JLabel(_labels.getString("s599")
				+ " " + _lexicalConfigurationName);

		// GRAMMAR CONFIGURATION
		_grammarConfigurationName = _labels.getString("s598");
		_grammarConfigurationNameLabel = new JLabel(_labels.getString("s642")
				+ " " + _grammarConfigurationName);

		// CREATE LEXICAL BUTTON
		_createLexicalButton = new JButton(_labels.getString("s600"));
		_createLexicalButton.setHorizontalAlignment(JButton.CENTER);
		_createLexicalButton.setToolTipText(_labels.getString("s601"));

		// LOAD LEXICAL BUTTON
		_loadLexicalButton = new JButton(_labels.getString("s602"));
		_loadLexicalButton.setHorizontalAlignment(JButton.CENTER);
		_loadLexicalButton.setToolTipText(_labels.getString("s603"));

		// CREATE GRAMMAR BUTTON
		_createGrammarButton = new JButton(_labels.getString("s600"));
		_createGrammarButton.setHorizontalAlignment(JButton.CENTER);
		_createGrammarButton.setToolTipText(_labels.getString("s601"));

		// LOAD GRAMMAR BUTTON
		_loadGrammarButton = new JButton(_labels.getString("s602"));
		_loadGrammarButton.setHorizontalAlignment(JButton.CENTER);
		_loadGrammarButton.setToolTipText(_labels.getString("s603"));

		// OUTPUT BUTTON
		_outputButton = new JButton(_labels.getString("s637"));
		_outputButton.setHorizontalAlignment(JButton.CENTER);
		_outputButton.setToolTipText(_labels.getString("s637"));

		// COMPILER BUTTON
		_compilerButton = new JButton(_labels.getString("s636"));
		_compilerButton.setHorizontalAlignment(JButton.CENTER);
		_compilerButton.setToolTipText(_labels.getString("s636"));

		// WORKSPACE
		_workspaceButton = new JButton(_labels.getString("s948"));
		_workspaceButton.setHorizontalAlignment(JButton.RIGHT);
		_workspaceTextField = new JTextField("");
		_workspaceLabel = new JLabel(_labels.getString("s949"));

		// ACCEPT BUTTON
		_acceptButton = new JButton(_labels.getString("s154"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		_acceptButton.setToolTipText(_labels.getString("s611"));

		// CANCEL BUTTON
		_cancelButton = new JButton(_labels.getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(_labels.getString("s612"));

		// LISTENERS

		// CREATE LEXICAL BUTTON
		_createLexicalButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				TokenTypeList.getInstance().reset();
				DelimiterList.getInstance().reset();
				Comments.getInstance().reset();
				GUIFactory.getInstance().buildNewLexiconGUI();
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
			public void actionPerformed(ActionEvent e) {

				// LOAD THE PARAMETERS
				MainWindow.getInstance().getMenu().getConfiguration()
						.getLexicon().getLoadParameters().doClick();

				// GET THE CURRENT LEXICAL CONFIGURATION OF THE PROJECT
				_lexicalConfigurationName = MainWindow.getInstance()
						.getProjectConfiguration().getLexicalConfiguration();

				// GET THE NAME OF THE CURRENT LEXICAL CONFIGURATION
				String lexicalConfigurationName = LexiconConfiguration
						.getInstance().getName();

				_lexicalConfigurationNameLabel.setText(_labels
						.getString("s599")
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
			public void actionPerformed(ActionEvent e) {

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
				_grammarConfigurationNameLabel.setText(_labels
						.getString("s642")
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
			public void actionPerformed(ActionEvent e) {

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
				_grammarConfigurationNameLabel.setText(_labels
						.getString("s642")
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
			public void actionPerformed(ActionEvent e) {

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
			public void actionPerformed(ActionEvent e) {

				// IF THE REQUIRED PARAMETERS ARE SET
				if (!_nameTextField.getText().equals("") && 
						!_lexicalConfigurationNameLabel.getText().contains(_labels.getString("s598")) &&
						!_grammarConfigurationNameLabel.getText().contains(_labels.getString("s598"))) {

					boolean selected = true;

					String txtFile = "";
					int index1 = _workspaceTextField.getText()
							.lastIndexOf("\\");
					if (index1 == -1)
						txtFile = _workspaceTextField.getText() + "/"
								+ _nameTextField.getText();
					else
						txtFile = _workspaceTextField.getText() + "\\"
								+ _nameTextField.getText();

					if (!txtFile.contains(".acidePrj"))
						txtFile = txtFile + ".acidePrj";

					File ff = new File(txtFile);

					// IF THE FILE EXISTS
					if (ff.exists()) {

						int result = JOptionPane.showConfirmDialog(null,
								_labels.getString("s955"), _labels
										.getString("s953"),
								JOptionPane.YES_NO_OPTION);

						if (result == JOptionPane.YES_OPTION)
							selected = true;

						if (result == JOptionPane.NO_OPTION)
							selected = false;

					} else
						selected = true;

					if (selected) {

						// SET THE COMPILER PATHS
						if (!_areCompilerPathsDefined) {
							MainWindow.getInstance().getProjectConfiguration()
									.setCompilerPath(null);
							MainWindow.getInstance().getProjectConfiguration()
									.setCompilerArguments(null);
						} else {
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getCompilerPath().equals(""))
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setCompilerPath(null);
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getCompilerArguments().equals(""))
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setCompilerArguments(null);
						}

						// SET THE SHELL PATHS
						if (!_areShellPathsDefined) {
							MainWindow.getInstance().getProjectConfiguration()
									.setShellPath(null);
							MainWindow.getInstance().getProjectConfiguration()
									.setShellDirectory(null);
						} else {
							if (MainWindow.getInstance()
									.getProjectConfiguration().getShellPath()
									.equals(""))
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setShellPath(null);
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getShellDirectory().equals(""))
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setShellDirectory(null);
						}

						// SET THE LEXICAL CONFIGURATION
						MainWindow.getInstance().getProjectConfiguration()
								.removeFiles();
						MainWindow.getInstance().getProjectConfiguration()
								.setName(_nameTextField.getText());
						if (_lexicalConfigurationName.equals(_labels
								.getString("s598"))) {
							try {
								_lexicalConfigurationName = PropertiesManager
										.getProperty("languagePath");
							} catch (Exception e1) {

							}
						}

						MainWindow.getInstance().getProjectConfiguration()
								.setLexicalConfiguration(
										_lexicalConfigurationName);
						MainWindow.getInstance().getProjectConfiguration()
								.setFirstSave(false);
						_logger.info(_labels.getString("s615")
								+ _nameTextField.getText());
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

							IOFactory fact = IOFactory.getInstance();
							ResourceBundle labels = Language.getInstance()
									.getLabels();
							TextFile f = fact.buildFile();

							// GET THE PROJECT CONFIGURATION
							String project = null;
							try {
								project = PropertiesManager
										.getProperty("defaultAcideProject");
							} catch (Exception e1) {
								e1.printStackTrace();
							}

							// NOT DEFAULT PROJECT
							if (!(project
									.equals("./configuration/default.acidePrj") && MainWindow
									.getInstance().getProjectConfiguration()
									.getName().equals(""))) {

								String[] askExtension = new String[] { "acidePrj" };
								f.getFileChooser().addChoosableFileFilter(
										new ExtensionFilter(askExtension,
												labels.getString("s328")));

								String file = "";
								int index2 = _workspaceTextField.getText()
										.lastIndexOf("\\");
								if (index2 == -1)
									file = _workspaceTextField.getText() + "/"
											+ _nameTextField.getText();
								else
									file = _workspaceTextField.getText() + "\\"
											+ _nameTextField.getText();

								String currentMenu = PropertiesManager
										.getProperty("currentMenuConfiguration");
								String currentTB = PropertiesManager
										.getProperty("currentToolBarConfiguration");

								MainWindow.getInstance()
										.getProjectConfiguration().setMenu(
												currentMenu);
								MainWindow.getInstance()
										.getProjectConfiguration().setToolBar(
												currentTB);

								if (!file.contains(".acidePrj"))
									file = file + ".acidePrj";

								MainWindow.getInstance()
										.getProjectConfiguration()
										.setPath(file);
								String cad = MainWindow.getInstance()
										.getProjectConfiguration().save();
								f.save(MainWindow.getInstance()
										.getProjectConfiguration()
										.getProjectPath(), cad);
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setFirstSave(true);
								PropertiesManager.setProperty(
										"defaultAcideProject", file);
								PropertiesManager.setProperty("defaultPath",
										file);

								MainWindow.getInstance()
										.getProjectConfiguration()
										.setIsModified(false);
							}
						} catch (Exception ex) {
							ex.printStackTrace();
						}

						MainWindow.getInstance().getExplorer().getRoot().add(
								defaultMutableTreeNode);
						MainWindow.getInstance().setTitle(
								_labels.getString("s425") + " - "
										+ _nameTextField.getText());

						// ADD ALL THE EDITORS
						for (int i = 0; i < MainWindow.getInstance()
								.getEditorBuilder().getNumEditors(); i++) {

							// EXCEPT THE NEW FILE AND THE LOG TAB
							if (!MainWindow.getInstance().getEditorBuilder()
									.getEditorAt(i).isNewFile()
									&& !MainWindow.getInstance()
											.getEditorBuilder().getEditorAt(i)
											.isLogFile()) {
								
								explorerFile = new ExplorerFile();
								explorerFile.setIsMainFile(MainWindow
										.getInstance().getEditorBuilder()
										.getEditorAt(i).isMainFile());
								explorerFile.setIsCompilableFile(MainWindow
										.getInstance().getEditorBuilder()
										.getEditorAt(i).isCompilerFile());
								explorerFile.setPath(MainWindow.getInstance()
										.getEditorBuilder().getEditorAt(i)
										.getAbsolutePath());
								explorerFile.setParent(defaultMutableTreeNode
										.toString());
								MainWindow.getInstance()
										.getProjectConfiguration().addFile(
												explorerFile);
								String file = MainWindow.getInstance()
										.getEditorBuilder().getEditorAt(i)
										.getAbsolutePath();

								String txtFile2 = "";
								int index2 = file.lastIndexOf("\\");
								if (index2 == -1)
									index2 = file.lastIndexOf("/");
								txtFile2 = file.substring(index2 + 1, file
										.length());

								explorerFile.setName(txtFile2);
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
						MainWindow.getInstance().getProjectConfiguration()
								.setWindowWidth(
										MainWindow.getInstance().getWidth());
						MainWindow.getInstance().getProjectConfiguration()
								.setWindowHeight(
										MainWindow.getInstance().getHeight());
						MainWindow.getInstance().getProjectConfiguration()
								.setPosX(MainWindow.getInstance().getX());
						MainWindow.getInstance().getProjectConfiguration()
								.setPosY(MainWindow.getInstance().getY());
						MainWindow.getInstance().getProjectConfiguration()
								.setSplitPaneVerticalDividerLocation(
										MainWindow.getInstance().getExplorer()
												.getWidth());
						MainWindow.getInstance().getProjectConfiguration()
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
						_frame.dispose();

						if (!MainWindow.getInstance().getMenu().getView()
								.getShowBrowser().isSelected())
							MainWindow.getInstance().getExplorer()
									.showExplorer();

						MainWindow.getInstance().getMenu().getView()
								.getShowBrowser().setSelected(true);
						MainWindow.getInstance().getMenu().getView()
								.getShowShellWindow().setSelected(true);
						MainWindow.getInstance().getMenu().enableProjectMenu();
					}
				}
				else
					
					// WARNING MESSAGES
					if (!_nameTextField.getText().equals(""))
							JOptionPane.showMessageDialog(null,
							_labels.getString("s973"), _labels
									.getString("s972"),
							JOptionPane.WARNING_MESSAGE);
					
					if(!_lexicalConfigurationNameLabel.getText().contains(_labels.getString("s598")))
						JOptionPane.showMessageDialog(null,
								_labels.getString("s974"), _labels
										.getString("s972"),
								JOptionPane.WARNING_MESSAGE);
					
					if(!_grammarConfigurationNameLabel.getText().contains(_labels.getString("s598")))
						JOptionPane.showMessageDialog(null,
								_labels.getString("s975"), _labels
										.getString("s972"),
								JOptionPane.WARNING_MESSAGE);
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
			public void actionPerformed(ActionEvent e) {
				_logger.info(_labels.getString("s614"));
				MainWindow.getInstance().setEnabled(true);
				_frame.dispose();
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
			public void actionPerformed(ActionEvent e) {
				GUIFactory.getInstance().generaOutputGUI();
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
			public void actionPerformed(ActionEvent e) {
				GUIFactory.getInstance().buildCompilerGUI();
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
		constraints.ipadx = 140;
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
		constraints.insets = new Insets(20, 20, 20, 20);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_mainPanel, constraints);

		// LEXICAL CONFIGURATION PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_lexicalConfigurationNameLabel,
				constraints);
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 40;
		_languageConfigurationPanel.add(_createLexicalButton, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_loadLexicalButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_grammarConfigurationNameLabel,
				constraints);
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.ipadx = 40;
		_languageConfigurationPanel.add(_createGrammarButton, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_loadGrammarButton, constraints);
		constraints.insets = new Insets(0, 20, 20, 20);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_languageConfigurationPanel, constraints);

		// COMPILER PANEL
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipady = 10;
		_compilerPanel.add(_compilerButton, constraints);
		constraints.gridx = 1;
		_compilerPanel.add(_outputButton, constraints);
		constraints.insets = new Insets(0, 20, 0, 20);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_frame.add(_compilerPanel, constraints);

		// BUTTON PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridy = 0;
		_buttonPanel.add(_acceptButton, constraints);
		constraints.gridx = 1;
		constraints.insets = new Insets(5, 70, 5, 5);
		_buttonPanel.add(_cancelButton, constraints);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_frame.add(_buttonPanel, constraints);

		_frame.setVisible(true);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setLocationRelativeTo(null);
		_logger.info(_labels.getString("s613"));
		_frame.addWindowListener(new AcideWindowListener());
	}

	/**
	 * Returns the lexical configuration name label.
	 * 
	 * @return The lexical configuration name label.
	 */
	public JLabel getLexicalConfigurationNameLabel() {
		return _lexicalConfigurationNameLabel;
	}

	/**
	 * Set a new value to the lexical configuration name label.
	 * 
	 * @param lexicalConfigurationLabel
	 *            New value to set.
	 */
	public void setLexicalConfigurationNameLabel(
			String lexicalConfigurationLabel) {
		_lexicalConfigurationNameLabel.setText(lexicalConfigurationLabel);
	}

	/**
	 * Returns the lexical configuration name.
	 * 
	 * @return The lexical configuration name.
	 */
	public String getLexicalConfigurationName() {
		return _lexicalConfigurationName;
	}

	/**
	 * Set a new value to lexical configuration name.
	 * 
	 * @param lexicalConfigurationName
	 *            New value to set.
	 */
	public void setLexicalConfigurationName(String lexicalConfigurationName) {
		_lexicalConfigurationName = lexicalConfigurationName;
	}

	/**
	 * Returns the accept button.
	 * 
	 * @return The accept button.
	 */
	public JButton getAcceptButton() {
		return _acceptButton;
	}

	/**
	 * Returns the grammar configuration name.
	 * 
	 * @return The grammar configuration name.
	 */
	public String getGrammarConfigurationName() {
		return _grammarConfigurationName;
	}

	/**
	 * Set a new value to the grammar configuration name.
	 * 
	 * @param grammarConfigurationName
	 *            New value to set.
	 */
	public void setGrammarConfigurationName(String grammarConfigurationName) {
		_grammarConfigurationName = grammarConfigurationName;
	}

	/**
	 * Returns the grammar configuration name label.
	 * 
	 * @return The grammar configuration name label.
	 */
	public JLabel getGrammarConfigurationNameLabel() {
		return _grammarConfigurationNameLabel;
	}

	/**
	 * Set a new value to the grammar configuration name label.
	 * 
	 * @param grammarConfigurationNameLabel
	 *            New value to set.
	 */
	public void setNombreConfLabelGram(JLabel grammarConfigurationNameLabel) {
		_grammarConfigurationNameLabel = grammarConfigurationNameLabel;
	}

	/**
	 * Returns the workspace path.
	 * 
	 * @return The workspace path.
	 */
	public String getWorkspacePath() {
		return _workspacePath;
	}

	/**
	 * Set a new value to the workspace path.
	 * 
	 * @param workspacePath
	 *            New value to set.
	 */
	public void setWorkspacePath(String workspacePath) {
		_workspacePath = workspacePath;
	}

	/**
	 * Set a new value to the are shell paths defined flag.
	 * 
	 * @param areShellPathsDefined
	 *            New value to set.
	 */
	public void setAreShellPathsDefined(boolean areShellPathsDefined) {
		_areShellPathsDefined = areShellPathsDefined;
	}

	/**
	 * Set a new value to the are compiler paths defined flag.
	 * 
	 * @param areCompilerPathsDefined
	 *            New value to set.
	 */
	public void setAreCompilerPathsDefined(boolean areCompilerPathsDefined) {
		_areCompilerPathsDefined = areCompilerPathsDefined;
	}
}
