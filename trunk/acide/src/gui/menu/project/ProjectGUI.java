package gui.menu.project;

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
import javax.swing.tree.DefaultMutableTreeNode;

import language.Language;
import operations.lexicon.Comments;
import operations.lexicon.DividerList;
import operations.configuration.ExplorerFile;
import operations.factory.IOFactory;
import operations.factory.GUIFactory;
import operations.lexicon.TokenTypeList;
import operations.listeners.AcideWindowListener;
import operations.log.Log;
import org.apache.log4j.Logger;
import properties.PropertiesManager;
import es.configuration.programmingLanguage.ProgrammingLanguage;
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

		// GET THE MAIN WINDOW
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);

		_logger.info(_labels.getString("s587"));

		// FRAME
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s588"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());

		// GENERAL PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s589")));
		_mainPanel.setLayout(new GridBagLayout());

		// LANGUAGE CONFIGURATION PANEL
		_languageConfigurationPanel = new JPanel();
		_languageConfigurationPanel.setBorder(BorderFactory
				.createTitledBorder(_labels.getString("s590")));
		_languageConfigurationPanel.setLayout(new GridBagLayout());

		// COMPILER PANEL
		_compilerPanel = new JPanel();
		_compilerPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s591")));
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
		_lexicalConfigurationNameLabel = new JLabel(_labels.getString("s599") + " "
				+ _lexicalConfigurationName);

		// GRAMMAR CONFIGURATION
		_grammarConfigurationName = _labels.getString("s598");
		_grammarConfigurationNameLabel = new JLabel(_labels.getString("s642") + " "
				+ _grammarConfigurationName);

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

		// WORKSPACE BUTTON
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
				DividerList.getInstance().reset();
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
				_lexicalConfigurationName =  MainWindow.getInstance()
						.getProjectConfiguration().getLexicalConfiguration();
				
				// GET THE NAME OF THE CURRENT LEXICAL CONFIGURATION
				String lexicalConfigurationName = ProgrammingLanguage.getInstance()
						.getName();
				
				_lexicalConfigurationNameLabel.setText(_labels.getString("s599")
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
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getGrammar()
						.getNewGrammar().doClick();
				int index = mainWindow.getProjectConfiguration()
						.getSyntacticConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = mainWindow.getProjectConfiguration()
							.getSyntacticConfiguration().lastIndexOf("/");
				String grammarName = mainWindow
						.getProjectConfiguration()
						.getSyntacticConfiguration()
						.substring(
								index + 1,
								mainWindow.getProjectConfiguration()
										.getSyntacticConfiguration().length() - 4);
				_grammarConfigurationNameLabel.setText(_labels.getString("s642")
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

				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getGrammar()
						.getLoadGrammar().doClick();
				int index = mainWindow.getProjectConfiguration()
						.getSyntacticConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = mainWindow.getProjectConfiguration()
							.getSyntacticConfiguration().lastIndexOf("/");
				String grammarName = mainWindow
						.getProjectConfiguration()
						.getSyntacticConfiguration()
						.substring(
								index + 1,
								mainWindow.getProjectConfiguration()
										.getSyntacticConfiguration().length() - 4);
				_grammarConfigurationNameLabel.setText(_labels.getString("s642")
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

				MainWindow mainWindow = MainWindow.getInstance();

				if (!_nameTextField.getText().equals("")) {

					boolean selected = true;
					String txtFile = _workspaceTextField.getText() + "\\"
							+ _nameTextField.getText();
					if (!txtFile.contains(".acidePrj"))
						txtFile = txtFile + ".acidePrj";

					File ff = new File(txtFile);

					if (ff.exists()) {
						int result = JOptionPane.showConfirmDialog(null,
								_labels.getString("s955"), _labels
										.getString("s953"),
								JOptionPane.YES_NO_OPTION);
						if (result == JOptionPane.YES_OPTION) {
							selected = true;
						}
						if (result == JOptionPane.NO_OPTION) {
							selected = false;
						}
					} else
						selected = true;

					if (selected) {

						if (!_areCompilerPathsDefined) {
							mainWindow.getProjectConfiguration()
									.setCompilerPath(null);
							mainWindow.getProjectConfiguration()
									.setCompilerArguments(null);
						} else {
							if (mainWindow.getProjectConfiguration()
									.getCompilerPath().equals(""))
								mainWindow.getProjectConfiguration()
										.setCompilerPath(null);
							if (mainWindow.getProjectConfiguration()
									.getCompilerArguments().equals(""))
								mainWindow.getProjectConfiguration()
										.setCompilerArguments(null);
						}
						
						if (!_areShellPathsDefined) {
							mainWindow.getProjectConfiguration().setShellPath(
									null);
							mainWindow.getProjectConfiguration()
									.setShellDirectory(null);
						} else {
							if (mainWindow.getProjectConfiguration()
									.getShellPath().equals(""))
								mainWindow.getProjectConfiguration()
										.setShellPath(null);
							if (mainWindow.getProjectConfiguration()
									.getShellDirectory().equals(""))
								mainWindow.getProjectConfiguration()
										.setShellDirectory(null);
						}

						mainWindow.getProjectConfiguration().removeFiles();
						mainWindow.getProjectConfiguration().setName(
								_nameTextField.getText());
						if (_lexicalConfigurationName.equals(_labels
								.getString("s598"))) {
							try {
								_lexicalConfigurationName = PropertiesManager
										.getProperty("languagePath");
							} catch (Exception e1) {

							}
						}
						mainWindow.getProjectConfiguration()
								.setLexicalConfiguration(
										_lexicalConfigurationName);
						mainWindow.getProjectConfiguration()
								.setFirstSave(false);
						_logger.info(_labels.getString("s615")
								+ _nameTextField.getText());
						mainWindow.getExplorer().getRoot().removeAllChildren();

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

							if (!mainWindow.getProjectConfiguration().getName()
									.equals("")) {

								String[] askExtension = new String[] { "acidePrj" };
								f.getFileChooser().addChoosableFileFilter(
										new ExtensionFilter(askExtension,
												labels.getString("s328")));

								String file = _workspaceTextField.getText()
										+ "\\" + _nameTextField.getText();

								String currentMenu = PropertiesManager
										.getProperty("currentMenuConfiguration");
								String currentTB = PropertiesManager
										.getProperty("currentToolBarConfiguration");

								mainWindow.getProjectConfiguration().setMenu(
										currentMenu);
								mainWindow.getProjectConfiguration()
										.setToolBar(currentTB);

								if (!file.contains(".acidePrj"))
									file = file + ".acidePrj";

								mainWindow.getProjectConfiguration().setPath(
										file);
								String cad = mainWindow
										.getProjectConfiguration().save();
								f.save(mainWindow.getProjectConfiguration()
										.getProjectPath(), cad);
								mainWindow.getProjectConfiguration()
										.setFirstSave(true);
								PropertiesManager.setProperty(
										"defaultAcideProject", file);
								PropertiesManager.setProperty("defaultPath",
										file);

								mainWindow.getProjectConfiguration()
										.setIsModified(false);
							}
						} catch (Exception ex) {
							ex.printStackTrace();
						}

						mainWindow.getExplorer().getRoot().add(
								defaultMutableTreeNode);
						mainWindow.setTitle(_labels.getString("s425") + " - "
								+ _nameTextField.getText());

						// ADD ALL THE EDITORS
						for (int i = 0; i < mainWindow.getEditorBuilder()
								.getNumEditors(); i++) {
							explorerFile = new ExplorerFile();
							explorerFile.setIsMainFile(mainWindow
									.getEditorBuilder().getEditorAt(i)
									.isMainFile());
							explorerFile.setIsCompilableFile(mainWindow
									.getEditorBuilder().getEditorAt(i)
									.isCompilerFile());
							explorerFile.setPath(mainWindow.getEditorBuilder()
									.getEditorAt(i).getAbsolutePath());
							explorerFile.setParent(defaultMutableTreeNode
									.toString());
							mainWindow.getProjectConfiguration().addFile(
									explorerFile);
							String file = mainWindow.getEditorBuilder()
									.getEditorAt(i).getAbsolutePath();


							String txtFile2 = "";
							int index2 = file.lastIndexOf("\\");
							if (index2 == -1)
								index2 = file.lastIndexOf("/");
							index2++;
							txtFile2 = file.substring(index2, file.length());

							explorerFile.setName(txtFile2);
							DefaultMutableTreeNode de = new DefaultMutableTreeNode(
									explorerFile);
							de.setAllowsChildren(false);
							defaultMutableTreeNode.add(de);

						}

						mainWindow.getProjectConfiguration()
								.setIsExplorerShowed(true);
						mainWindow.getProjectConfiguration().setIsShellShowed(
								true);
						mainWindow.getProjectConfiguration().setWindowWidth(
								mainWindow.getWidth());
						mainWindow.getProjectConfiguration().setWindowHeight(
								mainWindow.getHeight());
						mainWindow.getProjectConfiguration().setPosX(
								mainWindow.getX());
						mainWindow.getProjectConfiguration().setPosY(
								mainWindow.getY());
						mainWindow.getProjectConfiguration()
								.setSplitPaneVerticalDividerLocation(
										mainWindow.getExplorer().getWidth());
						mainWindow.getProjectConfiguration()
								.setSplitPaneHorizontalDividerLocation(
										mainWindow.getOutput().getHeight());

						mainWindow.validate();
						mainWindow.repaint();
						mainWindow.getExplorer().getPopupMenu().getAddFile().setEnabled(true);
						mainWindow.getExplorer().getPopupMenu().getSaveProject().setEnabled(true);
						mainWindow.getExplorer().getTreeModel().reload();
						mainWindow.getExplorer().expandTree();
						mainWindow.setEnabled(true);
						_frame.dispose();

						if (!mainWindow.getMenu().getView().getShowBrowser()
								.isSelected())
							mainWindow.getExplorer().showExplorer();

						mainWindow.getMenu().getView().getShowBrowser()
								.setSelected(true);
						mainWindow.getMenu().getView().getShowShellWindow()
								.setSelected(true);
						mainWindow.getMenu().enableProjectMenu();
					}
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
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_mainPanel, constraints);
		
		// LEXICAL CONFIGURATION PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridwidth = 2;
		_languageConfigurationPanel
				.add(_lexicalConfigurationNameLabel, constraints);
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
		_languageConfigurationPanel
				.add(_grammarConfigurationNameLabel, constraints);
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.ipadx = 40;
		_languageConfigurationPanel.add(_createGrammarButton, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_loadGrammarButton, constraints);
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
	 * @param lexicalConfigurationLabel New value to set.
	 */
	public void setLexicalConfigurationNameLabel(String lexicalConfigurationLabel) {
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
	 * @param lexicalConfigurationName New value to set.
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
	 * @param grammarConfigurationName New value to set.
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
	 * @param grammarConfigurationNameLabel New value to set.
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
	 * @param workspacePath New value to set.
	 */
	public void setWorkspacePath(String workspacePath) {
		_workspacePath = workspacePath;
	}

	/**
	 * Set a new value to the are shell paths defined flag.
	 *  
	 * @param areShellPathsDefined New value to set.
	 */
	public void setAreShellPathsDefined(boolean areShellPathsDefined) {
		_areShellPathsDefined = areShellPathsDefined;
	}

	/**
	 * Set a new value to the are compiler paths defined flag.
	 *  
	 * @param areCompilerPathsDefined New value to set.
	 */
	public void setAreCompilerPathsDefined(boolean areCompilerPathsDefined) {
		_areCompilerPathsDefined = areCompilerPathsDefined;
	}
}
