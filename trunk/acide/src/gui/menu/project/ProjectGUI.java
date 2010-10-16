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
 * 
 */
public class ProjectGUI extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private JFrame _frame;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private ResourceBundle _labels;
	/**
	 * 
	 */
	private String _lexicalConfigurationName;
	/**
	 * 
	 */
	private JLabel _lblLexicalConfiguration;
	/**
	 * 
	 */
	private String _grammarConfigurationName;
	/**
	 * 
	 */
	private JLabel _lblGrammarConfiguration;
	/**
	 * 
	 */
	private JTextField _txtName;
	/**
	 * 
	 */
	private JButton _btnAccept;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private JButton _btnWorkspace;
	/**
	 * 
	 */
	private JButton _btnCompiler;
	/**
	 * 
	 */
	private JButton _btnInterpreter;
	/**
	 * 
	 */
	private JButton _btnCreateGrammar;
	/**
	 * 
	 */
	private JButton _btnLoadGrammar;
	/**
	 * 
	 */
	private String _workspacePath;
	/**
	 * 
	 */
	private JTextField _txtWorkspace;
	/**
	 * 
	 */
	private JPanel _generalPanel;
	/**
	 * 
	 */
	private JPanel _languageConfigurationPanel;
	/**
	 * 
	 */
	private JPanel _compilerPanel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
	/**
	 * 
	 */
	private JLabel _lblName;
	/**
	 * 
	 */
	private JButton _btnCreateLexical;
	/**
	 * 
	 */
	private JButton _btnLoadLexical;
	/**
	 * 
	 */
	private JLabel _lblWorkspace;
	/**
	 * 
	 */
	private boolean _b1;
	/**
	 * 
	 */
	private boolean _b2;

	/**
	 * Constructor of the class.
	 */
	public ProjectGUI() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		_b1 = false;
		_b2 = false;

		_labels = language.getLabels();
		
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		
		_logger.info(_labels.getString("s587"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s588"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());

		// GENERAL PANEL
		_generalPanel = new JPanel();
		_generalPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s589")));
		_generalPanel.setLayout(new GridBagLayout());
		
		// LANGUAGE CONFIGURATION PANEL
		_languageConfigurationPanel = new JPanel();
		_languageConfigurationPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s590")));
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
		_lblName = new JLabel(_labels.getString("s592"));
		_txtName = new JTextField();
		_txtName.setToolTipText(_labels.getString("s593"));
		
		// LEXICAL CONFIGURATION
		_lexicalConfigurationName = _labels.getString("s598");
		_lblLexicalConfiguration = new JLabel(_labels.getString("s599") + " "
				+ _lexicalConfigurationName);
		
		// GRAMMAR CONFIGURATION
		_grammarConfigurationName = _labels.getString("s598");
		_lblGrammarConfiguration = new JLabel(_labels.getString("s642") + " "
				+ _grammarConfigurationName);
		
		// CREATE LEXICAL BUTTON
		_btnCreateLexical = new JButton(_labels.getString("s600"));
		_btnCreateLexical.setHorizontalAlignment(JButton.CENTER);
		_btnCreateLexical.setToolTipText(_labels.getString("s601"));

		// LOAD LEXICAL BUTTON
		_btnLoadLexical = new JButton(_labels.getString("s602"));
		_btnLoadLexical.setHorizontalAlignment(JButton.CENTER);
		_btnLoadLexical.setToolTipText(_labels.getString("s603"));

		// CREATE GRAMMAR BUTTON
		_btnCreateGrammar = new JButton(_labels.getString("s600"));
		_btnCreateGrammar.setHorizontalAlignment(JButton.CENTER);
		_btnCreateGrammar.setToolTipText(_labels.getString("s601"));
		
		// LOAD GRAMMAR BUTTON
		_btnLoadGrammar = new JButton(_labels.getString("s602"));
		_btnLoadGrammar.setHorizontalAlignment(JButton.CENTER);
		_btnLoadGrammar.setToolTipText(_labels.getString("s603"));

		// INTERPRETER BUTTON
		_btnInterpreter = new JButton(_labels.getString("s637"));
		_btnInterpreter.setHorizontalAlignment(JButton.CENTER);
		_btnInterpreter.setToolTipText(_labels.getString("s637"));

		// COMPILER BUTTON
		_btnCompiler = new JButton(_labels.getString("s636"));
		_btnCompiler.setHorizontalAlignment(JButton.CENTER);
		_btnCompiler.setToolTipText(_labels.getString("s636"));

		// WORKSPACE BUTTON
		_btnWorkspace = new JButton(_labels.getString("s948"));
		_btnWorkspace.setHorizontalAlignment(JButton.RIGHT);

		_txtWorkspace = new JTextField("");
		_lblWorkspace = new JLabel(_labels.getString("s949"));

		// ACCEPT BUTTON
		_btnAccept = new JButton(_labels.getString("s154"));
		_btnAccept.setHorizontalAlignment(JButton.CENTER);
		_btnAccept.setToolTipText(_labels.getString("s611"));
		
		// CANCEL BUTTON
		_btnCancel = new JButton(_labels.getString("s162"));
		_btnCancel.setHorizontalAlignment(JButton.CENTER);
		_btnCancel.setToolTipText(_labels.getString("s612"));

		// LISTENERS
		_btnCreateLexical.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TokenTypeList.getInstance().reset();
				DividerList.getInstance().reset();
				Comments.getInstance().reset();
				GUIFactory.getInstance().buildLanguageGUI();
			}
		});
		
		_btnLoadLexical.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getLexicon().getLoadParameters().doClick();
				_lexicalConfigurationName = mainWindow.getProjectConfiguration()
						.getLanguageConfiguration();
				String nombreConfLex = ProgrammingLanguage.getInstance()
						.getName();
				_lblLexicalConfiguration.setText(_labels.getString("s599") + " "
						+ nombreConfLex);

			}
		});
		
		_btnCreateGrammar.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getGrammar().getNewGrammar().doClick();
				int index = mainWindow.getProjectConfiguration()
						.getGrammarConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = mainWindow.getProjectConfiguration()
							.getGrammarConfiguration().lastIndexOf("/");
				String grammarName = mainWindow
						.getProjectConfiguration()
						.getGrammarConfiguration()
						.substring(
								index + 1,
								mainWindow.getProjectConfiguration()
										.getGrammarConfiguration().length() - 4);
				_lblGrammarConfiguration.setText(_labels.getString("s642") + " "
						+ grammarName);
				_lblGrammarConfiguration.validate();
				_lblGrammarConfiguration.repaint();

			}
		});
		_btnLoadGrammar.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getGrammar().getLoadGrammar().doClick();
				int index = mainWindow.getProjectConfiguration()
						.getGrammarConfiguration().lastIndexOf("\\");
				if (index == -1)
					index = mainWindow.getProjectConfiguration()
							.getGrammarConfiguration().lastIndexOf("/");
				String grammarName = mainWindow
						.getProjectConfiguration()
						.getGrammarConfiguration()
						.substring(
								index + 1,
								mainWindow.getProjectConfiguration()
										.getGrammarConfiguration().length() - 4);
				_lblGrammarConfiguration.setText(_labels.getString("s642") + " "
						+ grammarName);
				_lblGrammarConfiguration.validate();
				_lblGrammarConfiguration.repaint();
			}
		});
		_btnWorkspace.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				JFileChooser fileChooser = new JFileChooser();
				fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int option = fileChooser.showOpenDialog(null);
				
				if (option == JFileChooser.APPROVE_OPTION)
					_workspacePath = fileChooser.getSelectedFile().getAbsolutePath();
				
				_txtWorkspace.setText(_workspacePath);
				_txtWorkspace.validate();
				_txtWorkspace.repaint();
			}
		});
		
		_btnAccept.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				MainWindow mainWindow = MainWindow.getInstance();
				
				if (!_txtName.getText().equals("")) {
					
					boolean selected = true;
					String txtFile = _txtWorkspace.getText() + "\\"
							+ _txtName.getText();
					if (!txtFile.contains(".acidePrj"))
						txtFile = txtFile + ".acidePrj";
					
					File ff = new File(txtFile);
					
					if (ff.exists()) {
						int result = JOptionPane.showConfirmDialog(null,
								_labels.getString("s955"),
								_labels.getString("s953"),
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

						if (!_b1) {
							mainWindow.getProjectConfiguration().setCompilerPath(null);
							mainWindow.getProjectConfiguration().setCompilerArguments(
									null);
						} else {
							if (mainWindow.getProjectConfiguration().getCompilerPath()
									.equals(""))
								mainWindow.getProjectConfiguration().setCompilerPath(
										null);
							if (mainWindow.getProjectConfiguration()
									.getCompilerArguments().equals(""))
								mainWindow.getProjectConfiguration()
										.setCompilerArguments(null);
						}
						if (!_b2) {
							mainWindow.getProjectConfiguration().setShellPath(null);
							mainWindow.getProjectConfiguration().setShellDir(null);
						} else {
							if (mainWindow.getProjectConfiguration().getShellPath()
									.equals(""))
								mainWindow.getProjectConfiguration().setShellPath(null);
							if (mainWindow.getProjectConfiguration().getShellDir()
									.equals(""))
								mainWindow.getProjectConfiguration().setShellDir(null);
						}

						mainWindow.getProjectConfiguration().removeFiles();
						mainWindow.getProjectConfiguration().setName(
								_txtName.getText());
						if (_lexicalConfigurationName.equals(_labels.getString("s598"))) {
							try {
								_lexicalConfigurationName = PropertiesManager
										.getProperty("languagePath");
							} catch (Exception e1) {
								
							}
						}
						mainWindow.getProjectConfiguration().setLanguageConfiguration(
								_lexicalConfigurationName);
						mainWindow.getProjectConfiguration().setFirstSave(false);
						_logger.info(_labels.getString("s615")
								+ _txtName.getText());
						mainWindow.getExplorer().getRoot().removeAllChildren();
						
						ExplorerFile explorerFile = new ExplorerFile();
						explorerFile.setPath(_txtName.getText());
						explorerFile.setName(_txtName.getText());
						explorerFile.setParent(null);
						explorerFile.setDirectory(true);
						
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
										new ExtensionFilter(askExtension, labels
												.getString("s328")));

								String file = _txtWorkspace.getText()
										+ "\\" + _txtName.getText();

								String currentMenu = PropertiesManager
										.getProperty("currentMenuConfiguration");
								String currentTB = PropertiesManager
										.getProperty("currentToolBarConfiguration");

								mainWindow.getProjectConfiguration().setCurrentMenu(
										currentMenu);
								mainWindow.getProjectConfiguration().setCurrentToolBar(
										currentTB);

								if (!file.contains(".acidePrj"))
									file = file + ".acidePrj";

								mainWindow.getProjectConfiguration().setPath(file);
								String cad = mainWindow.getProjectConfiguration().save();
								f.save(mainWindow.getProjectConfiguration()
										.getProjectPath(), cad);
								mainWindow.getProjectConfiguration().setFirstSave(true);
								PropertiesManager.setProperty(
										"defaultAcideProject", file);
								PropertiesManager.setProperty("DefaultPath",
										file);

								mainWindow.getProjectConfiguration().setModified(false);
							}
						} catch (Exception ex) {
							ex.printStackTrace();
						}

						mainWindow.getExplorer().getRoot().add(defaultMutableTreeNode);
						mainWindow.setTitle(_labels.getString("s425") + " - "
								+ _txtName.getText());

						// Add all editors
						mainWindow.getProjectConfiguration().setNumFiles(
								Integer.toString(mainWindow.getEditorBuilder()
										.getNumEditors()));

						for (int i = 0; i < mainWindow.getEditorBuilder()
								.getNumEditors(); i++) {
							explorerFile = new ExplorerFile();
							explorerFile.setMainFile(mainWindow.getEditorBuilder().getEditorAt(i)
									.isMainFile());
							explorerFile.setSetFile(mainWindow.getEditorBuilder().getEditorAt(i)
									.isCompilerFile());
							explorerFile.setPath(mainWindow.getEditorBuilder().getEditorAt(i)
									.getPath());
							explorerFile.setParent(defaultMutableTreeNode.toString());
							mainWindow.getProjectConfiguration().setFile(explorerFile);
							String file = mainWindow.getEditorBuilder().getEditorAt(i)
									.getPath();
							
							int index2 = file.lastIndexOf("\\");
							
							String txtFile2 = "";
							if (index2 != -1) {
								index2++;
								txtFile2 = file.substring(index2, file.length());
							} else {
								index2 = file.lastIndexOf("/");
								index2++;
								txtFile2 = file.substring(index2, file.length());
							}

							explorerFile.setName(txtFile2);
							DefaultMutableTreeNode de = new DefaultMutableTreeNode(
									explorerFile);
							de.setAllowsChildren(false);
							defaultMutableTreeNode.add(de);

						}

						mainWindow.getProjectConfiguration().setExplorer(true);
						mainWindow.getProjectConfiguration().setShell(true);
						mainWindow.getProjectConfiguration()
								.setWidthWindow(mainWindow.getWidth());
						mainWindow.getProjectConfiguration().setHeightWindow(
								mainWindow.getHeight());
						mainWindow.getProjectConfiguration().setPosX(mainWindow.getX());
						mainWindow.getProjectConfiguration().setPosY(mainWindow.getY());
						mainWindow.getProjectConfiguration().setWidth1(
								mainWindow.getExplorer().getWidth());
						mainWindow.getProjectConfiguration().setHeight1(
								mainWindow.getOutput().getHeight());

						mainWindow.validate();
						mainWindow.repaint();
						mainWindow.getExplorer().setEnabledAddFile();
						mainWindow.getExplorer().setEnabledSaveProj();
						mainWindow.getExplorer().getTreeModel().reload();
						mainWindow.getExplorer().expandTree();
						mainWindow.setEnabled(true);
						_frame.dispose();

						if (!mainWindow.getMenu().getView().getShowBrowser().isSelected())
							mainWindow.getExplorer().showExplorer();
						
						mainWindow.getMenu().getView().getShowBrowser().setSelected(true);
						mainWindow.getMenu().getView().getShowShellWindow().setSelected(true);
						mainWindow.getMenu().enableProjectMenu();
					} 
				}
			}
		});
		
		_btnCancel.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_logger.info(_labels.getString("s614"));
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});

		_btnInterpreter.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				GUIFactory.getInstance().generaOutputGUI();
			}
		});
		_btnCompiler.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				GUIFactory.getInstance().buildCompilerGUI();
			}
		});

		// SET THE COMPONENTS WITH LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipady = 10;
		_generalPanel.add(_lblName, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 140;
		constraints.ipady = 0;
		_generalPanel.add(_txtName, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_generalPanel.add(_lblWorkspace, constraints);

		constraints.gridx = 1;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_generalPanel.add(_txtWorkspace, constraints);

		constraints.gridx = 2;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_generalPanel.add(_btnWorkspace, constraints);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_generalPanel, constraints);

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_lblLexicalConfiguration, constraints);

		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 40;
		_languageConfigurationPanel.add(_btnCreateLexical, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_btnLoadLexical, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridwidth = 2;
		_languageConfigurationPanel.add(_lblGrammarConfiguration, constraints);

		constraints.gridy = 3;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.ipadx = 40;
		_languageConfigurationPanel.add(_btnCreateGrammar, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 0;
		_languageConfigurationPanel.add(_btnLoadGrammar, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_languageConfigurationPanel, constraints);

		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipady = 10;

		_compilerPanel.add(_btnCompiler, constraints);
		constraints.gridx = 1;
		_compilerPanel.add(_btnInterpreter, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;

		_frame.add(_compilerPanel, constraints);

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridy = 0;
		_buttonPanel.add(_btnAccept, constraints);

		constraints.gridx = 1;
		constraints.insets = new Insets(5, 70, 5, 5);
		_buttonPanel.add(_btnCancel, constraints);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_frame.add(_buttonPanel, constraints);
		_frame.setVisible(true);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setLocationRelativeTo(null);
		_logger.info(_labels.getString("s613"));
		AcideWindowListener window = new AcideWindowListener();
		_frame.addWindowListener(window);
	}

	/**
	 * 
	 * @return
	 */
	public JLabel getNombreConfLabelLexico() {
		return _lblLexicalConfiguration;
	}

	/**
	 * 
	 * @param nombreConfLabel
	 */
	public void setNombreConfLabelLexico(String nombreConfLabel) {
		_lblLexicalConfiguration.setText(nombreConfLabel);
	}

	/**
	 * 
	 * @return
	 */
	public String getNombreConfLexico() {
		return _lexicalConfigurationName;
	}

	/**
	 * 
	 * @param nombreConfLexico
	 */
	public void setNombreConfLexico(String nombreConfLexico) {
		_lexicalConfigurationName = nombreConfLexico;
	}

	/**
	 * 
	 * @return
	 */
	public JButton getAceptarBoton() {
		return _btnAccept;
	}

	/**
	 * 
	 * @return
	 */
	public String getNombreConfGram() {
		return _grammarConfigurationName;
	}

	/**
	 * 
	 * @param nombreConfGram
	 */
	public void setNombreConfGram(String nombreConfGram) {
		_grammarConfigurationName = nombreConfGram;
	}

	/**
	 * 
	 * @return
	 */
	public JLabel getNombreConfLabelGram() {
		return _lblGrammarConfiguration;
	}

	/**
	 * 
	 * @param nombreConfLabelGram
	 */
	public void setNombreConfLabelGram(JLabel nombreConfLabelGram) {
		_lblGrammarConfiguration = nombreConfLabelGram;
	}

	/**
	 * 
	 * @return
	 */
	public String getWorkspacePath() {
		return _workspacePath;
	}

	/**
	 * 
	 * @param workspacePath
	 */
	public void setWorkspacePath(String workspacePath) {
		_workspacePath = workspacePath;
	}

	/**
	 * 
	 * @param b
	 */
	public void setB2(boolean b) {
		_b2 = b;
	}

	/**
	 * 
	 * @param b
	 */
	public void setB1(boolean b) {
		_b1 = b;
	}
}
