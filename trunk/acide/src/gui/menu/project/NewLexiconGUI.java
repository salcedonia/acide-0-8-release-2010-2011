package gui.menu.project;

import gui.MainWindow;
import gui.menu.configuration.grammar.GrammarGUI;
import gui.menu.configuration.lexicon.LexiconGUI;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.configuration.lexicon.LexiconConfiguration;
import es.text.ValidExtensions;

/**
 * 
 */
public class NewLexiconGUI extends JFrame {

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
	private JPanel _namePanel;
	/**
	 * 
	 */
	private JPanel _configurationPanel;
	/**
	 * 
	 */
	private JPanel _extensionsPanel;
	/**
	 * 
	 */
	private JPanel _typePanel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
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
	JLabel _lblName;
	/**
	 * 
	 */
	private final JTextField _tfName; 
	/**
	 * 
	 */
	private JButton _btnLexicalConfiguration;
	/**
	 * 
	 */
	private JButton _bntGrammarConfiguration;
	/**
	 * 
	 */
	private JLabel _lblFiles;
	/**
	 * 
	 */
	private final JTextField _tfFiles;
	/**
	 * 
	 */
	private final JRadioButton _rdCompiled;
	/**
	 * 
	 */
	private JRadioButton _rdInterpreted;
	/**
	 * 
	 */
	private ButtonGroup _radioGroup;
	/**
	 * 
	 */
	private String _undoPath = "";

	/**
	 * Constructor of the class.
	 */
	public NewLexiconGUI() {

		ResourceBundle labels = Language.getInstance().getLabels();
		_logger.info(labels.getString("s351"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setLayout(new GridBagLayout());
		_frame.setTitle(labels.getString("s352"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());

		// NAME PANEL
		_namePanel = new JPanel();
		_namePanel.setLayout(new GridBagLayout());
		_namePanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s353"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// CONFIGURATION PANEL
		_configurationPanel = new JPanel();
		_configurationPanel.setLayout(new GridBagLayout());
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s354"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// EXTENSIONS PANEL
		_extensionsPanel = new JPanel();
		_extensionsPanel.setLayout(new GridLayout(0, 1));
		_extensionsPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s355"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// TYPE PANEL
		_typePanel = new JPanel();
		_typePanel.setLayout(new GridLayout(1, 0));
		_typePanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s356"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());

		// NAME
		_lblName = new JLabel(labels.getString("s357"), JLabel.CENTER);
		_tfName = new JTextField();
		_tfName.setToolTipText(labels.getString("s358"));
		
		// LEXICAL CONFIGURATION BUTTON
		_btnLexicalConfiguration = new JButton(labels.getString("s359"));
		_btnLexicalConfiguration.setHorizontalAlignment(JButton.CENTER);
		_btnLexicalConfiguration.setToolTipText(labels.getString("s360"));
		
		// GRAMMAR CONFIGURATION BUTTON
		_bntGrammarConfiguration = new JButton(labels.getString("s361"));
		_bntGrammarConfiguration.setToolTipText(labels.getString("s362"));
		_bntGrammarConfiguration.setHorizontalAlignment(JButton.CENTER);
		
		// FILES 
		_lblFiles = new JLabel(labels.getString("s363"),
				JLabel.CENTER);
		_tfFiles = new JTextField();
		_tfFiles.setToolTipText(labels.getString("s364"));
		
		// COMPILED 
		_rdCompiled = new JRadioButton(
				labels.getString("s365"));
		_rdCompiled.setHorizontalAlignment(JRadioButton.CENTER);
		
		// INTERPRETED
		_rdInterpreted = new JRadioButton(
				labels.getString("s366"));
		_rdInterpreted.setHorizontalAlignment(JRadioButton.CENTER);
		
		// RADIO GROUP
		_radioGroup = new ButtonGroup();
		_radioGroup.add(_rdCompiled);
		_radioGroup.add(_rdInterpreted);
		
		// ACCEPT BUTTON
		_btnAccept = new JButton(labels.getString("s367"));
		_btnAccept.setToolTipText(labels.getString("s368"));
		_btnAccept.setHorizontalAlignment(JButton.CENTER);
		
		// CANCEL BUTTON
		_btnCancel = new JButton(labels.getString("s369"));
		_btnCancel.setToolTipText(labels.getString("s370"));
		_btnCancel.setHorizontalAlignment(JButton.CENTER);
		
		// LISTENERS
		_btnLexicalConfiguration.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {
					_undoPath = PropertiesManager.getProperty("languagePath");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				LexiconConfiguration.getInstance().setName(
						_tfName.getText());
				new LexiconGUI();
			}
		});
		_bntGrammarConfiguration.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				new GrammarGUI(false);
			}
		});
		_btnAccept.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				ResourceBundle labels = Language.getInstance().getLabels();

				String fileName = _tfName.getText();

				ValidExtensions validExtensions = ValidExtensions.getInstance();
				validExtensions.tokenizeExtensions(_tfFiles.getText());

				LexiconConfiguration programmingLanguage = LexiconConfiguration.getInstance();
				programmingLanguage.save(fileName, _rdCompiled.isSelected());

				MainWindow mainWindow = MainWindow.getInstance();
				
				if (mainWindow.getProjectGUI() != null) {
					mainWindow.getProjectGUI().setLexicalConfigurationName(programmingLanguage.getName());
					mainWindow.getProjectGUI().setLexicalConfigurationNameLabel(
							labels.getString("s599")
									+ mainWindow.getProjectGUI().getLexicalConfigurationName());
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessagelexical(
									labels.getString("s449") + " "
											+ programmingLanguage.getName());
					_logger.info(labels.getString("s371")
							+ _tfName.getText());
				}
				_frame.dispose();
			}
		});
		
		_btnCancel.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				ResourceBundle labels = Language.getInstance().getLabels();
				_logger.info(labels.getString("s372"));
				try {
					LexiconConfiguration.getInstance().load(_undoPath);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				_frame.dispose();
			}
		});

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// NAME PANEL
		constraints.fill = GridBagConstraints.NONE;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_namePanel.add(_lblName, constraints);
		constraints.ipadx = 130;
		constraints.gridx = 1;
		_namePanel.add(_tfName, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(0, 0, 0, 0);
		_frame.add(_namePanel, constraints);

		// CONFIGURATION PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_configurationPanel.add(_btnLexicalConfiguration, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 40, 5, 5);
		_configurationPanel.add(_bntGrammarConfiguration, constraints);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_configurationPanel, constraints);

		// EXTENSIONS PANEL
		_extensionsPanel.add(_lblFiles);
		_extensionsPanel.add(_tfFiles);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_frame.add(_extensionsPanel, constraints);

		// TYPE PANEL
		_typePanel.add(_rdCompiled);
		_typePanel.add(_rdInterpreted);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_frame.add(_typePanel, constraints);

		// BUTTON PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_buttonPanel.add(_btnAccept, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 50, 5, 5);
		_buttonPanel.add(_btnCancel, constraints);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 4;
		_frame.add(_buttonPanel, constraints);
		
		_frame.setVisible(true);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setLocationRelativeTo(null);
		_logger.info(labels.getString("s373"));
	}
}
