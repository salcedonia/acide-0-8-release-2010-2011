package gui.menu.project.gui;

import gui.mainWindow.MainWindow;
import gui.menu.configuration.grammar.gui.GrammarConfigurationWindow;
import gui.menu.configuration.lexicon.gui.LexiconConfigurationWindow;

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

import properties.PropertiesManager;

import es.configuration.lexicon.LexiconConfiguration;
import es.text.ValidExtensions;

/************************************************************************																
 * New lexicon configuration window of ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see JFrame																													
 ***********************************************************************/
public class NewLexiconConfigurationWindow extends JFrame {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Name panel
	 */
	private JPanel _namePanel;
	/**
	 * Configuration panel
	 */
	private JPanel _configurationPanel;
	/**
	 * Extensions panel
	 */
	private JPanel _extensionsPanel;
	/**
	 * Type panel
	 */
	private JPanel _typePanel;
	/**
	 * Button panel
	 */
	private JPanel _buttonPanel;
	/**
	 * Accept button
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button
	 */
	private JButton _cancelButton;
	/**
	 * Name label
	 */
	private JLabel _nameLabel;
	/**
	 * Name text field
	 */
	private final JTextField _nameTextField; 
	/**
	 * Lexical configuration button
	 */
	private JButton _lexicalConfigurationButton;
	/**
	 * Grammar configuration button
	 */
	private JButton _grammarConfigurationButton;
	/**
	 * Files label
	 */
	private JLabel _filesLabel;
	/**
	 * Files text field
	 */
	private final JTextField _filesTextField;
	/**
	 * Compiled radio button
	 */
	private final JRadioButton _compiledRadioButton;
	/**
	 * Interpreted radio button
	 */
	private JRadioButton _interpretedRadioButton;
	/**
	 * Button group
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * Undo path
	 */
	private String _undoPath = "";

	/**
	 * Class constructor
	 */
	public NewLexiconConfigurationWindow() {

		// Gets the language
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		// Updates the log
		Log.getLog().info(labels.getString("s351"));
		
		// FRAME
		setLayout(new GridBagLayout());
		setTitle(labels.getString("s352"));
		setIconImage(new ImageIcon(ICON).getImage());

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
		_nameLabel = new JLabel(labels.getString("s357"), JLabel.CENTER);
		_nameTextField = new JTextField();
		_nameTextField.setToolTipText(labels.getString("s358"));
		
		// LEXICAL CONFIGURATION BUTTON
		_lexicalConfigurationButton = new JButton(labels.getString("s359"));
		_lexicalConfigurationButton.setHorizontalAlignment(JButton.CENTER);
		_lexicalConfigurationButton.setToolTipText(labels.getString("s360"));
		
		// GRAMMAR CONFIGURATION BUTTON
		_grammarConfigurationButton = new JButton(labels.getString("s361"));
		_grammarConfigurationButton.setToolTipText(labels.getString("s362"));
		_grammarConfigurationButton.setHorizontalAlignment(JButton.CENTER);
		
		// FILES 
		_filesLabel = new JLabel(labels.getString("s363"),
				JLabel.CENTER);
		_filesTextField = new JTextField();
		_filesTextField.setToolTipText(labels.getString("s364"));
		
		// COMPILED 
		_compiledRadioButton = new JRadioButton(
				labels.getString("s365"));
		_compiledRadioButton.setHorizontalAlignment(JRadioButton.CENTER);
		
		// INTERPRETED
		_interpretedRadioButton = new JRadioButton(
				labels.getString("s366"));
		_interpretedRadioButton.setHorizontalAlignment(JRadioButton.CENTER);
		
		// RADIO GROUP
		_buttonGroup = new ButtonGroup();
		_buttonGroup.add(_compiledRadioButton);
		_buttonGroup.add(_interpretedRadioButton);
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s367"));
		_acceptButton.setToolTipText(labels.getString("s368"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		
		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s369"));
		_cancelButton.setToolTipText(labels.getString("s370"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		
		// Listeners
		_lexicalConfigurationButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				try {
					_undoPath = PropertiesManager.getProperty("languagePath");
				} catch (Exception exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
				LexiconConfiguration.getInstance().setName(
						_nameTextField.getText());
				new LexiconConfigurationWindow();
			}
		});
		_grammarConfigurationButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				new GrammarConfigurationWindow(false);
			}
		});
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
				Language language = Language.getInstance();
				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				String fileName = _nameTextField.getText();

				ValidExtensions validExtensions = ValidExtensions.getInstance();
				validExtensions.tokenizeExtensions(_filesTextField.getText());

				// Save the configuration
				LexiconConfiguration programmingLanguage = LexiconConfiguration.getInstance();
				programmingLanguage.save(fileName, _compiledRadioButton.isSelected());
				
				// If there is an opened project window configuration
				if (MainWindow.getInstance().getProjectWindowConfiguration() != null) {
					
					// Set the lexical configuration
					MainWindow.getInstance().getProjectWindowConfiguration().setLexicalConfigurationName(programmingLanguage.getName());
					MainWindow.getInstance().getProjectWindowConfiguration().setLexicalConfigurationNameLabel(
							labels.getString("s599")
									+ MainWindow.getInstance().getProjectWindowConfiguration().getLexicalConfigurationName());
					
					// Updates the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setLexiconMessage(
									labels.getString("s449") + " "
											+ programmingLanguage.getName());
					
					// Updates the log
					Log.getLog().info(labels.getString("s371")
							+ _nameTextField.getText());
				}
				
				// Closes the configuration window
				dispose();
			}
		});
		
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// Gets the language
				Language language = Language.getInstance();
				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();
				
				// Updates the log
				Log.getLog().info(labels.getString("s372"));
				
				try {
					LexiconConfiguration.getInstance().load(_undoPath);
				} catch (Exception exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
				
				// Closes the configuration window
				dispose();
			}
		});

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// NAME PANEL
		constraints.fill = GridBagConstraints.NONE;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_namePanel.add(_nameLabel, constraints);
		constraints.ipadx = 130;
		constraints.gridx = 1;
		_namePanel.add(_nameTextField, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(0, 0, 0, 0);
		add(_namePanel, constraints);

		// CONFIGURATION PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_configurationPanel.add(_lexicalConfigurationButton, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 40, 5, 5);
		_configurationPanel.add(_grammarConfigurationButton, constraints);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_configurationPanel, constraints);

		// EXTENSIONS PANEL
		_extensionsPanel.add(_filesLabel);
		_extensionsPanel.add(_filesTextField);
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_extensionsPanel, constraints);

		// TYPE PANEL
		_typePanel.add(_compiledRadioButton);
		_typePanel.add(_interpretedRadioButton);
		constraints.gridx = 0;
		constraints.gridy = 3;
		add(_typePanel, constraints);

		// BUTTON PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_buttonPanel.add(_acceptButton, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 50, 5, 5);
		_buttonPanel.add(_cancelButton, constraints);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 4;
		add(_buttonPanel, constraints);
		
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
		
		// Updates the log
		Log.getLog().info(labels.getString("s373"));
	}
}
