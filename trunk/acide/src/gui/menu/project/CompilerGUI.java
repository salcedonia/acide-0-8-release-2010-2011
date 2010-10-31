package gui.menu.project;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import language.Language;
import operations.listeners.AcideWindowListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * Compiler GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class CompilerGUI extends JFrame {

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
	 * Exec name text field.
	 */
	private JTextField _execNameTextField;
	/**
	 * Examine path button.
	 */
	private JButton _examinePathButton;
	/**
     * Arguments text field.
     */
	private JTextField _argumentsTextField;
	/**
     * Exec name label.
     */
	private JLabel _execNameLabel;
	/**
     * Arguments label.
     */
	private JLabel _argumentsLabel;
	/**
     * Main panel 1.
     */
	private JPanel _mainPanel1;
	/**
     * Main panel 2.
     */
	private JPanel _mainPanel2;
	/**
     * Button panel.
     */
	private JPanel _buttonPanel;
	/**
     * Compiler check box.
     */
	private JCheckBox _compilerCheckBox;
	/**
     * Check label
     */
	private JLabel _checkLabel;
	/**
     * Separator label.
     */
	private JLabel _separatorLabel;
	/**
     * Separator text field.
     */
	private JTextField _separatorTextField;
	/**
     * Extension label.
     */
	private JLabel _extensionLabel;
	/**
     * Extension text field.
     */
	private JTextField _extensionTextField;
	/**
     * Accept button.
     */
	private JButton _acceptButton;
	/**
     * Cancel button.
     */
	private JButton _cancelButton;
	/**
     * Log of the class.
     */
	private Logger _logger = Log.getLog();
	/**
	 * Labels to display in the selected language.
	 */
	private ResourceBundle _labels;

	/**
	 * Constructor of the class.
	 */
	public CompilerGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		_labels = language.getLabels();
		
		// GET THE MAIN WINDOW
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		mainWindow.getProjectConfiguration().setCheckCompiler(false);
		_logger.info(_labels.getString("s646"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setLayout(new GridBagLayout());
		_frame.setTitle(_labels.getString("s647"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());
		
		// MAIN PANEL 1
		_mainPanel1 = new JPanel();
		_mainPanel1.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s644")));
		_mainPanel1.setLayout(new GridBagLayout());
		
		// MAIN PANEL 2
		_mainPanel2 = new JPanel();
		_mainPanel2.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s645")));
		_mainPanel2.setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// EXEC
		_execNameLabel = new JLabel(_labels.getString("s606"));
		_execNameTextField = new JTextField();
		_execNameTextField.setToolTipText(_labels.getString("s607"));
		
		// ARGUMENTS
		_argumentsLabel = new JLabel(_labels.getString("s609"));
		_argumentsTextField = new JTextField();
		_argumentsTextField.setToolTipText(_labels.getString("s610"));
		
		// CHECK
		_checkLabel = new JLabel(_labels.getString("s650"));
		
		// SEPARATOR
		_separatorLabel = new JLabel(_labels.getString("s649"));
		
		// EXTENSION
		_extensionLabel = new JLabel(_labels.getString("s653"));
		_extensionTextField = new JTextField(7);
		_extensionTextField.setToolTipText(_labels.getString("s652"));
		_extensionTextField.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
			
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getProjectConfiguration().setFileExtension(
						_extensionTextField.getText());
			}
		});
		
		// COMPILER
		_compilerCheckBox = new JCheckBox();
		_compilerCheckBox.setToolTipText(_labels.getString("s650"));
		_compilerCheckBox.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				if (_compilerCheckBox.isSelected()) {
					MainWindow.getInstance().getProjectConfiguration().setCheckCompiler(true);
					_extensionTextField.setText("");
					_extensionTextField.setEnabled(false);
					_separatorTextField.setEnabled(true);
				} else {
					MainWindow.getInstance().getProjectConfiguration().setCheckCompiler(false);
					_separatorTextField.setText("");
					_separatorTextField.setEnabled(false);
					_extensionTextField.setEnabled(true);
				}
			}
		});
		
		// SEPARATOR
		_separatorTextField = new JTextField(1);
		_separatorTextField.setToolTipText(_labels.getString("s651"));
		_separatorTextField.setEnabled(false);
		_separatorTextField.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow.getInstance().getProjectConfiguration().setSeparatorFile(
						_separatorTextField.getText());
			}
		});
		
		// EXAMINE PATH
		_examinePathButton = new JButton(_labels.getString("s596"));
		_examinePathButton.setHorizontalAlignment(JButton.CENTER);
		_examinePathButton.setToolTipText(_labels.getString("s641"));
		_examinePathButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile f = new TextFile();
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser()
						.addChoosableFileFilter(
								new ExtensionFilter(ExtAcide,
										"Compiler source (*.exe)"));
				String path = f.read();
				_execNameTextField.setText(path);
			}
		});
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(_labels.getString("s154"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		_acceptButton.setToolTipText(_labels.getString("s154"));
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * 
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				MainWindow.getInstance().getProjectConfiguration().setCompilerPath(
						_execNameTextField.getText());
				MainWindow.getInstance().getProjectConfiguration().setCompilerArguments(
						_argumentsTextField.getText());
				MainWindow.getInstance().getProjectConfiguration().setFileExtension(
						_extensionTextField.getText());
				MainWindow.getInstance().getProjectConfiguration().setSeparatorFile(
						_separatorTextField.getText());
				MainWindow.getInstance().setEnabled(true);
				
				if (MainWindow.getInstance().getProjectGUI() != null)
					MainWindow.getInstance().getProjectGUI().setAreCompilerPathsDefined(true);
				
				_frame.dispose();
			}
		});
		
		// CANCEL BUTTON
		_cancelButton = new JButton(_labels.getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(_labels.getString("s162"));
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow.getInstance().setEnabled(true);
				_frame.dispose();
			}
		});
		
		_frame.addKeyListener(new CompilerGUIKeyboardListener());
		
		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// MAIN PANEL 1
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel1.add(_execNameLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		_mainPanel1.add(_execNameTextField, constraints);
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel1.add(_examinePathButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel1.add(_argumentsLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_mainPanel1.add(_argumentsTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_mainPanel1, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		
		// MAIN PANEL 2
		_mainPanel2.add(_checkLabel, constraints);
		constraints.gridx = 1;
		_mainPanel2.add(_compilerCheckBox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel2.add(_separatorLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		_mainPanel2.add(_separatorTextField, constraints);
		constraints.gridx = 2;
		constraints.gridy = 1;
		_mainPanel2.add(_extensionLabel, constraints);
		constraints.gridx = 3;
		constraints.gridy = 1;
		_mainPanel2.add(_extensionTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_mainPanel2, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		
		_frame.add(_buttonPanel, constraints);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setVisible(true);
		_frame.setLocationRelativeTo(null);
		_frame.addWindowListener(new AcideWindowListener());
		
		mainWindow.setEnabled(false);
		mainWindow.closeDefaultProject();
	}

	/**
	 * Compiler GUI keyboard listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class CompilerGUIKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		public void keyPressed(KeyEvent evt) {
			
			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
				MainWindow.getInstance().setEnabled(true);
				_frame.dispose();
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
