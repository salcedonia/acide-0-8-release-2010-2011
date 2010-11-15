package gui.menu.project.gui;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.Language;
import operations.listeners.AcideWindowListener;
import operations.log.Log;

import properties.PropertiesManager;

/************************************************************************																
 * Compiler configuration window of ACIDE - A Configurable IDE											
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
public class CompilerConfigurationWindow extends JFrame {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Shell name text field
	 */
	private JTextField _shellNameTextField;
	/**
	 * Examine path button
	 */
	private JButton _examinePathButton;
	/**
     * Arguments text field
     */
	private JTextField _argumentsTextField;
	/**
     * Shell name label
     */
	private JLabel _shellNameLabel;
	/**
     * Arguments label
     */
	private JLabel _argumentsLabel;
	/**
     * Main panel 1
     */
	private JPanel _mainPanel1;
	/**
     * Main panel 2
     */
	private JPanel _mainPanel2;
	/**
     * Button panel
     */
	private JPanel _buttonPanel;
	/**
     * Compiler check box
     */
	private JCheckBox _compilerCheckBox;
	/**
     * Check label
     */
	private JLabel _checkLabel;
	/**
     * Separator label
     */
	private JLabel _separatorLabel;
	/**
     * Separator text field
     */
	private JTextField _separatorTextField;
	/**
     * Extension label
     */
	private JLabel _extensionLabel;
	/**
     * Extension text field
     */
	private JTextField _extensionTextField;
	/**
     * Accept button
     */
	private JButton _acceptButton;
	/**
     * Cancel button
     */
	private JButton _cancelButton;

	/**
	 * Class constructor
	 */
	public CompilerConfigurationWindow() {

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
		
		MainWindow.getInstance().setEnabled(false);
		MainWindow.getInstance().getProjectConfiguration().setCheckCompiler(false);
		
		// Updates the log
		Log.getLog().info(labels.getString("s646"));
		
		// FRAME
		setTitle(labels.getString("s647"));
		setIconImage(new ImageIcon(ICON).getImage());
		setLayout(new GridBagLayout());
		
		// MAIN PANEL 1
		_mainPanel1 = new JPanel();
		_mainPanel1.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s644"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_mainPanel1.setLayout(new GridBagLayout());
		
		// MAIN PANEL 2
		_mainPanel2 = new JPanel();
		_mainPanel2.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s645"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_mainPanel2.setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// EXEC
		_shellNameLabel = new JLabel(labels.getString("s606"));
		_shellNameTextField = new JTextField();
		_shellNameTextField.setToolTipText(labels.getString("s607"));
		
		// ARGUMENTS
		_argumentsLabel = new JLabel(labels.getString("s609"));
		_argumentsTextField = new JTextField();
		_argumentsTextField.setToolTipText(labels.getString("s610"));
		
		// CHECK
		_checkLabel = new JLabel(labels.getString("s650"));
		
		// SEPARATOR
		_separatorLabel = new JLabel(labels.getString("s649"));
		
		// EXTENSION
		_extensionLabel = new JLabel(labels.getString("s653"));
		_extensionTextField = new JTextField(7);
		_extensionTextField.setToolTipText(labels.getString("s652"));
		_extensionTextField.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getProjectConfiguration().setFileExtension(
						_extensionTextField.getText());
			}
		});
		
		// COMPILER
		_compilerCheckBox = new JCheckBox();
		_compilerCheckBox.setToolTipText(labels.getString("s650"));
		_compilerCheckBox.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
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
		_separatorTextField.setToolTipText(labels.getString("s651"));
		_separatorTextField.setEnabled(false);
		_separatorTextField.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getProjectConfiguration().setSeparatorFile(
						_separatorTextField.getText());
			}
		});
		
		// EXAMINE PATH
		_examinePathButton = new JButton(labels.getString("s596"));
		_examinePathButton.setHorizontalAlignment(JButton.CENTER);
		_examinePathButton.setToolTipText(labels.getString("s641"));
		_examinePathButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				TextFile f = new TextFile();
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser()
						.addChoosableFileFilter(
								new ExtensionFilter(ExtAcide,
										"Compiler source (*.exe)"));
				String path = f.read();
				_shellNameTextField.setText(path);
			}
		});
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s154"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		_acceptButton.setToolTipText(labels.getString("s154"));
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * 
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				MainWindow.getInstance().getProjectConfiguration().setCompilerPath(
						_shellNameTextField.getText());
				MainWindow.getInstance().getProjectConfiguration().setCompilerArguments(
						_argumentsTextField.getText());
				MainWindow.getInstance().getProjectConfiguration().setFileExtension(
						_extensionTextField.getText());
				MainWindow.getInstance().getProjectConfiguration().setSeparatorFile(
						_separatorTextField.getText());
				MainWindow.getInstance().setEnabled(true);
				
				if (MainWindow.getInstance().getProjectWindowConfiguration() != null)
					MainWindow.getInstance().getProjectWindowConfiguration().setAreCompilerPathsDefined(true);
				
				dispose();
			}
		});
		
		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(labels.getString("s162"));
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().setEnabled(true);
				dispose();
			}
		});
		
		addKeyListener(new CompilerConfigurationWindowKeyboardListener());
		
		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// MAIN PANEL 1
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel1.add(_shellNameLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		_mainPanel1.add(_shellNameTextField, constraints);
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
		add(_mainPanel1, constraints);
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
		add(_mainPanel2, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		
		add(_buttonPanel, constraints);
		setResizable(false);
		pack();
		setVisible(true);
		setLocationRelativeTo(null);
		addWindowListener(new AcideWindowListener());
		
		MainWindow.getInstance().setEnabled(false);
		MainWindow.getInstance().closeDefaultProject();
	}

	/************************************************************************																
	 * Compiler configuration window keyboard listener										
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
	 * @see KeyAdapter																													
	 ***********************************************************************/
	class CompilerConfigurationWindowKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {
			
			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
				
				MainWindow.getInstance().setEnabled(true);
				dispose();
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
