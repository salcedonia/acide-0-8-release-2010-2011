package gui.menuBar.projectMenu.gui;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.AcideLanguage;
import operations.listeners.AcideWindowListener;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Compiler configuration window of ACIDE - A Configurable IDE.											
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
	 * Compiler configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Compiler configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * Compiler configuration window shell name text field.
	 */
	private JTextField _shellNameTextField;
	/**
	 * Compiler configuration window examine path button.
	 */
	private JButton _examinePathButton;
	/**
     * Compiler configuration window arguments text field.
     */
	private JTextField _argumentsTextField;
	/**
     * Compiler configuration window shell name label.
     */
	private JLabel _shellNameLabel;
	/**
     * Compiler configuration window arguments label.
     */
	private JLabel _argumentsLabel;
	/**
     * Compiler configuration window configuration panel.
     */
	private JPanel _configurationPanel;
	/**
     * Compiler configuration window cptions panel.
     */
	private JPanel optionsPanel;
	/**
     * Compiler configuration window button panel.
     */
	private JPanel _buttonPanel;
	/**
     * Compiler configuration window compiler check box.
     */
	private JCheckBox _compilerCheckBox;
	/**
     * Compiler configuration window check label.
     */
	private JLabel _checkLabel;
	/**
     * Compiler configuration window separator label.
     */
	private JLabel _separatorLabel;
	/**
     * Compiler configuration window separator text field.
     */
	private JTextField _separatorTextField;
	/**
     * Compiler configuration window extension label.
     */
	private JLabel _extensionLabel;
	/**
     * Compiler configuration window extension text field.
     */
	private JTextField _extensionTextField;
	/**
     * Compiler configuration window accept button.
     */
	private JButton _acceptButton;
	/**
     * Compiler configuration window cancel button.
     */
	private JButton _cancelButton;

	/**
	 * Creates a new compiler configuration window.
	 */
	public CompilerConfigurationWindow() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		MainWindow.getInstance().setEnabled(false);
		MainWindow.getInstance().getProjectConfiguration().setCheckCompiler(false);
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s646"));
		
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// CONFIGURATION PANEL
		_configurationPanel = new JPanel(new GridBagLayout());
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s644"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		
		// OPTIONS PANEL
		optionsPanel = new JPanel(new GridBagLayout());
		optionsPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s645"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
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
				
				// Closes the window
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
				
				// Enables the main window
				MainWindow.getInstance().setEnabled(true);
				
				// Closes the window
				dispose();
			}
		});
		
		addKeyListener(new CompilerConfigurationWindowKeyboardListener());
		
		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// CONFIGURATION PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_configurationPanel.add(_shellNameLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		_configurationPanel.add(_shellNameTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_configurationPanel.add(_examinePathButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_configurationPanel.add(_argumentsLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_configurationPanel.add(_argumentsTextField, constraints);		
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_configurationPanel, constraints);
	
		// OPTION PANEL
		optionsPanel.add(_checkLabel, constraints);
		constraints.gridx = 1;
		optionsPanel.add(_compilerCheckBox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		optionsPanel.add(_separatorLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		optionsPanel.add(_separatorTextField, constraints);
		constraints.gridx = 2;
		constraints.gridy = 1;
		optionsPanel.add(_extensionLabel, constraints);
		constraints.gridx = 3;
		constraints.gridy = 1;
		optionsPanel.add(_extensionTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(optionsPanel, constraints);
			
		// BUTTON PANEL
		_buttonPanel.add(_acceptButton, constraints);
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_buttonPanel, constraints);
		
		// FRAME
		setTitle(labels.getString("s647"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		setVisible(true);
		setLocationRelativeTo(null);
		addWindowListener(new AcideWindowListener());
		
		MainWindow.getInstance().setEnabled(false);
		MainWindow.getInstance().closeDefaultProject();
	}

	/************************************************************************																
	 * Compiler configuration window keyboard listener.										
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
