package gui.menuBar.configurationMenu.outputMenu.gui;

import es.configuration.output.OutputConfiguration;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;


/************************************************************************																
 * Output configuration window of ACIDE - A Configurable IDE.
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
public class OutputConfigurationWindow extends JFrame {

	/**
	 * Output configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Output configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * Output configuration window main panel of the window.
	 */
	private JPanel _mainPanel;
	/**
	 * Output configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Output configuration window echo command label.
	 */
	private JLabel _echoCommandLabel;
	/**
	 * Output configuration window shell path label.
	 */
	private JLabel _shellPathLabel;
	/**
	 * Output configuration window exit command text field.
	 */
	private JLabel _exitCommandLabel;
	/**
	 * Output configuration window shell directory label.
	 */
	private final JLabel _shellDirectoryLabel;
	/**
	 * Output configuration window shell path text field.
	 */
	private final JTextField _shellPathTextField;
	/**
	 * Output configuration window exit command text field.
	 */
	private final JTextField _exitCommandTextField;
	/**
	 * Output configuration window shell directory text field.
	 */
	private final JTextField _shellDirectoryTextField;
	/**
	 * Output configuration window manual path label.
	 */
	private JLabel _manualPathLabel;
	/**
	 * Output configuration window manual path label.
	 */
	private final JCheckBox _manualPathCheckBox;
	/**
	 * Output configuration window echo command check box.
	 */
	private final JCheckBox _echoCommandCheckBox;
	/**
	 * Output configuration window apply button.
	 */
	private JButton _applyButton;
	/**
	 * Output configuration window examine button.
	 */
	private JButton _examineButton;
	/**
	 * Output configuration window examine 2 button.
	 */
	private final JButton _examine2Button;
	/**
	 * Output configuration window cancel button.
	 */
	private JButton _cancelButton;

	/**
	 * Creates a new output configuration window.
	 */
	public OutputConfigurationWindow() {

		super();

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

		// Updates the log
		AcideLog.getLog().info(labels.getString("s331"));

		// Sets the layout
		setLayout(new GridBagLayout());
		
		// PANEL
		_mainPanel = new JPanel(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
		// SHELL DIRECTORY
		_shellDirectoryLabel = new JLabel(labels.getString("s337"), JLabel.LEFT);
		_shellDirectoryTextField = new JTextField();
		_shellDirectoryLabel.setEnabled(false);
		_shellDirectoryTextField.setEnabled(false);

		// MANUAL PATH
		_manualPathLabel = new JLabel(labels.getString("s350"), JLabel.LEFT);
		_manualPathCheckBox = new JCheckBox();

		// SHELL PATH
		_shellPathLabel = new JLabel(labels.getString("s338"), JLabel.LEFT);
		_shellPathTextField = new JTextField();

		// EXIT COMMAND
		_exitCommandLabel = new JLabel(labels.getString("s339"), JLabel.LEFT);
		_exitCommandTextField = new JTextField();
		_exitCommandTextField.setColumns(10);

		// ECHO COMMAND
		_echoCommandLabel = new JLabel(labels.getString("s340"), JLabel.LEFT);
		_echoCommandCheckBox = new JCheckBox();

		// SET THE VALUES INTO THE TEXT FIELDS
		try {

			// SHELL PATH
			if (OutputConfiguration.getInstance().getShellPath()
					.matches("null"))
				_shellPathTextField.setText("");
			else
				_shellPathTextField.setText(OutputConfiguration.getInstance()
						.getShellPath());

			// SHELL DIRECTORY
			if (OutputConfiguration.getInstance().getShellDirectory()
					.matches("null"))
				_shellDirectoryTextField.setText("");
			else
				_shellDirectoryTextField.setText(OutputConfiguration
						.getInstance().getShellDirectory());

			// EXIT COMMAND
			if (OutputConfiguration.getInstance().getExitCommand()
					.matches("null"))
				_exitCommandTextField.setText("null");
			else
				_exitCommandTextField.setText(OutputConfiguration.getInstance()
						.getExitCommand());

			// ECHO COMMAND
			_echoCommandCheckBox.setSelected(OutputConfiguration.getInstance()
					.getEchoCommand());

		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// APPLY BUTTON
		_applyButton = new JButton(labels.getString("s335"));
		_applyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_applyButton.setMnemonic(KeyEvent.VK_A);
		_applyButton.setToolTipText(labels.getString("s336"));

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s178"));
		_cancelButton.setVerticalTextPosition(AbstractButton.CENTER);
		_cancelButton.setHorizontalTextPosition(AbstractButton.LEADING);
		
		// EXAMINE BUTTON
		_examineButton = new JButton(labels.getString("s142"));
		_examineButton.setToolTipText(labels.getString("s301"));

		// EXAMINE2 BUTTON
		_examine2Button = new JButton(labels.getString("s142"));
		_examine2Button.setToolTipText(labels.getString("s301"));

		// Listeners
		_examineButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				TextFile file = new TextFile();
				String path = file.read();
				_shellPathTextField.setText(path);
			}
		});

		_examine2Button.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				String path = "";

				JFileChooser fileChooser = new JFileChooser();
				fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

				int option = fileChooser.showOpenDialog(null);

				if (option == JFileChooser.APPROVE_OPTION)
					path = fileChooser.getSelectedFile().getAbsolutePath();

				_shellDirectoryTextField.setText(path);
			}
		});
		_examine2Button.setEnabled(false);

		_manualPathCheckBox.addItemListener(new ItemListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent
			 * )
			 */
			@Override
			public void itemStateChanged(ItemEvent itemEvent) {
				
				if (itemEvent.getStateChange() == ItemEvent.SELECTED) {
					_shellDirectoryLabel.setEnabled(true);
					_shellDirectoryTextField.setEnabled(true);
					_examine2Button.setEnabled(true);
				} else {
					_shellDirectoryLabel.setEnabled(false);
					_shellDirectoryTextField.setEnabled(false);
					_examine2Button.setEnabled(false);
				}
			}
		});

		_applyButton.addActionListener(new ActionListener() {
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
					
					// Gets the labels
					ResourceBundle labels = AcideLanguage.getInstance().getLabels();

					// Exits the output
					MainWindow.getInstance().getOutput().executeExitCommand();

					File path = new File(_shellPathTextField.getText());

					// If the selected path exists
					if (path.exists()) {

						// SHELL DIRECTORY
						if (_shellDirectoryTextField.isEnabled()) {

							OutputConfiguration.getInstance()
									.setShellDirectory(
											_shellDirectoryTextField.getText());
						} else {

							String calculatedPath = "";
							String execTextField = _shellPathTextField
									.getText();
							String separator = "\\";

							int index = execTextField.lastIndexOf("\\");
							if (index == -1)
								separator = "/";
							StringTokenizer stringTokenizer = new StringTokenizer(
									execTextField, separator);

							int limit = stringTokenizer.countTokens();
							for (int i = 0; i < limit - 1; i++)
								calculatedPath = calculatedPath
										+ stringTokenizer.nextToken()
										+ separator;

							OutputConfiguration.getInstance()
									.setShellDirectory(calculatedPath);
						}
						
						// ECHO COMMAND
						OutputConfiguration.getInstance().setEchoCommand(
								_echoCommandCheckBox.isSelected());

						// EXIT COMMAND
						OutputConfiguration.getInstance().setExitCommand(
								_exitCommandTextField.getText());
						
						// Sets the shell path
						OutputConfiguration.getInstance().setShellPath(
								_shellPathTextField.getText());
						
						// Resets the output
						MainWindow.getInstance().getOutput().resetOutput();

					} else {

						// Shows an error message
						JOptionPane.showMessageDialog(null,
								labels.getString("s993"), "Error",
								JOptionPane.ERROR_MESSAGE);

						// Sets the shell path to null
						OutputConfiguration.getInstance().setShellPath("null");
						
						// ECHO COMMAND
						OutputConfiguration.getInstance().setEchoCommand(
								_echoCommandCheckBox.isSelected());

						// EXIT COMMAND
						OutputConfiguration.getInstance().setExitCommand(
								_exitCommandTextField.getText());
					}

					// Updates the RESOURCE MANAGER
					ResourceManager.getInstance().setProperty("outputConfiguration",
							"./configuration/output/configuration.xml");
					OutputConfiguration.getInstance().save();

					// Not default project
					if (!MainWindow.getInstance().getProjectConfiguration()
							.isDefaultProject())

						// The project has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);

					// If the project window configuration has been configured
					if (MainWindow.getInstance().getProjectWindowConfiguration() != null)
						
						// The paths have been defined
						MainWindow.getInstance().getProjectWindowConfiguration()
								.setAreShellPathsDefined(true);

				} catch (Exception exception) {
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
								
				// Closes the window
				dispose();
			}
		});

		_cancelButton.addActionListener(new ActionListener(){
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
					
				// Closes the window
				dispose();	
			}	
		});
		
		// Sets the components with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;

		// SHELL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_shellPathLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.ipadx = 150;
		constraints.gridy = 0;
		_mainPanel.add(_shellPathTextField, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.gridy = 0;
		_mainPanel.add(_examineButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_shellDirectoryLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.ipadx = 150;
		constraints.gridx = 1;
		constraints.gridy = 2;
		_mainPanel.add(_shellDirectoryTextField, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.gridy = 2;
		_mainPanel.add(_examine2Button, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		_mainPanel.add(_exitCommandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 3;
		_mainPanel.add(_exitCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandCheckBox, constraints);
		add(_mainPanel, constraints);
		
		// BUTTON PANEL
		_buttonPanel.add(_applyButton);
		_buttonPanel.add(_cancelButton);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 5;
		add(_buttonPanel, constraints);

		// FRAME
		setIconImage(ICON.getImage());
		setTitle(labels.getString("s334"));
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
	}
}
