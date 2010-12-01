package gui.menuBar.configurationMenu.outputMenu.gui;

import es.configuration.output.OutputConfiguration;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.outputPanel.AcideOutputPanel;

import java.awt.Checkbox;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
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
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import language.AcideLanguage;
import operations.log.AcideLog;
import operations.output.OutputThread;
import resources.ResourceManager;


/************************************************************************																
 * External command configuration window of ACIDE - A Configurable IDE.
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
public class ExternalCommandConfigurationWindow extends JFrame{

	/**
	 * External command configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * External command configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * Main panel of the window.
	 */
	private JPanel _mainPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Scroll panel.
	 */
	private JScrollPane _scrollPanel;
	/**
	 * Command text area.
	 */
	private final JTextArea _commandTextArea;
	/**
	 * Shell directory text field.
	 */
	private final JTextField _shellDirectoryTextField;
	/**
	 * Shell path text field.
	 */
	private final JTextField _shellPathTextField;
	/**
	 * External command text field.
	 */
	private final JTextField _externalCommandTextField;
	/**
	 * Shell directory label.
	 */
	private final JLabel _shellDirectoryLabel;
	/**
	 * Shell path label.
	 */
	private JLabel _shellPathLabel;
	/**
	 * External command label.
	 */
	private JLabel _externalCommandLabel;
	/**
	 * Echo command list.
	 */
	private JLabel _echoCommandLabel;
	/**
	 * Command label.
	 */
	private JLabel _commandLabel;
	/**
	 * Manual path label.
	 */
	private JLabel _manualPathLabel;
	/**
	 * Echo command text field.
	 */
	private final Checkbox _echoCommandTextField;
	/**
	 * Manual path text field.
	 */
	private final Checkbox _manualPathTextField;
	/**
	 * Apply button.
	 */
	private JButton _applyButton;
	/**
	 * Examine button.
	 */
	private JButton _examineButton;
	/**
	 * Examine 2 button.
	 */
	private JButton _examine2Button;
	/**
	 * Output of the application.
	 */
	private AcideOutputPanel _output;
	/**
	 * Labels to display in the selected language.
	 */
	private ResourceBundle _labels;

	/**
	 * Class constructor.
	 */
	public ExternalCommandConfigurationWindow() {

		// Gets the language
		AcideLanguage _language = AcideLanguage.getInstance();

		try {
			_language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		_labels = _language.getLabels();

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s330"));

		// Sets the layout
		setLayout(new GridBagLayout());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
		// SHELL DIRECTORY
		_shellDirectoryLabel = new JLabel(_labels.getString("s345"),
				JLabel.CENTER);
		_shellDirectoryTextField = new JTextField();
		_shellDirectoryLabel.setEnabled(false);
		_shellDirectoryTextField.setEnabled(false);

		// SHELL PATH LABEL
		_shellPathLabel = new JLabel(_labels.getString("s346"), JLabel.CENTER);
		_shellPathTextField = new JTextField();

		// EXTERNAL COMMAND
		_externalCommandLabel = new JLabel(_labels.getString("s347"),
				JLabel.CENTER);
		_externalCommandTextField = new JTextField();

		// ECHO COMMAND
		_echoCommandLabel = new JLabel(_labels.getString("s348"), JLabel.CENTER);
		_echoCommandTextField = new Checkbox();

		// MANUAL PATH
		_manualPathTextField = new Checkbox();
		_manualPathLabel = new JLabel(_labels.getString("s350"), JLabel.LEFT);

		// EXAMINE BUTTON
		_examineButton = new JButton(_labels.getString("s142"));
		_examineButton.setToolTipText(_labels.getString("s301"));
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
				TextFile f = new TextFile();
				String path = f.read();
				_shellPathTextField.setText(path);
			}
		});

		// EXAMINE BUTTON 2
		_examine2Button = new JButton(_labels.getString("s142"));
		_examine2Button.setToolTipText(_labels.getString("s301"));
		_examine2Button.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile f = new TextFile();
				String path = f.read();
				int index = path.lastIndexOf("\\");
				if (index == -1)
					index = path.lastIndexOf("/");
				index++;
				path = path.substring(0, index + 1);
				_shellDirectoryTextField.setText(path);
			}
		});
		_examine2Button.setEnabled(false);

		_manualPathTextField.addItemListener(new ItemListener() {
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

		// COMMAND
		_commandLabel = new JLabel(_labels.getString("s349"), JLabel.CENTER);
		_commandTextArea = new JTextArea();
		_commandTextArea.setSize(50, 50);

		// SCROLL PANE
		_scrollPanel = new JScrollPane(_commandTextArea);
		_scrollPanel.setPreferredSize(new Dimension(50, 50));

		try {
			_shellDirectoryTextField.setText(OutputConfiguration.getInstance()
					.getShellDirectory());
			_shellPathTextField.setText(OutputConfiguration.getInstance()
					.getShellPath());
			_externalCommandTextField.setText(OutputConfiguration.getInstance()
					.getExitCommand());
			_echoCommandTextField.setState(OutputConfiguration.getInstance()
					.getEchoCommand());
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// APPLY BUTTON
		_applyButton = new JButton(_labels.getString("s343"));
		_applyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_applyButton.setMnemonic(KeyEvent.VK_A);
		_applyButton.setToolTipText(_labels.getString("s344"));
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

					// Get the labels
					ResourceBundle labels = AcideLanguage.getInstance().getLabels();

					String execPath = "";

					File path = new File(_shellDirectoryTextField.getText());

					// CHECK OF THE SHELL EXISTS OR NOT
					if (path.exists()) {
						if (_shellDirectoryTextField.isEnabled()) {
							execPath = _shellDirectoryTextField.getText();
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
							
							execPath = calculatedPath;
						}

						OutputThread thread = new OutputThread();

						// SHOWS A NEW OUTPUT WINDOW WITH THE RESULT OF THE
						// COMMAND EXECUTION
						JFrame result = new JFrame();
						result.setTitle(_labels.getString("s342"));
						result.setIconImage(ICON.getImage());
						_output = new AcideOutputPanel(false);

						String command = _commandTextArea.getText();
						String commandAux = _shellPathTextField.getText();

						if (MainWindow.getInstance().getFileEditorManager()
								.getNumFileEditorPanels() > 0) {
							command = command.replace("$activeFile$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());
							commandAux = commandAux.replace("$activeFile$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());
							command = command.replace("$activeFilePath$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel().getFilePath());
							commandAux = commandAux.replace("$activeFilePath$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel().getFilePath());
							command = command.replace("$activeFileExt$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getFileExtension());
							commandAux = commandAux.replace("$activeFileExt$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getFileExtension());
							command = command.replace("$activeFileName$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel().getFileName());
							commandAux = commandAux.replace("$activeFileName$",
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel().getFileName());
						}

						// DEFAULT PROJECT
						if (MainWindow.getInstance().getProjectConfiguration()
								.isDefaultProject()) {

							// IF THERE IS AN OPENED MAIN FAIL IN THE EDITOR
							if (MainWindow.getInstance().getFileEditorManager()
									.getMainEditor() != null) {

								command = command.replace("$mainFile$",
										MainWindow.getInstance()
												.getFileEditorManager()
												.getMainEditor()
												.getAbsolutePath());
								commandAux = commandAux.replace("$mainFile$",
										MainWindow.getInstance()
												.getFileEditorManager()
												.getMainEditor()
												.getAbsolutePath());
								command = command.replace("$mainFilePath$",
										MainWindow.getInstance()
												.getFileEditorManager()
												.getMainEditor().getFilePath());
								commandAux = commandAux.replace(
										"$mainFilePath$", MainWindow
												.getInstance()
												.getFileEditorManager()
												.getMainEditor().getFilePath());
								command = command.replace("$mainFileExt$",
										MainWindow.getInstance()
												.getFileEditorManager()
												.getMainEditor()
												.getFileExtension());
								commandAux = commandAux.replace(
										"$mainFileExt$", MainWindow
												.getInstance()
												.getFileEditorManager()
												.getMainEditor()
												.getFileExtension());
								command = command.replace("$mainFileName$",
										MainWindow.getInstance()
												.getFileEditorManager()
												.getMainEditor().getFileName());
								commandAux = commandAux.replace(
										"$mainFileName$", MainWindow
												.getInstance()
												.getFileEditorManager()
												.getMainEditor().getFileName());
							}
						} else {

							// LOOK FOR AN OPENED MAIN FILE IN THE EDITOR
							int posMainFile = -1;
							for (int i = 0; i < MainWindow.getInstance()
									.getProjectConfiguration()
									.getNumFilesFromList(); i++) {
								if (MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(i)
										.isMainFile())
									posMainFile = i;
							}

							// IF EXISTS
							if (posMainFile != -1) {

								command = command.replace("$mainFile$",
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getPath());
								commandAux = commandAux.replace("$mainFile$",
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getPath());
								command = command.replace("$mainFilePath$",
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getFilePath());
								commandAux = commandAux.replace(
										"$mainFilePath$", MainWindow
												.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getFilePath());
								command = command.replace("$mainFileExt$",
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getFileExt());
								commandAux = commandAux.replace(
										"$mainFileExt$", MainWindow
												.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getFileExt());
								command = command.replace("$mainFileName$",
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getFileName());
								commandAux = commandAux.replace(
										"$mainFileName$", MainWindow
												.getInstance()
												.getProjectConfiguration()
												.getFileAt(posMainFile)
												.getFileName());
							}
						}

						// EXECUTES THE COMMAND IN A DIFFERENT THREAD
						thread.executeCommand(commandAux, execPath, command,
								_externalCommandTextField.getText(), _output);

						result.add(_output);
						result.setSize(new Dimension(300, 400));
						result.setVisible(true);
						
					} else
						// Error message
						JOptionPane.showMessageDialog(null,
								labels.getString("s993"), "Error",
								JOptionPane.ERROR_MESSAGE);
				} catch (Exception exception) {
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
				
				// Closes the window
				dispose();
			}
		});

		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// Closes the window
				dispose();
			}
		};

		_applyButton.registerKeyboardAction(actionListener, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_shellPathLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.ipadx = 50;
		_mainPanel.add(_shellPathTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examineButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 1;
		_mainPanel.add(_manualPathTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_shellDirectoryLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_mainPanel.add(_shellDirectoryTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examine2Button, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		_mainPanel.add(_externalCommandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_mainPanel.add(_externalCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_mainPanel.add(_echoCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 5;
		_mainPanel.add(_commandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridheight = 2;
		constraints.ipady = 60;
		_mainPanel.add(_scrollPanel, constraints);
		
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridheight = 1;
		add(_mainPanel, constraints);
		
		// BUTTON PANEL
		_buttonPanel.add(_applyButton);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);
		
		// FRAME
		setTitle(_labels.getString("s342"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		
		// Centers the window
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		setVisible(true);
		setLocationRelativeTo(null);
	}

	/**
	 * Returns the output.
	 * 
	 * @return The output.
	 */
	public AcideOutputPanel getOutput() {
		return _output;
	}
}