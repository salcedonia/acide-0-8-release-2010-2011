package gui.menu.configuration.output;

import es.configuration.output.OutputConfiguration;
import es.text.TextFile;
import gui.MainWindow;
import gui.output.Output;

import java.awt.Checkbox;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import language.Language;
import operations.log.Log;
import operations.output.ProcessThread;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * Main class of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ExternalCommandGUI {

	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
	/**
	 * Main panel of the window.
	 */
	private JPanel _mainPanel;
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
	private Output _output;
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
	public ExternalCommandGUI() {

		// GET THE LANGUAGE
		Language _language = Language.getInstance();
		
		try {
			_language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		_labels = _language.getLabels();

		_logger.info(_labels.getString("s330"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s342"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		
		// PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());	
		
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
		_manualPathLabel = new JLabel(_labels.getString("s350"),
				JLabel.LEFT);
		
		// EXAMINE BUTTON
		_examineButton = new JButton(_labels.getString("s142"));
		_examineButton.setToolTipText(_labels.getString("s301"));
		_examineButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
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
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
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
			 * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
			 */
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
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
			_shellDirectoryTextField.setText(OutputConfiguration.getInstance().getShellDirectory());
			_shellPathTextField.setText(OutputConfiguration.getInstance().getShellPath());
			_externalCommandTextField.setText(PropertiesManager
					.getProperty("exitCommand"));
			_echoCommandTextField.setState(Boolean.parseBoolean(PropertiesManager
					.getProperty("echoCommand")));
		} catch (Exception e) {
			e.printStackTrace();
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
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {
					
					String execPath = "";
					
					if (_shellDirectoryTextField.isEnabled()) {
						execPath = _shellDirectoryTextField.getText();
					} else {
						String calculedPath = "";
						String saux = _shellPathTextField.getText();
						StringTokenizer st = new StringTokenizer(saux, "\\");
						int limite = st.countTokens();
						for (int i = 0; i < limite - 1; i++) {
							calculedPath = calculedPath + st.nextToken()
									+ "\\";
						}
						execPath = calculedPath;
					}

					ProcessThread thread = new ProcessThread();
					
					// SHOWS A NEW OUTPUT WINDOW WITH THE RESULT OF THE COMMAND EXECUTION
					JFrame result = new JFrame();
					result.setTitle(_labels.getString("s342"));
					result.setIconImage(new ImageIcon(ICON).getImage());
					_output = new Output(false);
					
					String command = _commandTextArea.getText();
					String commandAux = _shellPathTextField.getText();
					
					if (MainWindow.getInstance().getEditorBuilder()
							.getNumEditors() > 0) {
						command = command.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath());
						commandAux = commandAux.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath());
						command = command.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFilePath());
						commandAux = commandAux.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFilePath());
						command = command.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileExtension());
						commandAux = commandAux.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileExtension());
						command = command.replace("$activeFileName$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileName());
						commandAux = commandAux.replace("$activeFileName$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileName());
					}

					MainWindow mainWindow = MainWindow.getInstance();
										
					// DEFAULT PROJECT
					if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
						
						// IF THERE IS AN OPENED MAIN FAIL IN THE EDITOR
						if (MainWindow.getInstance().getEditorBuilder()
								.getMainEditor() != null) {
							
							command = command.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getAbsolutePath());
							commandAux = commandAux.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getAbsolutePath());
							command = command.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFilePath());
							commandAux = commandAux.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFilePath());
							command = command.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileExtension());
							commandAux = commandAux.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileExtension());
							command = command.replace("$mainFileName$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileName());
							commandAux = commandAux.replace("$mainFileName$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileName());
						}
					} else {
						
						// LOOK FOR AN OPENED MAIN FILE IN THE EDITOR
						int posMainFile = -1;
						for (int i = 0; i < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (mainWindow.getProjectConfiguration().getFileAt(i)
									.isMainFile())
								posMainFile = i;
						}
						
						// IF EXISTS
						if (posMainFile != -1) {
							
							command = command.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getPath());
							commandAux = commandAux.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getPath());
							command = command.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getFilePath());
							commandAux = commandAux.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getFilePath());
							command = command.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getFileExt());
							commandAux = commandAux.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getFileExt());
							command = command.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getFileName());
							commandAux = commandAux.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFileAt(posMainFile)
									.getFileName());
						}
					}

					// EXECUTES THE COMMAND IN A DIFFERENT THREAD
					thread.executeCommand(commandAux, execPath, command,
							_externalCommandTextField.getText(), _output);

					result.add(_output);
					result.setSize(new Dimension(300, 400));
					result.setVisible(true);
					
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				_frame.dispose();
			}
		});

		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};

		_applyButton.registerKeyboardAction(actionListener, "EscapeKey", KeyStroke
				.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_shellPathLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 50;
		_mainPanel.add(_shellPathTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examineButton, constraints);

		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathLabel, constraints);
		constraints.gridx = 1;
		_mainPanel.add(_manualPathTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_shellDirectoryLabel, constraints);
		constraints.gridx = 1;
		_mainPanel.add(_shellDirectoryTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examine2Button, constraints);

		constraints.gridx = 0;
		constraints.gridy = 3;
		_mainPanel.add(_externalCommandLabel, constraints);
		constraints.gridx = 1;
		_mainPanel.add(_externalCommandTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandLabel, constraints);
		constraints.gridx = 1;
		_mainPanel.add(_echoCommandTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 5;
		_mainPanel.add(_commandLabel, constraints);
		constraints.gridx = 1;
		constraints.gridheight = 2;
		constraints.ipady = 60;
		_mainPanel.add(_scrollPanel, constraints);

		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 7;
		constraints.gridheight = 1;
		_mainPanel.add(_applyButton, constraints);

		_frame.add(_mainPanel);
		_frame.setResizable(false);
		_frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = _frame.getSize();
		_frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		_frame.setVisible(true);
		_frame.setLocationRelativeTo(null);
	}

	/**
	 * Returns the output.
	 * 
	 * @return The output.
	 */
	public Output getOutput() {
		return _output;
	}
}