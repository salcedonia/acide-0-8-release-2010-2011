package gui.menu.configuration.output;

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
 * 
 */
public class ExternalCommandGUI {

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
	private JPanel _panel;
	/**
	 * 
	 */
	private Output _output;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JButton _btnAplicate;
	/**
	 * 
	 */
	private final JLabel _lblExecPath;
	/**
	 * 
	 */
	private final JTextField _tfExecPath;
	/**
	 * 
	 */
	private JLabel _lblExec;
	/**
	 * 
	 */
	private final JTextField _tfExec;
	/**
	 * 
	 */
	private JLabel _lblExternalCommand;
	/**
	 * 
	 */
	private final JTextField _tfExternalCommand;
	/**
	 * 
	 */
	private JLabel _lblEchoCommand;
	/**
	 * 
	 */
	private final Checkbox _tfEchoCommand;
	/**
	 * 
	 */
	private final Checkbox _tfManualPath;
	/**
	 * 
	 */
	private JLabel _lblManualPath;
	/**
	 * 
	 */
	private JButton _btnExamine;
	/**
	 * 
	 */
	private JButton _btnExamine2;
	/**
	 * 
	 */
	private JLabel _lblCommand;
	/**
	 * 
	 */
	private final JTextArea _taCommand;
	/**
	 * 
	 */
	private JScrollPane _scrollPanel;
	/**
	 * 
	 */
	private ResourceBundle _labels;
	
	/**
	 * Constructor of the class.
	 */
	public ExternalCommandGUI() {

		Language _language = Language.getInstance();
		try {
			_language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		_labels = _language.getLabels();

		_logger.info(_labels.getString("s330"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s342"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		
		// PANEL
		_panel = new JPanel();
		_panel.setLayout(new GridBagLayout());
		
		
		// EXEC PATH
		_lblExecPath = new JLabel(_labels.getString("s345"),
				JLabel.CENTER);
		_tfExecPath = new JTextField();
		_lblExecPath.setEnabled(false);
		_tfExecPath.setEnabled(false);
		
		// EXEC
		_lblExec = new JLabel(_labels.getString("s346"), JLabel.CENTER);
		_tfExec = new JTextField();
		
		// EXTERNAL COMMAND
		_lblExternalCommand = new JLabel(_labels.getString("s347"),
				JLabel.CENTER);
		_tfExternalCommand = new JTextField();
		
		// ECHO COMMAND
		_lblEchoCommand = new JLabel(_labels.getString("s348"), JLabel.CENTER);
		_tfEchoCommand = new Checkbox();
		
		// MANUAL PATH
		_tfManualPath = new Checkbox();
		_lblManualPath = new JLabel(_labels.getString("s350"),
				JLabel.LEFT);
		
		// EXAMINE BUTTON
		_btnExamine = new JButton(_labels.getString("s142"));
		_btnExamine.setToolTipText(_labels.getString("s301"));
		_btnExamine.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile f = new TextFile();
				String path = f.read();
				_tfExec.setText(path);
			}
		});
		
		// EXAMINE BUTTON 2
		_btnExamine2 = new JButton(_labels.getString("s142"));
		_btnExamine2.setToolTipText(_labels.getString("s301"));
		_btnExamine2.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile f = new TextFile();
				String path = f.read();
				int index = path.lastIndexOf("\\");
				path = path.substring(0, index + 1);
				_tfExecPath.setText(path);
			}
		});
		_btnExamine2.setEnabled(false);
		
		_tfManualPath.addItemListener(new ItemListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
			 */
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					_lblExecPath.setEnabled(true);
					_tfExecPath.setEnabled(true);
					_btnExamine2.setEnabled(true);
				} else {
					_lblExecPath.setEnabled(false);
					_tfExecPath.setEnabled(false);
					_btnExamine2.setEnabled(false);
				}
			}
		});

		// COMMAND
		_lblCommand = new JLabel(_labels.getString("s349"), JLabel.CENTER);
		_taCommand = new JTextArea();
		_taCommand.setSize(50, 50);
		
		// SCROLL PANE
		_scrollPanel = new JScrollPane(_taCommand);
		_scrollPanel.setPreferredSize(new Dimension(50, 50));

		try {
			_tfExecPath.setText(PropertiesManager.getProperty("execPath"));
			_tfExec.setText(PropertiesManager.getProperty("exec"));
			_tfExternalCommand.setText(PropertiesManager
					.getProperty("exitCommand"));
			_tfEchoCommand.setState(Boolean.parseBoolean(PropertiesManager
					.getProperty("echoCommand")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// APLICATE BUTTON
		_btnAplicate = new JButton(_labels.getString("s343"));
		_btnAplicate.setVerticalTextPosition(AbstractButton.CENTER);
		_btnAplicate.setHorizontalTextPosition(AbstractButton.LEADING);
		_btnAplicate.setMnemonic(KeyEvent.VK_A);
		_btnAplicate.setToolTipText(_labels.getString("s344"));
		_btnAplicate.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {
					String execPath = "";
					if (_tfExecPath.isEnabled()) {
						execPath = _tfExecPath.getText();
					} else {
						String calculedPath = "";
						String saux = _tfExec.getText();
						StringTokenizer st = new StringTokenizer(saux, "\\");
						int limite = st.countTokens();
						for (int i = 0; i < limite - 1; i++) {
							calculedPath = calculedPath + st.nextToken()
									+ "\\";
						}
						execPath = calculedPath;
					}

					ProcessThread thread = new ProcessThread();
					JFrame result = new JFrame();
					result.setTitle(_labels.getString("s342"));
					result.setIconImage(new ImageIcon(ICON).getImage());
					_output = new Output(false);
					String command = _taCommand.getText();
					String commandAux = _tfExec.getText();
					if (MainWindow.getInstance().getEditorBuilder()
							.getNumEditors() > 0) {
						command = command.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getPath());
						commandAux = commandAux.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getPath());
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
					String prj = null;
					try {
						prj = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						// No project
						if (MainWindow.getInstance().getEditorBuilder()
								.getMainEditor() != null) {
							command = command.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getPath());
							commandAux = commandAux.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getPath());
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
						} else {
							// No main file
						}
					} else {// Yes project
						int j = -1;
						for (int i = 0; i < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (mainWindow.getProjectConfiguration().getFile(i)
									.isMainFile())
								j = i;
						}
						if (j != -1) {
							command = command.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getPath());
							commandAux = commandAux.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getPath());
							command = command.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFilePath());
							commandAux = commandAux.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFilePath());
							command = command.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileExt());
							commandAux = commandAux.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileExt());
							command = command.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileName());
							commandAux = commandAux.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileName());
						} else {
							// No main file
						}
					}

					System.out.println(commandAux + " " + execPath + " " + command);

					thread.executeCommand(commandAux, execPath, command,
							_tfExternalCommand.getText(), _output);

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

		_btnAplicate.registerKeyboardAction(actionListener, "EscapeKey", KeyStroke
				.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_panel.add(_lblExec, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 50;
		_panel.add(_tfExec, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_panel.add(_btnExamine, constraints);

		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_panel.add(_lblManualPath, constraints);
		constraints.gridx = 1;
		_panel.add(_tfManualPath, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;
		_panel.add(_lblExecPath, constraints);
		constraints.gridx = 1;
		_panel.add(_tfExecPath, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_panel.add(_btnExamine2, constraints);

		constraints.gridx = 0;
		constraints.gridy = 3;
		_panel.add(_lblExternalCommand, constraints);
		constraints.gridx = 1;
		_panel.add(_tfExternalCommand, constraints);

		constraints.gridx = 0;
		constraints.gridy = 4;
		_panel.add(_lblEchoCommand, constraints);
		constraints.gridx = 1;
		_panel.add(_tfEchoCommand, constraints);

		constraints.gridx = 0;
		constraints.gridy = 5;
		_panel.add(_lblCommand, constraints);
		constraints.gridx = 1;
		constraints.gridheight = 2;
		constraints.ipady = 60;
		_panel.add(_scrollPanel, constraints);

		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 7;
		constraints.gridheight = 1;
		_panel.add(_btnAplicate, constraints);

		_frame.add(_panel);
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
	 * 
	 * @return
	 */
	public Output getOutput() {
		return _output;
	}
}