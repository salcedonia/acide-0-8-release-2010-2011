package gui.menu.project;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ResourceBundle;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import language.Language;

import operations.listeners.AcideWindowListener;
import properties.PropertiesManager;

/**
 * Execution GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ExecutionGUI extends JFrame {

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
	 * Main panel of the application.
	 */
	private JPanel _mainPanel;
	/**
	 * Execution label.
	 */
	private JLabel _executionLabel;
	/**
	 * Examine path button.
	 */
	private JButton _examinePathButton;
	/**
	 * Arguments label.
	 */
	private JLabel _argumentsLabel;
	/**
	 * Arguments text field.
	 */
	private JTextField _argumentsTextField;
	/**
	 * Run button.
	 */
	private JButton _runButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Labels to display in the selected language.
	 */
	private ResourceBundle _labels;
	/**
	 * Execution text field.
	 */
	private JTextField _executionTextField;

	/**
	 * Constructor of the class.
	 */
	public ExecutionGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		_labels = language.getLabels();
		
		// FRAME
		_frame = new JFrame(_labels.getString("s639"));
		_frame.setLayout(new GridBagLayout());
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s640")));
		_mainPanel.setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// EXECUTION
		_executionLabel = new JLabel(_labels.getString("s606"));
		_executionTextField = new JTextField();
		_executionTextField.setToolTipText(_labels.getString("s638"));
		
		_argumentsLabel = new JLabel(_labels.getString("s609"));
		_argumentsTextField = new JTextField();
		_argumentsTextField.setToolTipText(_labels.getString("s610"));
		
		// EXAMINE PATH BUTTON
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
				f.getFileChooser().addChoosableFileFilter(
						new ExtensionFilter(ExtAcide,
								"Executable source (*.exe)"));
				String path = f.read();
				_executionTextField.setText(path);
			}
		});
		
		// RUN BUTTON
		_runButton = new JButton(_labels.getString("s154"));
		_runButton.setHorizontalAlignment(JButton.CENTER);
		_runButton.setToolTipText(_labels.getString("s154"));
		_runButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				MainWindow mainWindow = MainWindow.getInstance();
				
				try {
					
					String execution = _executionTextField.getText();
					String arguments = _argumentsTextField.getText();

					if (MainWindow.getInstance().getEditorBuilder()
							.getNumEditors() > 0) {
						
						execution = execution.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath());
						execution = execution.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFilePath());
						execution = execution.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileExtension());
						execution = execution.replace("$activeFileName$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileName());

						arguments = arguments.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath());
						arguments = arguments.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFilePath());
						arguments = arguments.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileExtension());
						arguments = arguments.replace("$activeFileName$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileName());
					}

					String project = null;
					try {
						project = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					
					// IF NO OPENED PROJECT
					if ((project.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						
						// IF THERE'S ONE MAIN FILE
						if (MainWindow.getInstance().getEditorBuilder()
								.getMainEditor() != null) {
							arguments = arguments.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getAbsolutePath());
							execution = execution.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getAbsolutePath());
							arguments = arguments.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFilePath());
							execution = execution.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFilePath());
							arguments = arguments.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileExtension());
							execution = execution.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileExtension());
							arguments = arguments.replace("$mainFileName$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileName());
							execution = execution.replace("$mainFileName$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileName());
						}
					} else {
						
						// SEARCH THE MAIN FILE OPENED IN THE EDITOR
						int mainFileIndex = -1;
						for (int i = 0; i < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (mainWindow.getProjectConfiguration().getFileAt(i)
									.isMainFile())
								mainFileIndex = i;
						}
						
						// IF EXISTS
						if (mainFileIndex != -1) {
							arguments = arguments.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getPath());
							execution = execution.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getPath());
							arguments = arguments.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFilePath());
							execution = execution.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFilePath());
							arguments = arguments.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileExt());
							execution = execution.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileExt());
							arguments = arguments.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileName());
							execution = execution.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileName());
						}
					}

					Runtime.getRuntime().exec(execution + " " + arguments);
				} catch (IOException ex) {
					JOptionPane.showMessageDialog(null, ex.getMessage());
				}
				mainWindow.setEnabled(true);
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
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});
		
		// LISTENERS
		_examinePathButton.addKeyListener(new ExecutionGUIKeyboardListener());
		_runButton.addKeyListener(new ExecutionGUIKeyboardListener());
		_cancelButton.addKeyListener(new ExecutionGUIKeyboardListener());
		_executionTextField.addKeyListener(new ExecutionGUIKeyboardListener());
		_argumentsTextField.addKeyListener(new ExecutionGUIKeyboardListener());
		
		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// MAIN PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_executionLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		_mainPanel.add(_executionTextField, constraints);
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examinePathButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_argumentsLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_mainPanel.add(_argumentsTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		// BUTTON PANEL
		_buttonPanel.add(_runButton, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_mainPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_buttonPanel, constraints);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setVisible(true);
		_frame.setLocationRelativeTo(null);
		_frame.addWindowListener(new AcideWindowListener());
		
		MainWindow.getInstance();setEnabled(false);
	}

	/**
	 * Execution GUI keyboard listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ExecutionGUIKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
			}
		}
	}
}
