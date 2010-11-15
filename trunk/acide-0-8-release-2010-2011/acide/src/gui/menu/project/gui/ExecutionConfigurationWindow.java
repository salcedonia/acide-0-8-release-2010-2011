package gui.menu.project.gui;

import java.awt.Color;
import java.awt.Font;
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
import gui.mainWindow.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.Language;

import operations.listeners.AcideWindowListener;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Execution configuration window of ACIDE - A Configurable IDE											
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
public class ExecutionConfigurationWindow extends JFrame {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Main panel
	 */
	private JPanel _mainPanel;
	/**
	 * Execution label
	 */
	private JLabel _executionLabel;
	/**
	 * Examine path button
	 */
	private JButton _examinePathButton;
	/**
	 * Arguments label
	 */
	private JLabel _argumentsLabel;
	/**
	 * Arguments text field
	 */
	private JTextField _argumentsTextField;
	/**
	 * Run button
	 */
	private JButton _runButton;
	/**
	 * Cancel button
	 */
	private JButton _cancelButton;
	/**
	 * Button panel
	 */
	private JPanel _buttonPanel;
	/**
	 * Execution text field
	 */
	private JTextField _executionTextField;

	/**
	 * Class constructor
	 */
	public ExecutionConfigurationWindow() {

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
		
		// FRAME
		setTitle(labels.getString("s639"));
		setLayout(new GridBagLayout());
		setIconImage(new ImageIcon(ICON).getImage());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s640"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_mainPanel.setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// EXECUTION
		_executionLabel = new JLabel(labels.getString("s606"));
		_executionTextField = new JTextField();
		_executionTextField.setToolTipText(labels.getString("s638"));
		
		_argumentsLabel = new JLabel(labels.getString("s609"));
		_argumentsTextField = new JTextField();
		_argumentsTextField.setToolTipText(labels.getString("s610"));
		
		// EXAMINE PATH BUTTON
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
				f.getFileChooser().addChoosableFileFilter(
						new ExtensionFilter(ExtAcide,
								"Executable source (*.exe)"));
				String path = f.read();
				_executionTextField.setText(path);
			}
		});
		
		// RUN BUTTON
		_runButton = new JButton(labels.getString("s154"));
		_runButton.setHorizontalAlignment(JButton.CENTER);
		_runButton.setToolTipText(labels.getString("s154"));
		_runButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				try {
					
					String execution = _executionTextField.getText();
					String arguments = _argumentsTextField.getText();

					if (MainWindow.getInstance().getEditorManager()
							.getNumEditors() > 0) {
						
						execution = execution.replace("$activeFile$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getAbsolutePath());
						execution = execution.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getFilePath());
						execution = execution.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getFileExtension());
						execution = execution.replace("$activeFileName$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getFileName());

						arguments = arguments.replace("$activeFile$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getAbsolutePath());
						arguments = arguments.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getFilePath());
						arguments = arguments.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getFileExtension());
						arguments = arguments.replace("$activeFileName$", MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getFileName());
					}
					
					// DEFAULT PROJECT
					if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
						
						// IF THERE'S ONE MAIN FILE
						if (MainWindow.getInstance().getEditorManager()
								.getMainEditor() != null) {
							arguments = arguments.replace("$mainFile$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getAbsolutePath());
							execution = execution.replace("$mainFile$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getAbsolutePath());
							arguments = arguments.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getFilePath());
							execution = execution.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getFilePath());
							arguments = arguments.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getFileExtension());
							execution = execution.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getFileExtension());
							arguments = arguments.replace("$mainFileName$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getFileName());
							execution = execution.replace("$mainFileName$", MainWindow
									.getInstance().getEditorManager()
									.getMainEditor().getFileName());
						}
					} else {
						
						// SEARCH THE MAIN FILE OPENED IN THE EDITOR
						int mainFileIndex = -1;
						for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i)
									.isMainFile())
								mainFileIndex = i;
						}
						
						// IF EXISTS
						if (mainFileIndex != -1) {
							arguments = arguments.replace("$mainFile$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getPath());
							execution = execution.replace("$mainFile$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getPath());
							arguments = arguments.replace("$mainFilePath$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFilePath());
							execution = execution.replace("$mainFilePath$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFilePath());
							arguments = arguments.replace("$mainFileExt$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileExt());
							execution = execution.replace("$mainFileExt$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileExt());
							arguments = arguments.replace("$mainFileName$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileName());
							execution = execution.replace("$mainFileName$", MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(mainFileIndex)
									.getFileName());
						}
					}

					Runtime.getRuntime().exec(execution + " " + arguments);
				} catch (IOException exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					
					// Error message
					JOptionPane.showMessageDialog(null, exception.getMessage());
				}
				
				MainWindow.getInstance().setEnabled(true);
				
				// Closes the configuration window
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
		
		// Listeners
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
		add(_mainPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);
		setResizable(false);
		pack();
		setVisible(true);
		setLocationRelativeTo(null);
		addWindowListener(new AcideWindowListener());
		
		MainWindow.getInstance().setEnabled(false);
	}

	/************************************************************************																
	 * Execution configuration window keyboard listener										
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
	class ExecutionGUIKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {
			
			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
				
				MainWindow.getInstance().setEnabled(true);
				
				// Closes the configuration window
				dispose();
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
