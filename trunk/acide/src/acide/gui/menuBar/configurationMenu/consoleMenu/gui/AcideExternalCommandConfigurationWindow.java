/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.gui.menuBar.configurationMenu.consoleMenu.gui;

import acide.configuration.console.AcideConsoleConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.gui.consolePanel.AcideConsolePanel;
import acide.gui.mainWindow.AcideMainWindow;

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
import java.util.StringTokenizer;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.console.AcideConsoleProcess;

/**
 * ACIDE - A Configurable IDE external command configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideExternalCommandConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE external command configuration window class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE external command configuration window image
	 * icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE external command configuration window main
	 * panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window button
	 * panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window scroll
	 * panel.
	 */
	private JScrollPane _scrollPanel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window command
	 * text area.
	 */
	private final JTextArea _commandTextArea;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * directory text field.
	 */
	private final JTextField _shellDirectoryTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * path text field.
	 */
	private final JTextField _shellPathTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window external
	 * command text field.
	 */
	private final JTextField _externalCommandTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * directory label.
	 */
	private final JLabel _shellDirectoryLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * path label.
	 */
	private JLabel _shellPathLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window external
	 * command label.
	 */
	private JLabel _externalCommandLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window echo
	 * command list.
	 */
	private JLabel _echoCommandLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window command
	 * label.
	 */
	private JLabel _commandLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window manual
	 * path label.
	 */
	private JLabel _manualPathLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window echo
	 * command text field.
	 */
	private final Checkbox _echoCommandTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window manual
	 * path text field.
	 */
	private final Checkbox _manualPathTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window apply
	 * button.
	 */
	private JButton _applyButton;
	/**
	 * ACIDE - A Configurable IDE external command configuration window cancel
	 * button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE external command configuration window examine
	 * button.
	 */
	private JButton _examinePathButton;
	/**
	 * ACIDE - A Configurable IDE external command configuration window examine
	 * 2 button.
	 */
	private JButton _examineDirectoryButton;
	/**
	 * ACIDE - A Configurable IDE external command configuration window console
	 * panel.
	 */
	private AcideConsolePanel _consolePanel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window external
	 * window.
	 */
	private JFrame _externalCommandWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE external command configuration
	 * window.
	 */
	public AcideExternalCommandConfigurationWindow() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s330"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// SHELL DIRECTORY
		_shellDirectoryLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s345"), JLabel.CENTER);
		_shellDirectoryTextField = new JTextField();
		_shellDirectoryLabel.setEnabled(false);
		_shellDirectoryTextField.setEnabled(false);

		// SHELL PATH LABEL
		_shellPathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s346"), JLabel.CENTER);
		_shellPathTextField = new JTextField();

		// EXTERNAL COMMAND
		_externalCommandLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s347"), JLabel.CENTER);
		_externalCommandTextField = new JTextField();

		// ECHO COMMAND
		_echoCommandLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s348"), JLabel.CENTER);
		_echoCommandTextField = new Checkbox();

		// MANUAL PATH
		_manualPathTextField = new Checkbox();
		_manualPathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s350"), JLabel.LEFT);

		// EXAMINE PATH BUTTON
		_examinePathButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s142"));
		_examinePathButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s301"));

		// EXAMINE DIRECTORY BUTTON
		_examineDirectoryButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s142"));
		_examineDirectoryButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s301"));
		_examineDirectoryButton.setEnabled(false);

		// COMMAND
		_commandLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s349"), JLabel.CENTER);
		_commandTextArea = new JTextArea();
		_commandTextArea.setSize(50, 50);

		// SCROLL PANE
		_scrollPanel = new JScrollPane(_commandTextArea);
		_scrollPanel.setPreferredSize(new Dimension(50, 50));

		try {
			_shellDirectoryTextField.setText(AcideConsoleConfiguration
					.getInstance().getShellDirectory());
			_shellPathTextField.setText(AcideConsoleConfiguration.getInstance()
					.getShellPath());
			_externalCommandTextField.setText(AcideConsoleConfiguration
					.getInstance().getExitCommand());
			_echoCommandTextField.setState(AcideConsoleConfiguration
					.getInstance().getIsEchoCommand());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// APPLY BUTTON
		_applyButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s343"));
		_applyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_applyButton.setMnemonic(KeyEvent.VK_A);
		_applyButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s344"));

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s178"));
		_cancelButton.setVerticalTextPosition(AbstractButton.CENTER);
		_cancelButton.setHorizontalTextPosition(AbstractButton.LEADING);

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window with the layout
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
		_mainPanel.add(_examinePathButton, constraints);

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
		_mainPanel.add(_examineDirectoryButton, constraints);

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
		_buttonPanel.add(_cancelButton);
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s342"));
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
	 * Returns the ACIDE - A Configurable IDE console panel.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel.
	 */
	public AcideConsolePanel getConsolePanel() {
		return _consolePanel;
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// APPLY BUTTON
		_applyButton.addActionListener(new ApplyButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());

		// MANUAL PATH TEXT FIELD
		_manualPathTextField.addItemListener(new ManualPathTextFieldAction());

		// EXAMINE DIRECTORY BUTTON
		_examineDirectoryButton
				.addActionListener(new ExamineDirectoryButtonAction());

		// EXAMINE PATH BUTTON
		_examinePathButton.addActionListener(new ExaminePathButtonAction());

	}

	/**
	 * 
	 * @param shellDirectoryPath
	 */
	public void executeExternalCommand(String shellDirectoryPath){
		
		// Creates the result window
		_externalCommandWindow = new JFrame(AcideLanguageManager.getInstance()
				.getLabels().getString("s342"));
		
		// Sets its icon
		_externalCommandWindow.setIconImage(ICON.getImage());
		
		// CONSOLE PANEL
		_consolePanel = new AcideConsolePanel(false);

		// Gets the command to execute from the text area
		String command = _commandTextArea.getText();
		
		// Gets the shell path from the text field
		String shellPath = _shellPathTextField.getText();

		// If there are file editor panels openeds
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {
			
			command = command
					.replace("$activeFile$", AcideMainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getAbsolutePath());
			
			shellPath = shellPath.replace("$activeFile$",
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getAbsolutePath());
			
			command = command.replace("$activeFilePath$",
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFilePath());
			
			shellPath = shellPath.replace("$activeFilePath$",
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFilePath());
			
			command = command.replace("$activeFileExt$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getFileExtension());
			
			shellPath = shellPath.replace("$activeFileExt$",
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFileExtension());
			
			command = command.replace("$activeFileName$",
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFileName());
			
			shellPath = shellPath.replace("$activeFileName$",
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFileName());
		}

		// If it is the default project
		if (AcideProjectConfiguration.getInstance()
				.isDefaultProject()) {

			// If there is an opened MAIN FILE in the editor
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getMainEditor() != null) {

				command = command.replace("$mainFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainEditor().getAbsolutePath());
				
				shellPath = shellPath.replace("$mainFile$",
						AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getMainEditor().getAbsolutePath());
				
				command = command.replace("$mainFilePath$",
						AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getMainEditor().getFilePath());
				
				shellPath = shellPath.replace("$mainFilePath$",
						AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getMainEditor().getFilePath());
				
				command = command
						.replace("$mainFileExt$", AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getMainEditor().getFileExtension());
				
				shellPath = shellPath
						.replace("$mainFileExt$", AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getMainEditor().getFileExtension());
				
				command = command.replace("$mainFileName$",
						AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getMainEditor().getFileName());
				
				shellPath = shellPath.replace("$mainFileName$",
						AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getMainEditor().getFileName());
			}
		} else {

			// Looks for an opened MAIN FILEin the editor
			int mainFileIndex = -1;
			for (int index = 0; index < AcideProjectConfiguration
					.getInstance().getNumberOfFilesFromList(); index++) {
				if (AcideProjectConfiguration.getInstance()
						.getFileAt(index).isMainFile())
					mainFileIndex = index;
			}

			// If exists
			if (mainFileIndex != -1) {

				command = command.replace("$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getAbsolutePath());
				
				shellPath = shellPath.replace("$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getAbsolutePath());
				
				command = command.replace("$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getRelativePath());
				
				shellPath = shellPath.replace("$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getRelativePath());
				
				command = command.replace("$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getFileExtension());
				
				shellPath = shellPath.replace("$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getFileExtension());
				
				command = command.replace("$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getFileName());
				
				shellPath = shellPath.replace("$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex)
								.getFileName());
			}
		}

		// Creates a new console thread
		AcideConsoleProcess consoleThread = new AcideConsoleProcess();

		// Executes the command
		consoleThread.executeCommand(shellPath, shellDirectoryPath, command,
				_externalCommandTextField.getText(), _consolePanel);

		// Adds the console panel to the window
		_externalCommandWindow.add(_consolePanel);
		
		// Sets the result window size
		_externalCommandWindow.setSize(new Dimension(300, 400));
		
		// Shows the result window
		_externalCommandWindow.setVisible(true);
	}
	
	/**
	 * ACIDE - A Configurable IDE console configuration window apply button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ApplyButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			try {

				String shellDirectoryPath = "";

				// Gets the shell directory path
				File path = new File(_shellDirectoryTextField.getText());

				// Checks if the shell exists or not
				if (path.exists()) {
					
					if (_shellDirectoryTextField.isEnabled()) {
						shellDirectoryPath = _shellDirectoryTextField.getText();
					} else {

						// Calculates the shell path
						String calculatedPath = "";
						String execTextField = _shellPathTextField.getText();
						String separator = "\\";

						int index = execTextField.lastIndexOf("\\");
						if (index == -1)
							separator = "/";
						StringTokenizer stringTokenizer = new StringTokenizer(
								execTextField, separator);

						int limit = stringTokenizer.countTokens();
						for (int i = 0; i < limit - 1; i++)
							calculatedPath = calculatedPath
									+ stringTokenizer.nextToken() + separator;

						shellDirectoryPath = calculatedPath;
					}

					// Executes the command in a new window
					executeExternalCommand(shellDirectoryPath);
					
				} else
					// Error message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s993"),
							"Error", JOptionPane.ERROR_MESSAGE);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Closes the window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window examine path
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExaminePathButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Asks the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askAbsolutePath();
			_shellPathTextField.setText(absolutePath);
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window examine directory
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineDirectoryButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Asks the path to the user
			String path = AcideFileManager.getInstance().askAbsolutePath();
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			index++;
			path = path.substring(0, index + 1);
			_shellDirectoryTextField.setText(path);
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window manual path text
	 * field item listener.
	 * 
	 * @version 0.8
	 * @see ItemListener
	 */
	class ManualPathTextFieldAction implements ItemListener {

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

				// Enables the shell directory label
				_shellDirectoryLabel.setEnabled(true);

				// Enables the shell directory text field
				_shellDirectoryTextField.setEnabled(true);

				// Enables the shell directory button
				_examineDirectoryButton.setEnabled(true);
			} else {

				// Disables the shell directory label
				_shellDirectoryLabel.setEnabled(false);

				// Disables the shell directory text field
				_shellDirectoryTextField.setEnabled(false);

				// Disables the shell directory button
				_examineDirectoryButton.setEnabled(false);
			}
		}
	}
}