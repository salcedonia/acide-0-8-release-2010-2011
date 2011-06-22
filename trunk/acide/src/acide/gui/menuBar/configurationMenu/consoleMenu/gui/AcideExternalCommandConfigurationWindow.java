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

import java.awt.Checkbox;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.StringTokenizer;

import javax.swing.AbstractAction;
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
import javax.swing.JTextPane;
import javax.swing.KeyStroke;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.externalCommand.AcideExternalCommandProcess;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;

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
	private JScrollPane _commandScrollPanel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window command
	 * text area.
	 */
	private JTextArea _commandTextArea;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * directory text field.
	 */
	private JTextField _shellDirectoryTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * path text field.
	 */
	private JTextField _shellPathTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window exit
	 * command text field.
	 */
	private JTextField _exitCommandTextField;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * directory label.
	 */
	private JLabel _shellDirectoryLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window shell
	 * path label.
	 */
	private JLabel _shellPathLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window exit
	 * command label.
	 */
	private JLabel _exitCommandLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window command
	 * label.
	 */
	private JLabel _commandLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window echo
	 * command check box.
	 */
	private Checkbox _echoCommandCheckBox;
	/**
	 * ACIDE - A Configurable IDE external command configuration window manual
	 * path check box.
	 */
	private Checkbox _manualPathCheckBox;
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
	private JTextPane _textComponent;
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

		// Builds the window components
		buildComponents();

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s330"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE external command configuration
	 * window components.
	 */
	private void buildComponents() {

		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the shell directory label
		_shellDirectoryLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s345"), JLabel.CENTER);

		try {

			// Creates the shell directory text field
			_shellDirectoryTextField = new JTextField(AcideResourceManager
					.getInstance().getProperty("consolePanel.shellDirectory"));

			// Sets the shell directory text field columns
			_shellDirectoryTextField.setColumns(35);

			// Disables the shell directory label
			_shellDirectoryLabel.setEnabled(false);

			// Disables the shell directory text field
			_shellDirectoryTextField.setEnabled(false);

			// Creates the shell path label
			_shellPathLabel = new JLabel(AcideLanguageManager.getInstance()
					.getLabels().getString("s346"), JLabel.CENTER);

			// Creates the shell path text field
			_shellPathTextField = new JTextField(AcideResourceManager
					.getInstance().getProperty("consolePanel.shellPath"));

			// Creates the exit command label
			_exitCommandLabel = new JLabel(AcideLanguageManager.getInstance()
					.getLabels().getString("s347"), JLabel.CENTER);

			// Creates the exit command text field
			_exitCommandTextField = new JTextField(AcideResourceManager
					.getInstance().getProperty("consolePanel.exitCommand"));

			// Sets the exit command text field columns
			_exitCommandTextField.setColumns(5);

			// Creates the echo command check box
			_echoCommandCheckBox = new Checkbox(AcideLanguageManager
					.getInstance().getLabels().getString("s348"),
					Boolean.parseBoolean(AcideResourceManager.getInstance()
							.getProperty("consolePanel.isEchoCommand")));

			// Creates the manual path check box
			_manualPathCheckBox = new Checkbox(AcideLanguageManager
					.getInstance().getLabels().getString("s350"));

			// Creates the examine path button
			_examinePathButton = new JButton(AcideLanguageManager.getInstance()
					.getLabels().getString("s142"));

			// Sets the examine path button tool tip text
			_examinePathButton.setToolTipText(AcideLanguageManager
					.getInstance().getLabels().getString("s301"));

			// Creates the examine directory button
			_examineDirectoryButton = new JButton(AcideLanguageManager
					.getInstance().getLabels().getString("s142"));

			// Sets the examine directory button tool tip text
			_examineDirectoryButton.setToolTipText(AcideLanguageManager
					.getInstance().getLabels().getString("s301"));

			// Disables the examine directory button
			_examineDirectoryButton.setEnabled(false);

			// Creates the command label
			_commandLabel = new JLabel(AcideLanguageManager.getInstance()
					.getLabels().getString("s349"), JLabel.CENTER);

			// Creates the command text area
			_commandTextArea = new JTextArea();

			// Sets the command text area rows
			_commandTextArea.setRows(4);

			// Creates the scroll pane which contains the command text area
			_commandScrollPanel = new JScrollPane(_commandTextArea);

			// Creates the apply button
			_applyButton = new JButton(AcideLanguageManager.getInstance()
					.getLabels().getString("s343"));

			// Sets the accept button vertical text position as center
			_applyButton.setVerticalTextPosition(AbstractButton.CENTER);

			// Sets the accept button horizontal text position as leading
			_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);

			// Sets the apply button mnemonic
			_applyButton.setMnemonic(KeyEvent.VK_A);

			// Sets the apply button tool tip text
			_applyButton.setToolTipText(AcideLanguageManager.getInstance()
					.getLabels().getString("s344"));

			// Creates the cancel button
			_cancelButton = new JButton(AcideLanguageManager.getInstance()
					.getLabels().getString("s178"));

			// Sets the cancel button vertical text position as center
			_cancelButton.setVerticalTextPosition(AbstractButton.CENTER);

			// Sets the cancel button horizontal text position as leading
			_cancelButton.setHorizontalTextPosition(AbstractButton.LEADING);

			// Sets the cancel button tool tip text
			_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
					.getLabels().getString("s178"));

			// Adds the apply button to the button panel
			_buttonPanel.add(_applyButton);

			// Adds the cancel button to the button panel
			_buttonPanel.add(_cancelButton);

		} catch (MissedPropertyException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE external command
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);

		// Adds the shell path label to the main panel
		_mainPanel.add(_shellPathLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 1;
		constraints.gridy = 0;

		// Adds the shell path text field to the main panel
		_mainPanel.add(_shellPathTextField, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 2;
		constraints.gridy = 0;

		// Adds the examine path button to the main panel
		_mainPanel.add(_examinePathButton, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridwidth = 3;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the manual path check box to the main panel
		_mainPanel.add(_manualPathCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 2;

		// Adds the shell directory label to the main panel
		_mainPanel.add(_shellDirectoryLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 1;
		constraints.gridy = 2;

		// Adds the shell directory text field to the main panel
		_mainPanel.add(_shellDirectoryTextField, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 2;
		constraints.gridy = 2;

		// Adds the examine directory button to the main panel
		_mainPanel.add(_examineDirectoryButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 3;

		// Adds the exit command label to the main panel
		_mainPanel.add(_exitCommandLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 3;

		// Adds the exit command text field to the main panel
		_mainPanel.add(_exitCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridwidth = 3;
		constraints.gridx = 0;
		constraints.gridy = 4;

		// Adds the echo command check box to the main panel
		_mainPanel.add(_echoCommandCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 5;

		// Adds the command label to the main panel
		_mainPanel.add(_commandLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 1;
		constraints.gridwidth = 2;
		constraints.gridheight = 2;

		// Adds the command scroll panel to the main panel
		_mainPanel.add(_commandScrollPanel, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.BOTH;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.gridheight = 1;

		// Adds the main panel to the window
		add(_mainPanel, constraints);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE external command configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s342"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Displays the window
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel.
	 */
	public JTextPane getConsolePanel() {
		return _textComponent;
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE external command
	 * configuration window components.
	 */
	private void setListeners() {

		// Sets the apply button action listener
		_applyButton.addActionListener(new ApplyButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the manual path check box item listener
		_manualPathCheckBox.addItemListener(new ManualPathCheckBoxAction());

		// Sets the examine directory button action listener
		_examineDirectoryButton
				.addActionListener(new ExamineDirectoryButtonAction());

		// Sets the examine path button action listener
		_examinePathButton.addActionListener(new ExaminePathButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());

		// Puts the escape key in the input map of the window
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false),
				"EscapeKey");

		// Puts the escape key in the action map of the window
		getRootPane().getActionMap().put("EscapeKey", new EscapeKeyAction());
	}

	/**
	 * Executes the external command in the shell specified by the parameter.
	 * 
	 * @param shellDirectoryPath
	 *            shell directory path.
	 */
	public void executeExternalCommand(String shellDirectoryPath) {

		// Creates the result window
		_externalCommandWindow = new JFrame(AcideLanguageManager.getInstance()
				.getLabels().getString("s342"));

		// Sets its icon
		_externalCommandWindow.setIconImage(ICON.getImage());

		// Creates the text component
		_textComponent = new JTextPane();

		// Sets the text component as not editable
		_textComponent.setEditable(false);

		// Sets the text component font
		_textComponent.setFont(new Font("Monospaced", Font.PLAIN, 12));
		
		// Gets the command to execute from the text area
		String command = _commandTextArea.getText();

		// Gets the shell path from the text field
		String shellPath = _shellPathTextField.getText();

		// If there are file editor panels opened
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Replaces the $activeFile$ variable for its real value
			command = command.replace("$activeFile$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath());

			// Replaces the $activeFile$ variable for its real value
			shellPath = shellPath.replace("$activeFile$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath());

			// Replaces the $activeFilePath$ variable for its real value
			command = command.replace("$activeFilePath$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFilePath());

			// Replaces the $activeFilePath$ variable for its real value
			shellPath = shellPath.replace("$activeFilePath$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFilePath());

			// Replaces the $activeFileExt$ variable for its real value
			command = command.replace("$activeFileExt$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileExtension());

			// Replaces the $activeFileExt$ variable for its real value
			shellPath = shellPath.replace("$activeFileExt$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileExtension());

			// Replaces the $activeFileName$ variable for its real value
			command = command
					.replace("$activeFileName$", AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFileNameWithoutExtension());

			// Replaces the $activeFileName$ variable for its real value
			shellPath = shellPath
					.replace("$activeFileName$", AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFileNameWithoutExtension());
		}

		// If it is the default project
		if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// If there is an opened MAIN FILE in the editor
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getMainFileEditorPanel() != null) {

				// Replaces the $mainFile$ variable for its real value
				command = command.replace("$mainFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getAbsolutePath());

				// Replaces the $mainFile$ variable for its real value
				shellPath = shellPath.replace("$mainFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getAbsolutePath());

				// Replaces the $mainFilePath$ variable for its real value
				command = command.replace("$mainFilePath$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFilePath());

				// Replaces the $mainFilePath$ variable for its real value
				shellPath = shellPath.replace("$mainFilePath$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFilePath());

				// Replaces the $mainFileExt$ variable for its real value
				command = command.replace("$mainFileExt$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFileExtension());

				// Replaces the $mainFileExt$ variable for its real value
				shellPath = shellPath.replace("$mainFileExt$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFileExtension());

				// Replaces the $mainFileName$ variable for its real value
				command = command
						.replace("$mainFileName$", AcideMainWindow
								.getInstance().getFileEditorManager()
								.getMainFileEditorPanel()
								.getFileNameWithoutExtension());

				// Replaces the $mainFileName$ variable for its real value
				shellPath = shellPath
						.replace("$mainFileName$", AcideMainWindow
								.getInstance().getFileEditorManager()
								.getMainFileEditorPanel()
								.getFileNameWithoutExtension());
			}
		} else {

			// Looks for an opened MAIN FILE in the file editor
			int mainFileIndex = -1;
			for (int index = 0; index < AcideProjectConfiguration.getInstance()
					.getNumberOfFilesFromList(); index++) {
				if (AcideProjectConfiguration.getInstance().getFileAt(index)
						.isMainFile())
					mainFileIndex = index;
			}

			// If exists
			if (mainFileIndex != -1) {

				// Replaces the $mainFile$ variable for its real value
				command = command.replace(
						"$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getAbsolutePath());

				// Replaces the $mainFile$ variable for its real value
				shellPath = shellPath.replace(
						"$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getAbsolutePath());

				// Replaces the $mainFilePath$ variable for its real value
				command = command.replace(
						"$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getRelativePath());

				// Replaces the $mainFilePath$ variable for its real value
				shellPath = shellPath.replace(
						"$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getRelativePath());

				// Replaces the $mainFileExt$ variable for its real value
				command = command.replace(
						"$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileExtension());

				// Replaces the $mainFileExt$ variable for its real value
				shellPath = shellPath.replace(
						"$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileExtension());

				// Replaces the $mainFileName$ variable for its real value
				command = command.replace(
						"$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileName());

				// Replaces the $mainFileName$ variable for its real value
				shellPath = shellPath.replace(
						"$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileName());
			} else {

				// Gets the main file editor panel
				AcideFileEditorPanel mainFileEditorPanel = AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel();

				// If exists
				if (mainFileEditorPanel != null) {

					// Replaces the $mainFile$ variable for its real
					// value
					command = command.replace("$mainFile$",
							mainFileEditorPanel.getAbsolutePath());

					// Replaces the $mainFile$ variable for its real
					// value
					shellPath = shellPath.replace("$mainFile$",
							mainFileEditorPanel.getAbsolutePath());

					// Replaces the $mainFilePath$ variable for its real
					// value
					command = command.replace("$mainFilePath$",
							mainFileEditorPanel.getFilePath());

					// Replaces the $mainFilePath$ variable for its real
					// value
					shellPath = shellPath.replace("$mainFilePath$",
							mainFileEditorPanel.getFilePath());

					// Replaces the $mainFileExt$ variable for its real
					// value
					command = command.replace("$mainFileExt$",
							mainFileEditorPanel.getFileExtension());

					// Replaces the $mainFileExt$ variable for its real
					// value
					shellPath = shellPath.replace("$mainFileExt$",
							mainFileEditorPanel.getFileExtension());

					// Replaces the $mainFileName$ variable for its real
					// value
					command = command.replace("$mainFileName$",
							mainFileEditorPanel.getFileNameWithoutExtension());

					// Replaces the $mainFileName$ variable for its real
					// value
					shellPath = shellPath.replace("$mainFileName$",
							mainFileEditorPanel.getFileNameWithoutExtension());
				}
			}
		}

		// Adds the console panel to the window
		_externalCommandWindow.add(new JScrollPane(_textComponent));

		// Sets the result window size
		_externalCommandWindow.setSize(new Dimension(450, 500));

		// Brings the window to the front
		_externalCommandWindow.setAlwaysOnTop(true);

		// Centers the window
		_externalCommandWindow.setLocationRelativeTo(null);

		// Shows the result window
		_externalCommandWindow.setVisible(true);

		// Creates a new external command process
		AcideExternalCommandProcess externalCommandThread = new AcideExternalCommandProcess(
				_shellPathTextField.getText(),
				_shellDirectoryTextField.getText(), _textComponent);

		// Executes the command
		externalCommandThread.executeCommand(shellPath, shellDirectoryPath, command,
				_exitCommandTextField.getText(), _textComponent);
	}

	/**
	 * Closes the ACIDE - A Configurable IDE external command configuration
	 * window.
	 */
	private void closeWindow() {

		// Enables the main window again
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
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
					// Displays an error message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s993"),
							"Error", JOptionPane.ERROR_MESSAGE);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Closes the window
			closeWindow();
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
			closeWindow();
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

			// Asks for the file path to the user
			String absolutePath = AcideFileManager.getInstance().askForFile(
					AcideFileOperation.OPEN, AcideFileTarget.FILES,
					AcideFileType.FILE, "", null);

			if (absolutePath != null) {

				// Updates the shell path text field with the absolute
				// path
				_shellPathTextField.setText(absolutePath);
			}
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

			// Asks for the file path to the user
			String absolutePath = AcideFileManager.getInstance().askForFile(
					AcideFileOperation.OPEN, AcideFileTarget.FILES,
					AcideFileType.DIRECTORY, "", null);

			if (absolutePath != null) {

				// Updates the shell directory text field with the
				// relative path
				_shellDirectoryTextField.setText(absolutePath);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window manual path check
	 * box item listener.
	 * 
	 * @version 0.8
	 * @see ItemListener
	 */
	class ManualPathCheckBoxAction implements ItemListener {

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

	/**
	 * ACIDE - A Configurable IDE external command configuration window escape
	 * key action.
	 * 
	 * @version 0.8
	 * @see AbstractAction
	 */
	class EscapeKeyAction extends AbstractAction {

		/**
		 * Escape key action serial version UID.
		 */
		private static final long serialVersionUID = 1L;

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
			closeWindow();
		}
	}
}