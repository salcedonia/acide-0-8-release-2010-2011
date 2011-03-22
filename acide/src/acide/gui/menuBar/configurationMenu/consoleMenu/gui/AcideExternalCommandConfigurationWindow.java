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
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

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
	private JScrollPane _commandScrollPanel;
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
	 * ACIDE - A Configurable IDE external command configuration window command
	 * label.
	 */
	private JLabel _commandLabel;
	/**
	 * ACIDE - A Configurable IDE external command configuration window echo
	 * command check box.
	 */
	private final Checkbox _echoCommandCheckBox;
	/**
	 * ACIDE - A Configurable IDE external command configuration window manual
	 * path check box.
	 */
	private final Checkbox _manualPathCheckBox;
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

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);
		
		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the shell directory label
		_shellDirectoryLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s345"), JLabel.CENTER);

		// Creates the shell directory text field
		_shellDirectoryTextField = new JTextField();

		// Disables the shell directory label
		_shellDirectoryLabel.setEnabled(false);

		// Disables the shell directory text field
		_shellDirectoryTextField.setEnabled(false);

		// Creates the shell path label
		_shellPathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s346"), JLabel.CENTER);

		// Creates the shell path text field
		_shellPathTextField = new JTextField();

		// Creates the external command label
		_externalCommandLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s347"), JLabel.CENTER);

		// Creates the external command text field
		_externalCommandTextField = new JTextField();

		// Creates the echo command check box
		_echoCommandCheckBox = new Checkbox(AcideLanguageManager.getInstance()
				.getLabels().getString("s348"));

		// Creates the manual path check box
		_manualPathCheckBox = new Checkbox(AcideLanguageManager.getInstance()
				.getLabels().getString("s350"));

		// Creates the examine path button
		_examinePathButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s142"));
		
		// Sets the examine path button tool tip text
		_examinePathButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s301"));

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
		
		// Sets the command text area size
		_commandTextArea.setSize(50, 50);

		// Creates the scroll pane which contains the command text area
		_commandScrollPanel = new JScrollPane(_commandTextArea);
		
		// Sets its preferred size
		_commandScrollPanel.setPreferredSize(new Dimension(50, 50));

		try {
			_shellDirectoryTextField.setText(AcideConsoleConfiguration
					.getInstance().getShellDirectory());
			_shellPathTextField.setText(AcideConsoleConfiguration.getInstance()
					.getShellPath());
			_externalCommandTextField.setText(AcideConsoleConfiguration
					.getInstance().getExitCommand());
			_echoCommandCheckBox.setState(AcideConsoleConfiguration
					.getInstance().getIsEchoCommand());
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

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

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		
		// Adds the shell path label to the main panel
		_mainPanel.add(_shellPathLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.ipadx = 50;
		
		// Adds the shell path text field to the main panel
		_mainPanel.add(_shellPathTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examinePathButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		
		// Adds the manual path check box to the main panel
		_mainPanel.add(_manualPathCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		
		// Adds the shell directory label to the main panel
		_mainPanel.add(_shellDirectoryLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the shell directory text field to the main panel
		_mainPanel.add(_shellDirectoryTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		
		// Adds the examine directory button to the main panel
		_mainPanel.add(_examineDirectoryButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		
		// Adds the external command label to the main panel
		_mainPanel.add(_externalCommandLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the external command text field to the main panel
		_mainPanel.add(_externalCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		
		// Adds the echo command check box to the main panel
		_mainPanel.add(_echoCommandCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 5;
		
		// Adds the command label to the main panel
		_mainPanel.add(_commandLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridheight = 2;
		constraints.ipady = 60;
		
		// Adds the command scroll panel to the main panel
		_mainPanel.add(_commandScrollPanel, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridheight = 1;
		
		// Adds the main panel to the window
		add(_mainPanel, constraints);

		// Adds the apply button to the button panel
		_buttonPanel.add(_applyButton);
		
		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
		
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 1;
		
		// Adds the button panel to the window
		add(_buttonPanel, constraints);

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

		// Sets the apply button action listener
		_applyButton.addActionListener(new ApplyButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the escape key is pressed the executes the escape key action
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey", KeyStroke.getKeyStroke(
						java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
		
		// Sets the manual path check box item listener
		_manualPathCheckBox.addItemListener(new ManualPathCheckBoxAction());

		// Sets the examine directory button action listener
		_examineDirectoryButton
				.addActionListener(new ExamineDirectoryButtonAction());

		// Sets the examine path button action listener
		_examinePathButton.addActionListener(new ExaminePathButtonAction());
	}

	/**
	 * Executes the external command in the shell specified by the parameter.
	 * 
	 * @param shellDirectoryPath shell directory path.
	 */
	public void executeExternalCommand(String shellDirectoryPath) {

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

			command = command.replace("$activeFile$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath());

			shellPath = shellPath.replace("$activeFile$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath());

			command = command.replace("$activeFilePath$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFilePath());

			shellPath = shellPath.replace("$activeFilePath$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFilePath());

			command = command.replace("$activeFileExt$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileExtension());

			shellPath = shellPath.replace("$activeFileExt$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileExtension());

			command = command.replace("$activeFileName$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileName());

			shellPath = shellPath.replace("$activeFileName$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileName());
		}

		// If it is the default project
		if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// If there is an opened MAIN FILE in the editor
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getMainFileEditorPanel() != null) {

				command = command.replace("$mainFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getAbsolutePath());

				shellPath = shellPath.replace("$mainFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getAbsolutePath());

				command = command.replace("$mainFilePath$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFilePath());

				shellPath = shellPath.replace("$mainFilePath$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFilePath());

				command = command.replace("$mainFileExt$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFileExtension());

				shellPath = shellPath.replace("$mainFileExt$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFileExtension());

				command = command.replace("$mainFileName$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFileName());

				shellPath = shellPath.replace("$mainFileName$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getMainFileEditorPanel().getFileName());
			}
		} else {

			// Looks for an opened MAIN FILEin the editor
			int mainFileIndex = -1;
			for (int index = 0; index < AcideProjectConfiguration.getInstance()
					.getNumberOfFilesFromList(); index++) {
				if (AcideProjectConfiguration.getInstance().getFileAt(index)
						.isMainFile())
					mainFileIndex = index;
			}

			// If exists
			if (mainFileIndex != -1) {

				command = command.replace(
						"$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getAbsolutePath());

				shellPath = shellPath.replace(
						"$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getAbsolutePath());

				command = command.replace(
						"$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getRelativePath());

				shellPath = shellPath.replace(
						"$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getRelativePath());

				command = command.replace(
						"$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileExtension());

				shellPath = shellPath.replace(
						"$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileExtension());

				command = command.replace(
						"$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileName());

				shellPath = shellPath.replace(
						"$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileIndex).getFileName());
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

			// Enables the main window again
			AcideMainWindow.getInstance().setEnabled(true);
			
			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
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

			// Enables the main window again
			AcideMainWindow.getInstance().setEnabled(true);
			
			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
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
			
			// Updates the shell path text field with the absolute path
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
			String absolutePath = AcideFileManager.getInstance().askAbsolutePath();
			int index = absolutePath.lastIndexOf("\\");
			if (index == -1)
				index = absolutePath.lastIndexOf("/");
			
			// Gets the relative path
			String path = absolutePath.substring(0, index + 1);
			
			// Updates the shell directory text field with the relative path
			_shellDirectoryTextField.setText(path);
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
	 * ACIDE - A Configurable IDE external command configuration window escape key action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the main window
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();

			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
		}
	}
}