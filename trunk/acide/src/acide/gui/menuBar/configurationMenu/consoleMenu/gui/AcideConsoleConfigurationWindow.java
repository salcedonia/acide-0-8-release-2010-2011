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

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.projectMenu.gui.newProjectWindow.AcideNewProjectConfigurationWindow;

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
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;

/**
 * ACIDE - A Configurable IDE console configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideConsoleConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE console configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE console configuration window main panel of the
	 * window.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE console configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE console configuration window shell path label.
	 */
	private JLabel _shellPathLabel;
	/**
	 * ACIDE - A Configurable IDE console configuration window exit command text
	 * field.
	 */
	private JLabel _exitCommandLabel;
	/**
	 * ACIDE - A Configurable IDE console configuration window shell directory
	 * label.
	 */
	private JLabel _shellDirectoryLabel;
	/**
	 * ACIDE - A Configurable IDE console configuration window shell path text
	 * field.
	 */
	private JTextField _shellPathTextField;
	/**
	 * ACIDE - A Configurable IDE console configuration window exit command text
	 * field.
	 */
	private JTextField _exitCommandTextField;
	/**
	 * ACIDE - A Configurable IDE console configuration window shell directory
	 * text field.
	 */
	private JTextField _shellDirectoryTextField;
	/**
	 * ACIDE - A Configurable IDE console configuration window manual path
	 * label.
	 */
	private JCheckBox _manualPathCheckBox;
	/**
	 * ACIDE - A Configurable IDE console configuration window echo command
	 * check box.
	 */
	private JCheckBox _echoCommandCheckBox;
	/**
	 * ACIDE - A Configurable IDE console configuration window apply button.
	 */
	private JButton _applyButton;
	/**
	 * ACIDE - A Configurable IDE console configuration window examine shell
	 * path button.
	 */
	private JButton _examineShellPathButton;
	/**
	 * ACIDE - A Configurable IDE console configuration window examine shell
	 * directory button.
	 */
	private JButton _examineShellDirectoryButton;
	/**
	 * ACIDE - A Configurable IDE console configuration window cancel button.
	 */
	private JButton _cancelButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE console configuration window.
	 */
	public AcideConsoleConfigurationWindow() {

		super();

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
						.getString("s331"));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE console window configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s334"));

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
	 * Builds the ACIDE - A Configurable IDE console configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the shell directory label
		_shellDirectoryLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s337"), JLabel.LEFT);

		// Disables the shell directory label
		_shellDirectoryLabel.setEnabled(false);

		// Creates the echo command check box
		try {

			// Creates the shell directory text field
			_shellDirectoryTextField = new JTextField(AcideResourceManager
					.getInstance().getProperty("consolePanel.shellDirectory"));

			// Sets the shell directory text field columns
			_shellDirectoryTextField.setColumns(35);

			// Disables the shell directory text field
			_shellDirectoryTextField.setEnabled(false);

			// Creates the manual path check box
			_manualPathCheckBox = new JCheckBox(AcideLanguageManager
					.getInstance().getLabels().getString("s350"));

			// Creates the shell path label
			_shellPathLabel = new JLabel(AcideLanguageManager.getInstance()
					.getLabels().getString("s338"), JLabel.LEFT);

			// Creates the shell path text field
			_shellPathTextField = new JTextField(AcideResourceManager
					.getInstance().getProperty("consolePanel.shellPath"));

			// Creates the exit command label
			_exitCommandLabel = new JLabel(AcideLanguageManager.getInstance()
					.getLabels().getString("s339"), JLabel.LEFT);

			// Creates the exit command text field
			_exitCommandTextField = new JTextField(AcideResourceManager
					.getInstance().getProperty("consolePanel.exitCommand"));

			// Sets the exit command text field columns
			_exitCommandTextField.setColumns(5);

			_echoCommandCheckBox = new JCheckBox(AcideLanguageManager
					.getInstance().getLabels().getString("s340"),
					Boolean.parseBoolean(AcideResourceManager.getInstance()
							.getProperty("consolePanel.isEchoCommand")));

			// Creates the apply button
			_applyButton = new JButton(AcideLanguageManager.getInstance()
					.getLabels().getString("s335"));

			// Sets the apply button vertical text position as center
			_applyButton.setVerticalTextPosition(AbstractButton.CENTER);

			// Sets the apply button horizontal text position as leading
			_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);

			// Sets the apply button mnemonic
			_applyButton.setMnemonic(KeyEvent.VK_A);

			// Sets the apply button tool tip text
			_applyButton.setToolTipText(AcideLanguageManager.getInstance()
					.getLabels().getString("s336"));

			// Creates the cancel button
			_cancelButton = new JButton(AcideLanguageManager.getInstance()
					.getLabels().getString("s178"));

			// Sets the cancel button vertical text position as center
			_cancelButton.setVerticalTextPosition(AbstractButton.CENTER);

			// Sets the cancel button horizontal text position as leading
			_cancelButton.setHorizontalTextPosition(AbstractButton.LEADING);

			// Creates the examine shell path button
			_examineShellPathButton = new JButton(AcideLanguageManager
					.getInstance().getLabels().getString("s142"));

			// Sets the examine shell path button tool tip text
			_examineShellPathButton.setToolTipText(AcideLanguageManager
					.getInstance().getLabels().getString("s301"));

			// Creates the examine shell directory button
			_examineShellDirectoryButton = new JButton(AcideLanguageManager
					.getInstance().getLabels().getString("s142"));

			// Sets the examine shell directory button tool tip text
			_examineShellDirectoryButton.setToolTipText(AcideLanguageManager
					.getInstance().getLabels().getString("s301"));

			// Disables the examine shell directory button
			_examineShellDirectoryButton.setEnabled(false);

		} catch (MissedPropertyException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE console
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the examine shell path label to the main panel
		_mainPanel.add(_shellPathLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 0;

		// Adds the examine shell path text field to the main panel
		_mainPanel.add(_shellPathTextField, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 2;
		constraints.gridy = 0;

		// Adds the examine shell path button to the main panel
		_mainPanel.add(_examineShellPathButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the manual path check box to the main panel
		_mainPanel.add(_manualPathCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;

		// Adds the examine shell directory label to the main panel
		_mainPanel.add(_shellDirectoryLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 2;

		// Adds the examine shell directory text field to the main panel
		_mainPanel.add(_shellDirectoryTextField, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 2;
		constraints.gridy = 2;

		// Adds the examine shell directory button to the main panel
		_mainPanel.add(_examineShellDirectoryButton, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;

		// Adds the exit command label to the main panel
		_mainPanel.add(_exitCommandLabel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 3;

		// Adds the exit command text field to the main panel
		_mainPanel.add(_exitCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;

		// Adds the echo command check box to the main panel
		_mainPanel.add(_echoCommandCheckBox, constraints);

		// Adds the main panel to the window
		add(_mainPanel, constraints);

		// Adds the apply button to the button panel
		_buttonPanel.add(_applyButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 5;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE console
	 * configuration window components.
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

		// Sets the examine shell directory button action listener
		_examineShellDirectoryButton
				.addActionListener(new ExamineShellDirectoryButtonAction());

		// Sets the examine shell path button action listener
		_examineShellPathButton
				.addActionListener(new ExamineShellPathButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
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

				// Gets the shell path from the text field
				File shellPath = new File(_shellPathTextField.getText());

				// If it is not empty
				if (!_shellPathTextField.getText().matches("")) {

					// If the selected path exists
					if (shellPath.exists()) {

						// Exits the shell
						AcideMainWindow.getInstance().getConsolePanel()
								.executeExitCommand();

						// If the shell directory is enabled
						if (_shellDirectoryTextField.isEnabled()) {

							// Sets the shell directory in the resource manager
							AcideResourceManager.getInstance().setProperty(
									"consolePanel.shellDirectory",
									_shellDirectoryTextField.getText());

							// Sets the shell directory in the project
							// configuration
							AcideProjectConfiguration.getInstance()
									.setShellDirectory(
											_shellDirectoryTextField.getText());

						} else {

							// Gets the shell directory
							String calculatedPath = "";
							String execTextField = _shellPathTextField
									.getText();
							String separator = "\\";

							int lastIndexOfSlash = execTextField
									.lastIndexOf("\\");
							if (lastIndexOfSlash == -1)
								separator = "/";
							StringTokenizer stringTokenizer = new StringTokenizer(
									execTextField, separator);

							int limit = stringTokenizer.countTokens();
							for (int index = 0; index < limit - 1; index++)
								calculatedPath = calculatedPath
										+ stringTokenizer.nextToken()
										+ separator;

							// Sets the shell directory in the resource manager
							AcideResourceManager.getInstance().setProperty(
									"consolePanel.shellDirectory",
									calculatedPath);

							// Sets the shell directory in the project
							// configuration
							AcideProjectConfiguration.getInstance()
									.setShellDirectory(calculatedPath);
						}

						// Sets the shell path in the resource manager
						AcideResourceManager.getInstance().setProperty(
								"consolePanel.shellPath",
								_shellPathTextField.getText());

						// Sets the echo command in the resource manager
						AcideResourceManager.getInstance().setProperty(
								"consolePanel.isEchoCommand",
								String.valueOf(_echoCommandCheckBox
										.isSelected()));

						// Sets the exit command in the resource manager
						AcideResourceManager.getInstance().setProperty(
								"consolePanel.exitCommand",
								_exitCommandTextField.getText());

						// Resets the console
						AcideMainWindow.getInstance().getConsolePanel()
								.resetConsole();

						// Sets the shell path in the project configuration
						AcideProjectConfiguration.getInstance().setShellPath(
								_shellPathTextField.getText());

						// Sets the echo command in the project configuration
						AcideProjectConfiguration.getInstance()
								.setIsEchoCommand(
										_echoCommandCheckBox.isSelected());

						// Sets the exit command in the project configuration
						AcideProjectConfiguration.getInstance().setExitCommand(
								_exitCommandTextField.getText());

						// If it is not the default project
						if (!AcideProjectConfiguration.getInstance()
								.isDefaultProject())

							// The project has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);

						// If the project window configuration has been
						// configured
						if (AcideNewProjectConfigurationWindow.getInstance()
								.isVisible())

							// The paths have been defined
							AcideNewProjectConfigurationWindow.getInstance()
									.setAreShellPathsDefined(true);

					} else {

						// Displays an error message
						JOptionPane.showMessageDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s993"), "Error",
								JOptionPane.ERROR_MESSAGE);
					}
				} else {

					// Exits the console
					AcideMainWindow.getInstance().getConsolePanel()
							.executeExitCommand();

					// Sets the shell directory in the resource manager
					AcideResourceManager.getInstance().setProperty(
							"consolePanel.shellDirectory",
							_shellDirectoryTextField.getText());

					// Sets the shell path in the resource manager
					AcideResourceManager.getInstance().setProperty(
							"consolePanel.shellPath",
							_shellPathTextField.getText());

					// Sets the echo command in the resource manager
					AcideResourceManager.getInstance().setProperty(
							"consolePanel.isEchoCommand",
							String.valueOf(_echoCommandCheckBox.isSelected()));

					// Sets the exit command in the resource manager
					AcideResourceManager.getInstance().setProperty(
							"consolePanel.exitCommand",
							_exitCommandTextField.getText());

					// Resets the console
					AcideMainWindow.getInstance().getConsolePanel()
							.resetConsole();

					// Sets the shell directory in the project
					// configuration
					AcideProjectConfiguration.getInstance().setShellDirectory(
							_shellDirectoryTextField.getText());

					// Sets the shell path in the project configuration
					AcideProjectConfiguration.getInstance().setShellPath(
							_shellPathTextField.getText());

					// Sets the echo command in the project configuration
					AcideProjectConfiguration.getInstance().setIsEchoCommand(
							_echoCommandCheckBox.isSelected());

					// Sets the exit command in the project configuration
					AcideProjectConfiguration.getInstance().setExitCommand(
							_exitCommandTextField.getText());

					// Not default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject())

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

					// If the project window configuration has been
					// configured
					if (AcideNewProjectConfigurationWindow.getInstance()
							.isVisible())

						// The paths have been defined
						AcideNewProjectConfigurationWindow.getInstance()
								.setAreShellPathsDefined(true);
				}
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Set the main window enabled again
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

			// Set the main window enabled again
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
	 * ACIDE - A Configurable IDE console configuration window examine shell
	 * path button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineShellPathButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Removes the filter
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.removeChoosableFileFilter(
							AcideFileManager.getInstance().getFileChooser()
									.getFileFilter());

			// Asks the the file to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenFile(true);

			if (absolutePath != null)

				// Updates the shell path text field with the absolute path
				_shellPathTextField.setText(absolutePath);
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window examine shell
	 * directory button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineShellDirectoryButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Removes the filter
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.removeChoosableFileFilter(
							AcideFileManager.getInstance().getFileChooser()
									.getFileFilter());

			// Asks the the file to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenDirectory();

			if (absolutePath != null)

				// Updates the shell directory text field with it
				_shellDirectoryTextField.setText(absolutePath);
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
				_examineShellDirectoryButton.setEnabled(true);
			} else {

				// Disables the shell directory label
				_shellDirectoryLabel.setEnabled(false);

				// Disables the shell directory text field
				_shellDirectoryTextField.setEnabled(false);

				// Disables the shell directory button
				_examineShellDirectoryButton.setEnabled(false);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE console configuration window escape key action
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

			// Set the main window enabled again
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
