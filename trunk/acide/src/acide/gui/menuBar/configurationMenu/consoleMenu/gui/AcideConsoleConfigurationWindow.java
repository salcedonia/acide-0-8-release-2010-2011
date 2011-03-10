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
import acide.gui.mainWindow.AcideMainWindow;

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
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

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
	 * ACIDE - A Configurable IDE console configuration window echo command
	 * label.
	 */
	private JLabel _echoCommandLabel;
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
	private final JLabel _shellDirectoryLabel;
	/**
	 * ACIDE - A Configurable IDE console configuration window shell path text
	 * field.
	 */
	private final JTextField _shellPathTextField;
	/**
	 * ACIDE - A Configurable IDE console configuration window exit command text
	 * field.
	 */
	private final JTextField _exitCommandTextField;
	/**
	 * ACIDE - A Configurable IDE console configuration window shell directory
	 * text field.
	 */
	private final JTextField _shellDirectoryTextField;
	/**
	 * ACIDE - A Configurable IDE console configuration window manual path
	 * label.
	 */
	private JLabel _manualPathLabel;
	/**
	 * ACIDE - A Configurable IDE console configuration window manual path
	 * label.
	 */
	private final JCheckBox _manualPathCheckBox;
	/**
	 * ACIDE - A Configurable IDE console configuration window echo command
	 * check box.
	 */
	private final JCheckBox _echoCommandCheckBox;
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
	private final JButton _examineShellDirectoryButton;
	/**
	 * ACIDE - A Configurable IDE console configuration window cancel button.
	 */
	private JButton _cancelButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE console configuration window.
	 */
	public AcideConsoleConfigurationWindow() {

		super();

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s331"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// PANEL
		_mainPanel = new JPanel(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// SHELL DIRECTORY
		_shellDirectoryLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s337"), JLabel.LEFT);
		_shellDirectoryTextField = new JTextField();
		_shellDirectoryLabel.setEnabled(false);
		_shellDirectoryTextField.setEnabled(false);

		// MANUAL PATH
		_manualPathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s350"), JLabel.LEFT);
		_manualPathCheckBox = new JCheckBox();

		// SHELL PATH
		_shellPathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s338"), JLabel.LEFT);
		_shellPathTextField = new JTextField();

		// EXIT COMMAND
		_exitCommandLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s339"), JLabel.LEFT);
		_exitCommandTextField = new JTextField();
		_exitCommandTextField.setColumns(10);

		// ECHO COMMAND
		_echoCommandLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s340"), JLabel.LEFT);
		_echoCommandCheckBox = new JCheckBox();

		// Sets the values into the text fields
		try {

			// SHELL PATH
			if (AcideConsoleConfiguration.getInstance().getShellPath()
					.matches("null"))
				_shellPathTextField.setText("");
			else
				_shellPathTextField.setText(AcideConsoleConfiguration
						.getInstance().getShellPath());

			// SHELL DIRECTORY
			if (AcideConsoleConfiguration.getInstance().getShellDirectory()
					.matches("null"))
				_shellDirectoryTextField.setText("");
			else
				_shellDirectoryTextField.setText(AcideConsoleConfiguration
						.getInstance().getShellDirectory());

			// EXIT COMMAND
			if (AcideConsoleConfiguration.getInstance().getExitCommand()
					.matches("null"))
				_exitCommandTextField.setText("null");
			else
				_exitCommandTextField.setText(AcideConsoleConfiguration
						.getInstance().getExitCommand());

			// ECHO COMMAND
			_echoCommandCheckBox.setSelected(AcideConsoleConfiguration
					.getInstance().getIsEchoCommand());

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// APPLY BUTTON
		_applyButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s335"));
		_applyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_applyButton.setMnemonic(KeyEvent.VK_A);
		_applyButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s336"));

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s178"));
		_cancelButton.setVerticalTextPosition(AbstractButton.CENTER);
		_cancelButton.setHorizontalTextPosition(AbstractButton.LEADING);

		// EXAMINE SHELL PATH BUTTON
		_examineShellPathButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s142"));
		_examineShellPathButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s301"));

		// EXAMINE SHELL DIRECTORY BUTTON
		_examineShellDirectoryButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s142"));
		_examineShellDirectoryButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s301"));
		_examineShellDirectoryButton.setEnabled(false);

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window with the layout
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
		_mainPanel.add(_examineShellPathButton, constraints);

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
		_mainPanel.add(_examineShellDirectoryButton, constraints);

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
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s334"));
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// APPLY BUTTON
		_applyButton.addActionListener(new ApplyButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());

		// MANUAL PATH CHECK BOX
		_manualPathCheckBox.addItemListener(new ManualPathCheckBoxAction());

		// EXAMINE SHELL DIRECTORY BUTTON
		_examineShellDirectoryButton
				.addActionListener(new ExamineShellDirectoryButtonAction());

		// EXAMINE SHELL PATH BUTTON
		_examineShellPathButton
				.addActionListener(new ExamineShellPathButtonAction());

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

						// Exits the output
						AcideMainWindow.getInstance().getConsolePanel()
								.executeExitCommand();

						// If the shell directory is enabled
						if (_shellDirectoryTextField.isEnabled()) {

							// Sets the shell directory in the console configuration
							AcideConsoleConfiguration.getInstance()
									.setShellDirectory(
											_shellDirectoryTextField.getText());
						} else {

							// Gets the shell directory
							String calculatedPath = "";
							String execTextField = _shellPathTextField
									.getText();
							String separator = "\\";

							int lastIndexOfSlash = execTextField.lastIndexOf("\\");
							if (lastIndexOfSlash == -1)
								separator = "/";
							StringTokenizer stringTokenizer = new StringTokenizer(
									execTextField, separator);

							int limit = stringTokenizer.countTokens();
							for (int index = 0; index < limit - 1; index++)
								calculatedPath = calculatedPath
										+ stringTokenizer.nextToken()
										+ separator;

							// Sets the shell directory in the console configuration
							AcideConsoleConfiguration.getInstance()
									.setShellDirectory(calculatedPath);
						}

						// Sets the shell path
						AcideConsoleConfiguration.getInstance().setShellPath(
								_shellPathTextField.getText());

						// Sets the echo command
						AcideConsoleConfiguration.getInstance().setEchoCommand(
								_echoCommandCheckBox.isSelected());

						// Sets the echo command
						AcideConsoleConfiguration.getInstance().setExitCommand(
								_exitCommandTextField.getText());

						// Resets the console
						AcideMainWindow.getInstance().getConsolePanel()
								.resetConsole();

						// Updates the ACIDE - A Configurable IDE console configuration
						AcideResourceManager.getInstance().setProperty(
								"consoleConfiguration",
								"./configuration/console/configuration.xml");
						
						// Saves the console configuration
						AcideConsoleConfiguration.getInstance().save();

						// If it is not the default project
						if (!AcideProjectConfiguration.getInstance()
								.isDefaultProject())

							// The project has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);

						// If the project window configuration has been
						// configured
						if (AcideMainWindow.getInstance()
								.getNewProjectWindowConfiguration() != null)

							// The paths have been defined
							AcideMainWindow.getInstance()
									.getNewProjectWindowConfiguration()
									.setAreShellPathsDefined(true);

					} else {

						// Shows an error message
						JOptionPane.showMessageDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s993"), "Error",
								JOptionPane.ERROR_MESSAGE);
					}
				} else {

					// Exits the console
					AcideMainWindow.getInstance().getConsolePanel()
							.executeExitCommand();

					// Sets the shell directory
					AcideConsoleConfiguration.getInstance().setShellDirectory(
							_shellDirectoryTextField.getText());

					// Sets the shell path
					AcideConsoleConfiguration.getInstance().setShellPath(
							_shellPathTextField.getText());

					// Sets the echo command
					AcideConsoleConfiguration.getInstance().setEchoCommand(
							_echoCommandCheckBox.isSelected());

					// Sets the echo command
					AcideConsoleConfiguration.getInstance().setExitCommand(
							_exitCommandTextField.getText());

					// Resets the console
					AcideMainWindow.getInstance().getConsolePanel().resetConsole();

					// Updates the ACIDE - A Configurable IDE console configuration
					AcideResourceManager.getInstance().setProperty(
							"consoleConfiguration",
							"./configuration/console/configuration.xml");
					
					// Saves the console configuration
					AcideConsoleConfiguration.getInstance().save();

					// Not default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject())

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

					// If the project window configuration has been
					// configured
					if (AcideMainWindow.getInstance()
							.getNewProjectWindowConfiguration() != null)

						// The paths have been defined
						AcideMainWindow.getInstance()
								.getNewProjectWindowConfiguration()
								.setAreShellPathsDefined(true);
				}
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

			// Asks the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askAbsolutePath();
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

			// Asks the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askDirectoryAbsolutePath();

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
}
