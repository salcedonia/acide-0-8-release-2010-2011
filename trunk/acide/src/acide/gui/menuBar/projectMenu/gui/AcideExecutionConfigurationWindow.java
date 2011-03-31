/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.gui.menuBar.projectMenu.gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE execution configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideExecutionConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE execution configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE execution configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE execution configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window execution
	 * label.
	 */
	private JLabel _executionLabel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window examine path
	 * button.
	 */
	private JButton _examinePathButton;
	/**
	 * ACIDE - A Configurable IDE execution configuration window arguments
	 * label.
	 */
	private JLabel _argumentsLabel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window arguments text
	 * field.
	 */
	private JTextField _argumentsTextField;
	/**
	 * ACIDE - A Configurable IDE execution configuration window run button.
	 */
	private JButton _runButton;
	/**
	 * ACIDE - A Configurable IDE execution configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE execution configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window execution text
	 * field.
	 */
	private JTextField _executionTextField;

	/**
	 * Creates a new ACIDE - A Configurable IDE execution configuration window.
	 */
	public AcideExecutionConfigurationWindow() {

		super();

		// Builds the window components
		buildComponents();

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE execution configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s639"));

		// Sets the windo icon image
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
	 * Adds the components to the ACIDE - A Configurable IDE execution
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the execution label to the main panel
		_mainPanel.add(_executionLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;

		// Adds the execution text field to the main panel
		_mainPanel.add(_executionTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.ipadx = 0;

		// Adds the examine path button to the main panel
		_mainPanel.add(_examinePathButton, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the arguments label to the main panel
		_mainPanel.add(_argumentsLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 150;

		// Adds the arguments text field to the main panel
		_mainPanel.add(_argumentsTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the main panel to the window
		add(_mainPanel, constraints);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE execution configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Sets the main panel border
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s640"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the execution label
		_executionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s606"));

		// Creates the execution text field
		_executionTextField = new JTextField();

		// Sets the execution text field tool tip text
		_executionTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s638"));

		// Creates the arguments label
		_argumentsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s609"));

		// Creates the arguments text field
		_argumentsTextField = new JTextField();

		// Sets the arguments text field tool tip text
		_argumentsTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s610"));

		// Creates the examine path button
		_examinePathButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s596"));

		// Sets the examine path button horizontal alignment as center
		_examinePathButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the examine path button tool tip text
		_examinePathButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s641"));

		// Creates the run button
		_runButton = new JButton(AcideLanguageManager.getInstance().getLabels()
				.getString("s154"));

		// Sets the run button horizontal alignment as center
		_runButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the run button tool tip text
		_runButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Sets the cancel button horizontal alignment as center
		_cancelButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Adds the run button to the button panel
		_buttonPanel.add(_runButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the examine path button action listener
		_examinePathButton.addActionListener(new ExaminePathButtonAction());

		// Sets the examine path button key listener
		_examinePathButton
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the run button action listener
		_runButton.addActionListener(new RunButtonAction());

		// Sets the run button key listener
		_runButton
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the cancel button key listener
		_cancelButton
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the execution text field key listener
		_executionTextField
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the arguments text field key listener
		_argumentsTextField
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * ACIDE - A Configurable IDE execution configuration window examine path
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

			// Creates the file manager
			AcideFileManager fileManager = new AcideFileManager();

			String[] extensions = new String[] { "exe" };
			fileManager.getFileChooser().addChoosableFileFilter(
					new AcideFileExtensionFilterManager(extensions,
							"Executable source (*.exe)"));
			String path = fileManager.askAbsolutePath();
			_executionTextField.setText(path);
		}
	}

	/**
	 * ACIDE - A Configurable IDE execution configuration window run button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class RunButtonAction implements ActionListener {

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

				String execution = _executionTextField.getText();
				String arguments = _argumentsTextField.getText();

				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getNumberOfFileEditorPanels() > 0) {

					execution = execution.replace("$activeFile$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath());
					execution = execution
							.replace("$activeFilePath$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getFilePath());
					execution = execution.replace("$activeFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getFileExtension());
					execution = execution
							.replace("$activeFileName$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getFileNameWithoutExtension());

					arguments = arguments.replace("$activeFile$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath());
					arguments = arguments
							.replace("$activeFilePath$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getFilePath());
					arguments = arguments.replace("$activeFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getFileExtension());
					arguments = arguments
							.replace("$activeFileName$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getFileNameWithoutExtension());
				}

				// If it is the default project
				if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// If there is a main file in the file editor
					if (AcideMainWindow.getInstance().getFileEditorManager()
							.getMainFileEditorPanel() != null) {

						arguments = arguments.replace("$mainFile$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getAbsolutePath());
						execution = execution.replace("$mainFile$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getAbsolutePath());
						arguments = arguments
								.replace("$mainFilePath$", AcideMainWindow
										.getInstance().getFileEditorManager()
										.getMainFileEditorPanel().getFilePath());
						execution = execution
								.replace("$mainFilePath$", AcideMainWindow
										.getInstance().getFileEditorManager()
										.getMainFileEditorPanel().getFilePath());
						arguments = arguments.replace("$mainFileExt$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getFileExtension());
						execution = execution.replace("$mainFileExt$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getFileExtension());
						arguments = arguments
								.replace("$mainFileName$", AcideMainWindow
										.getInstance().getFileEditorManager()
										.getMainFileEditorPanel().getFileNameWithoutExtension());
						execution = execution
								.replace("$mainFileName$", AcideMainWindow
										.getInstance().getFileEditorManager()
										.getMainFileEditorPanel().getFileNameWithoutExtension());
					}
				} else {

					// Searches for the main file in the project configuration
					int mainFileIndex = -1;
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).isMainFile())
							mainFileIndex = index;
					}

					// If there is a main file in the project configuration
					if (mainFileIndex != -1) {
						arguments = arguments.replace("$mainFile$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileIndex)
										.getAbsolutePath());
						execution = execution.replace("$mainFile$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileIndex)
										.getAbsolutePath());
						arguments = arguments.replace("$mainFilePath$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileIndex)
										.getRelativePath());
						execution = execution.replace("$mainFilePath$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileIndex)
										.getRelativePath());
						arguments = arguments.replace("$mainFileExt$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileIndex)
										.getFileExtension());
						execution = execution.replace("$mainFileExt$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileIndex)
										.getFileExtension());
						arguments = arguments
								.replace("$mainFileName$",
										AcideProjectConfiguration.getInstance()
												.getFileAt(mainFileIndex)
												.getFileName());
						execution = execution
								.replace("$mainFileName$",
										AcideProjectConfiguration.getInstance()
												.getFileAt(mainFileIndex)
												.getFileName());
					}
				}

				// Executes the command
				Runtime.getRuntime().exec(execution + " " + arguments);

			} catch (IOException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage());
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
	 * ACIDE - A Configurable IDE execution configuration window cancel button
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
	 * ACIDE - A Configurable IDE execution configuration window key listener.
	 * 
	 * @version 0.8
	 * @see KeyAdapter
	 */
	class ExecutionConfigurationWindowKeyListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {

			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {

				// Enables the main window
				AcideMainWindow.getInstance().setEnabled(true);

				// Closes the configuration window
				dispose();

				// Brings the main window to the front
				AcideMainWindow.getInstance().setAlwaysOnTop(true);

				// Only this time
				AcideMainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
