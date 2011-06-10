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
package acide.gui.menuBar.projectMenu.gui.executionWindow;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.execution.AcideProjectExecutionProcess;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE executable path configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideExecutionConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE executable path configuration window class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window image
	 * icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE executable path configuration window main
	 * panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window
	 * executable path label.
	 */
	private JLabel _executablePathLabel;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window examine
	 * path button.
	 */
	private JButton _examinePathButton;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window arguments
	 * label.
	 */
	private JLabel _argumentsLabel;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window arguments
	 * text field.
	 */
	private JTextField _argumentsTextField;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window run
	 * button.
	 */
	private JButton _runButton;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window cancel
	 * button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window button
	 * panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE executable path configuration window
	 * executable path text field.
	 */
	private JTextField _executablePathTextField;

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

		// Adds the executable path label to the main panel
		_mainPanel.add(_executablePathLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;

		// Adds the executable path text field to the main panel
		_mainPanel.add(_executablePathTextField, constraints);

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

		// Creates the executable path label
		_executablePathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s606"));

		// Creates the executable path text field
		_executablePathTextField = new JTextField();

		// Sets the executable path text field tool tip text
		_executablePathTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s638"));

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
	 * Sets the listeners of the ACIDE - A Configurable IDE execution
	 * configuration window components.
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

		// Sets the executable path text field key listener
		_executablePathTextField
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the arguments text field key listener
		_argumentsTextField
				.addKeyListener(new ExecutionConfigurationWindowKeyListener());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE execution configuration window.
	 */
	private void closeWindow() {

		// Enables the main window
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the configuration window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// Only this time
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
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

			// Adds the extension
			String[] extensions = new String[] { "exe" };

			File selectedFile = null;
			String absolutePath = null;

			// Creates the file chooser
			JFileChooser _fileChooser = new JFileChooser();

			String lastPath;

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedFileDirectory");

				// Sets the current directory to the default path
				_fileChooser.setCurrentDirectory(new File(lastPath));

				// Adds the filter to the file chooser
				_fileChooser
						.addChoosableFileFilter(new AcideFileExtensionFilterManager(
								extensions, "Compiler source (*.exe)"));

				// Disables the multiple selection of files
				_fileChooser.setMultiSelectionEnabled(false);

				// Sets only files
				_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Ask for the file to the user
				int returnValue = _fileChooser.showOpenDialog(null);

				// If it is ok
				if (returnValue == JFileChooser.APPROVE_OPTION) {

					// Gets the selected file
					selectedFile = _fileChooser.getSelectedFile();

					// Gets the absolute path
					absolutePath = selectedFile.getAbsolutePath();

					if (absolutePath != null) {

						// Updates the executable path text field with the
						// absolute path
						_executablePathTextField.setText(absolutePath);

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s300")
										+ absolutePath);

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedFileDirectory", absolutePath);
					}
				} else if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels the selection
					_fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s302"));
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE execution configuration window window run
	 * button action listener.
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

			// Gets the executable path
			String executablePath = _executablePathTextField.getText();

			// Gets the arguments
			String arguments = _argumentsTextField.getText();

			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels() > 0) {

				executablePath = executablePath
						.replace("$activeFile$", AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath());
				executablePath = executablePath.replace("$activeFilePath$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFilePath());
				executablePath = executablePath.replace("$activeFileExt$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileExtension());
				executablePath = executablePath.replace("$activeFileName$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileNameWithoutExtension());

				arguments = arguments.replace("$activeFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getAbsolutePath());
				arguments = arguments.replace("$activeFilePath$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFilePath());
				arguments = arguments.replace("$activeFileExt$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileExtension());
				arguments = arguments.replace("$activeFileName$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileNameWithoutExtension());
			}

			// If it is the default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// If there is a main file in the file editor
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getMainFileEditorPanel() != null) {

					arguments = arguments.replace("$mainFile$", AcideMainWindow
							.getInstance().getFileEditorManager()
							.getMainFileEditorPanel().getAbsolutePath());
					executablePath = executablePath
							.replace("$mainFile$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getMainFileEditorPanel().getAbsolutePath());
					arguments = arguments.replace("$mainFilePath$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel().getFilePath());
					executablePath = executablePath.replace("$mainFilePath$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel().getFilePath());
					arguments = arguments.replace("$mainFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileExtension());
					executablePath = executablePath.replace("$mainFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileExtension());
					arguments = arguments.replace("$mainFileName$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileNameWithoutExtension());
					executablePath = executablePath.replace("$mainFileName$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileNameWithoutExtension());
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
					arguments = arguments
							.replace("$mainFile$", AcideProjectConfiguration
									.getInstance().getFileAt(mainFileIndex)
									.getAbsolutePath());
					executablePath = executablePath
							.replace("$mainFile$", AcideProjectConfiguration
									.getInstance().getFileAt(mainFileIndex)
									.getAbsolutePath());
					arguments = arguments
							.replace("$mainFilePath$",
									AcideProjectConfiguration.getInstance()
											.getFileAt(mainFileIndex)
											.getRelativePath());
					executablePath = executablePath
							.replace("$mainFilePath$",
									AcideProjectConfiguration.getInstance()
											.getFileAt(mainFileIndex)
											.getRelativePath());
					arguments = arguments.replace(
							"$mainFileExt$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex)
									.getFileExtension());
					executablePath = executablePath.replace(
							"$mainFileExt$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex)
									.getFileExtension());
					arguments = arguments.replace(
							"$mainFileName$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex).getFileName());
					executablePath = executablePath.replace(
							"$mainFileName$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex).getFileName());
				}
			}

			// Creates the project execution process
			AcideProjectExecutionProcess process = new AcideProjectExecutionProcess(
					executablePath, arguments);

			// Starts the process
			process.start();

			// Closes the window
			closeWindow();
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

			// Closes the window
			closeWindow();
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

				// Closes the window
				closeWindow();
			}
		}
	}
}
