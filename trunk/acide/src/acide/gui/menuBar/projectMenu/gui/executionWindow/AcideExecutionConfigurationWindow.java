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

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.listeners.AcideStatusBarKeyboardListener;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.process.execution.AcideProjectExecutionProcess;

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

		// Creates the executable path text field with the project configuration
		// value
		_executablePathTextField = new JTextField(AcideProjectConfiguration
				.getInstance().getExecutablePath());

		// Sets the executable path text field tool tip text
		_executablePathTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s638"));

		// Creates the arguments label
		_argumentsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s609"));

		// Creates the arguments text field with the project configuration value
		_argumentsTextField = new JTextField(AcideProjectConfiguration
				.getInstance().getExecutableArguments());

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

		// Sets the run button action listener
		_runButton.addActionListener(new RunButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());

		// Puts the escape key in the input map of the window
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false),
				"EscapeKey");

		// Puts the escape key in the action map of the window
		getRootPane().getActionMap().put("EscapeKey", new EscapeKeyAction());

		// Adds the status bar keyboards listener to all the main window
		// components
		AcideStatusBarKeyboardListener statusBarKeyListener = new AcideStatusBarKeyboardListener();

		Component[] components = getContentPane().getComponents();
		for (Component component : components) {
			component.addKeyListener(statusBarKeyListener);
		}
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

			// Asks for the file path to the user
			String absolutePath = AcideFileManager.getInstance().askForFile(
					AcideFileOperation.OPEN,
					AcideFileTarget.FILES,
					AcideFileType.FILE,
					"",
					new AcideFileExtensionFilterManager(new String[] { "exe" },
							"Compiler source (*.exe)"));

			if (absolutePath != null) {

				// Updates the executable path text field with the
				// absolute path
				_executablePathTextField.setText(absolutePath);
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

			// Stores the executable path
			AcideProjectConfiguration.getInstance().setExecutablePath(
					_executablePathTextField.getText());

			// Stores the arguments path
			AcideProjectConfiguration.getInstance().setExecutableArguments(
					_argumentsTextField.getText());

			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels() > 0) {

				// Replaces the $activeFile$ variable for its real value
				executablePath = executablePath
						.replace("$activeFile$", AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath());

				// Replaces the $activeFilePath$ variable for its real value
				executablePath = executablePath.replace("$activeFilePath$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFilePath());

				// Replaces the $activeFileExt$ variable for its real value
				executablePath = executablePath.replace("$activeFileExt$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileExtension());

				// Replaces the $activeFileName$ variable for its real value
				executablePath = executablePath.replace("$activeFileName$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileNameWithoutExtension());

				// Replaces the $activeFile$ variable for its real value
				arguments = arguments.replace("$activeFile$", AcideMainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getAbsolutePath());

				// Replaces the $activeFilePath$ variable for its real value
				arguments = arguments.replace("$activeFilePath$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFilePath());

				// Replaces the $activeFileExt$ variable for its real value
				arguments = arguments.replace("$activeFileExt$",
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getFileExtension());

				// Replaces the $activeFileName$ variable for its real value
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

					// Replaces the $mainFile$ variable for its real value
					arguments = arguments.replace("$mainFile$", AcideMainWindow
							.getInstance().getFileEditorManager()
							.getMainFileEditorPanel().getAbsolutePath());

					// Replaces the $mainFile$ variable for its real value
					executablePath = executablePath
							.replace("$mainFile$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getMainFileEditorPanel().getAbsolutePath());

					// Replaces the $mainFilePath$ variable for its real value
					arguments = arguments.replace("$mainFilePath$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel().getFilePath());

					// Replaces the $mainFilePath$ variable for its real value
					executablePath = executablePath.replace("$mainFilePath$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel().getFilePath());

					// Replaces the $mainFileExt$ variable for its real value
					arguments = arguments.replace("$mainFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileExtension());

					// Replaces the $mainFileExt$ variable for its real value
					executablePath = executablePath.replace("$mainFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileExtension());

					// Replaces the $mainFileName$ variable for its real value
					arguments = arguments.replace("$mainFileName$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getMainFileEditorPanel()
									.getFileNameWithoutExtension());

					// Replaces the $mainFileName$ variable for its real value
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

					// Replaces the $mainFile$ variable for its real value
					arguments = arguments
							.replace("$mainFile$", AcideProjectConfiguration
									.getInstance().getFileAt(mainFileIndex)
									.getAbsolutePath());

					// Replaces the $mainFile$ variable for its real value
					executablePath = executablePath
							.replace("$mainFile$", AcideProjectConfiguration
									.getInstance().getFileAt(mainFileIndex)
									.getAbsolutePath());

					// Replaces the $mainFilePath$ variable for its real value
					arguments = arguments
							.replace("$mainFilePath$",
									AcideProjectConfiguration.getInstance()
											.getFileAt(mainFileIndex)
											.getRelativePath());

					// Replaces the $mainFilePath$ variable for its real value
					executablePath = executablePath
							.replace("$mainFilePath$",
									AcideProjectConfiguration.getInstance()
											.getFileAt(mainFileIndex)
											.getRelativePath());

					// Replaces the $mainFileExt$ variable for its real value
					arguments = arguments.replace(
							"$mainFileExt$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex)
									.getFileExtension());

					// Replaces the $mainFileExt$ variable for its real value
					executablePath = executablePath.replace(
							"$mainFileExt$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex)
									.getFileExtension());

					// Replaces the $mainFileName$ variable for its real value
					arguments = arguments.replace(
							"$mainFileName$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex).getFileName());

					// Replaces the $mainFileName$ variable for its real value
					executablePath = executablePath.replace(
							"$mainFileName$",
							AcideProjectConfiguration.getInstance()
									.getFileAt(mainFileIndex).getFileName());
				} else {

					// If there is a main file in the file editor
					if (AcideMainWindow.getInstance().getFileEditorManager()
							.getMainFileEditorPanel() != null) {

						// Replaces the $mainFile$ variable for its real value
						arguments = arguments.replace("$mainFile$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getAbsolutePath());

						// Replaces the $mainFile$ variable for its real value
						executablePath = executablePath.replace("$mainFile$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getAbsolutePath());

						// Replaces the $mainFilePath$ variable for its real
						// value
						arguments = arguments
								.replace("$mainFilePath$", AcideMainWindow
										.getInstance().getFileEditorManager()
										.getMainFileEditorPanel().getFilePath());

						// Replaces the $mainFilePath$ variable for its real
						// value
						executablePath = executablePath
								.replace("$mainFilePath$", AcideMainWindow
										.getInstance().getFileEditorManager()
										.getMainFileEditorPanel().getFilePath());

						// Replaces the $mainFileExt$ variable for its real
						// value
						arguments = arguments.replace("$mainFileExt$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getFileExtension());

						// Replaces the $mainFileExt$ variable for its real
						// value
						executablePath = executablePath.replace(
								"$mainFileExt$", AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getFileExtension());

						// Replaces the $mainFileName$ variable for its real
						// value
						arguments = arguments.replace("$mainFileName$",
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getFileNameWithoutExtension());

						// Replaces the $mainFileName$ variable for its real
						// value
						executablePath = executablePath.replace(
								"$mainFileName$", AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getMainFileEditorPanel()
										.getFileNameWithoutExtension());
					}
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
	 * ACIDE - A Configurable IDE execution configuration window escape key
	 * action.
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
