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
package acide.gui.menuBar.projectMenu.gui.compilerWindow;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.projectMenu.gui.newProjectWindow.AcideNewProjectConfigurationWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE compiler configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideCompilerConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE compiler configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE compiler configuration window compiler path
	 * text field.
	 */
	private JTextField _compilerPathTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window examine compiler
	 * path button.
	 */
	private JButton _examineCompilerPathButton;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window compiler
	 * arguments text field.
	 */
	private JTextField _compilerArgumentsTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window compiler path
	 * label.
	 */
	private JLabel _compilerPathLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window compiler
	 * arguments label.
	 */
	private JLabel _compilerArgumentsLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window configuration
	 * panel.
	 */
	private JPanel _configurationPanel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window options panel.
	 */
	private JPanel optionsPanel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window compile all
	 * files check box.
	 */
	private JCheckBox _compileAllFilesCheckBox;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window file separator
	 * label.
	 */
	private JLabel _fileSeparatorLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window file separator
	 * text field.
	 */
	private JTextField _fileSeparatorTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window file extension
	 * label.
	 */
	private JLabel _fileExtensionLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window file extension
	 * text field.
	 */
	private JTextField _fileExtensionTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window cancel button.
	 */
	private JButton _cancelButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE compiler configuration window.
	 */
	public AcideCompilerConfigurationWindow() {

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
	 * Builds the ACIDE - A Configurable IDE compiler configuration window
	 * components.
	 */
	private void buildComponents() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s646"));

		// Creates the configuration panel
		_configurationPanel = new JPanel(new GridBagLayout());

		// Sets the configuration panel border
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s644"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the options panel
		optionsPanel = new JPanel(new GridBagLayout());

		// Sets the options panel border
		optionsPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s645"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the compiler path label
		_compilerPathLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s606"));

		if (AcideProjectConfiguration.getInstance().getCompilerPath()
				.matches("null"))
			// Creates the compiler path text field
			_compilerPathTextField = new JTextField();
		else
			// Creates the compiler path text field
			_compilerPathTextField = new JTextField(AcideProjectConfiguration
					.getInstance().getCompilerPath());

		// Sets the compiler path text field
		_compilerPathTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s607"));

		// Creates the compiler arguments label
		_compilerArgumentsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s609"));

		if (AcideProjectConfiguration.getInstance().getCompilerArguments()
				.matches("null"))

			// Creates the compiler arguments text field
			_compilerArgumentsTextField = new JTextField();
		else
			// Creates the compiler arguments text field
			_compilerArgumentsTextField = new JTextField(
					AcideProjectConfiguration.getInstance()
							.getCompilerArguments());

		// Sets the compiler arguments text field tool tip text
		_compilerArgumentsTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s610"));

		// Creates the file extension label
		_fileExtensionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s653"));

		if (AcideProjectConfiguration.getInstance().getFileExtension()
				.matches("null"))
			// Creates the file extension text field
			_fileExtensionTextField = new JTextField(7);
		else
			// Creates the file extension text field
			_fileExtensionTextField = new JTextField(AcideProjectConfiguration
					.getInstance().getFileExtension(), 7);

		// Sets the file extension text field
		_fileExtensionTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s652"));

		// Creates the compile all files check box
		_compileAllFilesCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s650"),
				AcideProjectConfiguration.getInstance().getCompileAllFiles());

		// Sets the compile all files check box tool tip text
		_compileAllFilesCheckBox.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s650"));

		// Creates the file separator label
		_fileSeparatorLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s649"));

		if (AcideProjectConfiguration.getInstance().getFileSeparator()
				.matches("null"))

			// Creates the file separator text field
			_fileSeparatorTextField = new JTextField(1);
		else
			// Creates the file separator text field
			_fileSeparatorTextField = new JTextField(AcideProjectConfiguration
					.getInstance().getFileSeparator(), 1);

		// Sets the file separator text field tool tip text
		_fileSeparatorTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s651"));

		// Creates the examine compiler path button
		_examineCompilerPathButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s596"));

		// Sets the examine compiler path button horizontal alignment as center
		_examineCompilerPathButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the examine compiler path button tool tip text
		_examineCompilerPathButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s641"));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// Sets the accept button horizontal alignment as center
		_acceptButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the accept button tool tip text
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Sets the cancel button horizontal alignment as center
		_cancelButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE compiler
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

		// Adds the compiler path label to the configuration panel
		_configurationPanel.add(_compilerPathLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;

		// Adds the compiler path text field to the configuration panel
		_configurationPanel.add(_compilerPathTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.ipadx = 0;

		// Adds the examine compiler path button to the configuration panel
		_configurationPanel.add(_examineCompilerPathButton, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the compiler arguments label to the configuration panel
		_configurationPanel.add(_compilerArgumentsLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 150;

		// Adds the compiler arguments text field to the configuration panel
		_configurationPanel.add(_compilerArgumentsTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the configuration panel to the window
		add(_configurationPanel, constraints);

		constraints.gridwidth = 4;

		// Adds the compile all files check box to the options panel
		optionsPanel.add(_compileAllFilesCheckBox, constraints);

		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the file separator label to the options panel
		optionsPanel.add(_fileSeparatorLabel, constraints);

		constraints.gridx = 1;
		constraints.gridy = 1;

		// Adds the file separator text field to the options panel
		optionsPanel.add(_fileSeparatorTextField, constraints);

		constraints.gridx = 2;
		constraints.gridy = 1;

		// Adds the file extension label to the options panel
		optionsPanel.add(_fileExtensionLabel, constraints);

		constraints.gridx = 3;
		constraints.gridy = 1;

		// Adds the file extension text field to the options panel
		optionsPanel.add(_fileExtensionTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the options panel to the window
		add(optionsPanel, constraints);

		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 2;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE compiler configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s647"));

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

		// Saves the file editor panel configuration
		AcideWorkbenchConfiguration.getInstance()
				.saveFileEditorOpenedFilesConfiguration();
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE compiler
	 * configuration window components.
	 */
	private void setListeners() {

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the examine compiler path button action listener
		_examineCompilerPathButton
				.addActionListener(new ExamineCompilerPathButtonAction());

		// Sets the file separator text field action listener
		_fileSeparatorTextField
				.addActionListener(new FileSeparatorTextFieldAction());

		// Sets the compile all files check box action listener
		_compileAllFilesCheckBox
				.addActionListener(new CompileAllFilesCheckBoxActionListener());

		// Adds the keyboard listener
		addKeyListener(new AcideCompilerConfigurationWindowKeyListener());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE compiler configuration window
	 * components
	 */
	private void closeWindow() {

		// Enable the main window
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes this window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {
		/*
		 * 
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Sets the compiler path text field in the ACIDE - A Configurable
			// IDE
			// project configuration
			AcideProjectConfiguration.getInstance().setCompilerPath(
					_compilerPathTextField.getText());

			// Sets the compiler arguments in the ACIDE - A Configurable IDE
			// project configuration
			AcideProjectConfiguration.getInstance().setCompilerArguments(
					_compilerArgumentsTextField.getText());

			// Sets the file extension text field in the ACIDE - A Configurable
			// IDE
			// project configuration
			AcideProjectConfiguration.getInstance().setFileExtension(
					_fileExtensionTextField.getText());

			// Sets the file separator text field in the ACIDE - A Configurable
			// IDE
			// project configuration
			AcideProjectConfiguration.getInstance().setFileSeparator(
					_fileSeparatorTextField.getText());

			// If the new project configuration window is visible
			if (AcideNewProjectConfigurationWindow.getInstance().isVisible())

				// The compiler paths are defined
				AcideNewProjectConfigurationWindow.getInstance()
						.setAreCompilerPathsDefined(true);
			else {

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
			}

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window examine compiler
	 * path button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineCompilerPathButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
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

						// Updates the compiler path text field with the
						// absolute path
						_compilerPathTextField.setText(absolutePath);

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
	 * ACIDE - A Configurable IDE compiler configuration window file separator
	 * text field action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class FileSeparatorTextFieldAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Sets the separator text field text in the ACIDE - A Configurable
			// IDE project configuration
			AcideProjectConfiguration.getInstance().setFileSeparator(
					_fileSeparatorTextField.getText());
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window compile all
	 * files check box action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CompileAllFilesCheckBoxActionListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			if (_compileAllFilesCheckBox.isSelected()) {

				// All files are ready for the compilation
				AcideProjectConfiguration.getInstance()
						.setCompileAllFiles(true);

				// Sets the file extension text field as ""
				_fileExtensionTextField.setText("");

				// Disables the file extension text field
				_fileExtensionTextField.setEnabled(false);

				// Enables the file separator text field
				_fileSeparatorTextField.setEnabled(true);
			} else {

				// All the files are not ready for the compilation
				AcideProjectConfiguration.getInstance().setCompileAllFiles(
						false);

				// Sets the file separator text field as ""
				_fileSeparatorTextField.setText("");

				// Disables the file separator text field
				_fileSeparatorTextField.setEnabled(false);

				// Enables the file extension text field
				_fileExtensionTextField.setEnabled(true);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window file extension
	 * text field action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class FileExtensionTextField implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Sets the file extension text field text in the ACIDE - A
			// Configurable
			// IDE project configuration
			AcideProjectConfiguration.getInstance().setFileExtension(
					_fileExtensionTextField.getText());
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window key listener.
	 * 
	 * @version 0.8
	 * @see KeyAdapter
	 */
	class AcideCompilerConfigurationWindowKeyListener extends KeyAdapter {
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
