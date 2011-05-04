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

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileExtensionFilterManager;
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
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

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
	 * ACIDE - A Configurable IDE compiler configuration window shell name text
	 * field.
	 */
	private JTextField _shellNameTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window examine path
	 * button.
	 */
	private JButton _examinePathButton;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window arguments text
	 * field.
	 */
	private JTextField _argumentsTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window shell name
	 * label.
	 */
	private JLabel _shellNameLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window arguments label.
	 */
	private JLabel _argumentsLabel;
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
	 * ACIDE - A Configurable IDE compiler configuration window compiler check
	 * box.
	 */
	private JCheckBox _compilerCheckBox;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window check label.
	 */
	private JLabel _checkLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window separator label.
	 */
	private JLabel _separatorLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window separator text
	 * field.
	 */
	private JTextField _separatorTextField;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window extension label.
	 */
	private JLabel _extensionLabel;
	/**
	 * ACIDE - A Configurable IDE compiler configuration window extension text
	 * field.
	 */
	private JTextField _extensionTextField;
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

		// The compiler has not been checked yet
		AcideProjectConfiguration.getInstance().setCheckCompiler(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s646"));

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

		// Creates the shell name label
		_shellNameLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s606"));

		// Creates the shell name text field
		_shellNameTextField = new JTextField();

		// Sets the shell name text field
		_shellNameTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s607"));

		// Creates the arguments label
		_argumentsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s609"));

		// Creates the arguments text field
		_argumentsTextField = new JTextField();

		// Sets the arguments text field tool tip text
		_argumentsTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s610"));

		// Creates the check label
		_checkLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s650"));

		// Creates the extension label
		_extensionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s653"));

		// Creates the extension text field
		_extensionTextField = new JTextField(7);

		// Sets the extension text field
		_extensionTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s652"));

		// Creates the compiler check box
		_compilerCheckBox = new JCheckBox();

		// Sets the compiler check box tool tip text
		_compilerCheckBox.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s650"));

		// Creates the separator label
		_separatorLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s649"));

		// Creates the separator text field
		_separatorTextField = new JTextField(1);

		// Sets the separator text field tool tip text
		_separatorTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s651"));

		// Disables the separator text field
		_separatorTextField.setEnabled(false);

		// Creates the examine path button
		_examinePathButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s596"));

		// Sets the examine path button horizontal alignment as center
		_examinePathButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the examine path button tool tip text
		_examinePathButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s641"));

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

		// Adds the shell name label to the configuration panel
		_configurationPanel.add(_shellNameLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;

		// Adds the shell name text field to the configuration panel
		_configurationPanel.add(_shellNameTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.ipadx = 0;

		// Adds the examine path button to the configuration panel
		_configurationPanel.add(_examinePathButton, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the arguments label to the configuration panel
		_configurationPanel.add(_argumentsLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 150;

		// Adds the arguments text field to the configuration panel
		_configurationPanel.add(_argumentsTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the configuration panel to the window
		add(_configurationPanel, constraints);

		// Adds the check label to the options panel
		optionsPanel.add(_checkLabel, constraints);

		constraints.gridx = 1;

		// Adds the compiler check box to the options panel
		optionsPanel.add(_compilerCheckBox, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the separator label to the options panel
		optionsPanel.add(_separatorLabel, constraints);

		constraints.gridx = 1;
		constraints.gridy = 1;

		// Adds the separator text field to the options panel
		optionsPanel.add(_separatorTextField, constraints);

		constraints.gridx = 2;
		constraints.gridy = 1;

		// Adds the extension label to the options panel
		optionsPanel.add(_extensionLabel, constraints);

		constraints.gridx = 3;
		constraints.gridy = 1;

		// Adds the extension text field to the options panel
		optionsPanel.add(_extensionTextField, constraints);

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

		// Sets the examine path button action listener
		_examinePathButton.addActionListener(new ExaminePathButtonAction());

		// Sets the separator text field action listener
		_separatorTextField.addActionListener(new SeparatorTextFieldAction());

		// Sets the compiler check box action listener
		_compilerCheckBox
				.addActionListener(new CompilerCheckBoxActionListener());

		// Adds the keyboard listener
		addKeyListener(new AcideCompilerConfigurationWindowKeyListener());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
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

			// Sets the compiler path
			AcideProjectConfiguration.getInstance().setCompilerPath(
					_shellNameTextField.getText());

			// Sets the compiler arguments
			AcideProjectConfiguration.getInstance().setCompilerArguments(
					_argumentsTextField.getText());

			// Sets the extension text field
			AcideProjectConfiguration.getInstance().setFileExtension(
					_extensionTextField.getText());

			// Sets the separator text field
			AcideProjectConfiguration.getInstance().setSeparatorFile(
					_separatorTextField.getText());

			// If the new project configuration window is visible
			if (AcideNewProjectConfigurationWindow.getInstance().isVisible())

				// The compiler paths are defined
				AcideNewProjectConfigurationWindow.getInstance()
						.setAreCompilerPathsDefined(true);

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

	/**
	 * ACIDE - A Configurable IDE compiler configuration window examine path
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExaminePathButtonAction implements ActionListener {

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
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.addChoosableFileFilter(
							new AcideFileExtensionFilterManager(extensions,
									"Compiler source (*.exe)"));

			// Ask the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenFile(true);

			if (absolutePath != null)

				// Updates the shell name text field with the absolute path
				_shellNameTextField.setText(absolutePath);
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window separator text
	 * field action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SeparatorTextFieldAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Sets the separator text field text from the project configuration
			AcideProjectConfiguration.getInstance().setSeparatorFile(
					_separatorTextField.getText());
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window compiler check
	 * box action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CompilerCheckBoxActionListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			if (_compilerCheckBox.isSelected()) {
				AcideProjectConfiguration.getInstance().setCheckCompiler(true);
				_extensionTextField.setText("");
				_extensionTextField.setEnabled(false);
				_separatorTextField.setEnabled(true);
			} else {
				AcideProjectConfiguration.getInstance().setCheckCompiler(false);
				_separatorTextField.setText("");
				_separatorTextField.setEnabled(false);
				_extensionTextField.setEnabled(true);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE compiler configuration window extension text
	 * field action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExtensionTextField implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			AcideProjectConfiguration.getInstance().setFileExtension(
					_extensionTextField.getText());
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

				// Enable the main window
				AcideMainWindow.getInstance().setEnabled(true);

				// Closes this window
				dispose();

				// Brings the main window to the front
				AcideMainWindow.getInstance().setAlwaysOnTop(true);

				// But not permanently
				AcideMainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
