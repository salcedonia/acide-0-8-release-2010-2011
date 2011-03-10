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
package acide.gui.menuBar.projectMenu.gui;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.project.workbench.AcideWorkbenchManager;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.gui.listeners.AcideWindowListener;
import acide.gui.mainWindow.AcideMainWindow;

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

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// The compiler has not been checked yet
		AcideProjectConfiguration.getInstance().setCheckCompiler(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s646"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// CONFIGURATION PANEL
		_configurationPanel = new JPanel(new GridBagLayout());
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s644"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// OPTIONS PANEL
		optionsPanel = new JPanel(new GridBagLayout());
		optionsPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s645"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// SHELL NAME LABEL
		_shellNameLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s606"));

		// SHELL NAME TEXT FIELD
		_shellNameTextField = new JTextField();
		_shellNameTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s607"));

		// ARGUMENTS LABEL
		_argumentsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s609"));

		// ARGUMENTS TEXT FIELD
		_argumentsTextField = new JTextField();
		_argumentsTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s610"));

		// CHECK LABEL
		_checkLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s650"));

		// EXTENSION LABEL
		_extensionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s653"));

		// EXTENSION TEXT FIELD
		_extensionTextField = new JTextField(7);
		_extensionTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s652"));

		// COMPILER CHECK BOX
		_compilerCheckBox = new JCheckBox();
		_compilerCheckBox.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s650"));

		// SEPARATOR LABEL
		_separatorLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s649"));

		// SEPARATOR TEXT FIELD
		_separatorTextField = new JTextField(1);
		_separatorTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s651"));
		_separatorTextField.setEnabled(false);

		// EXAMINE PATH BUTTON
		_examinePathButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s596"));
		_examinePathButton.setHorizontalAlignment(JButton.CENTER);
		_examinePathButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s641"));

		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// CONFIGURATION PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_configurationPanel.add(_shellNameLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		_configurationPanel.add(_shellNameTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_configurationPanel.add(_examinePathButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_configurationPanel.add(_argumentsLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_configurationPanel.add(_argumentsTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_configurationPanel, constraints);

		// OPTION PANEL
		optionsPanel.add(_checkLabel, constraints);
		constraints.gridx = 1;
		optionsPanel.add(_compilerCheckBox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		optionsPanel.add(_separatorLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		optionsPanel.add(_separatorTextField, constraints);
		constraints.gridx = 2;
		constraints.gridy = 1;
		optionsPanel.add(_extensionLabel, constraints);
		constraints.gridx = 3;
		constraints.gridy = 1;
		optionsPanel.add(_extensionTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(optionsPanel, constraints);

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton, constraints);
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s647"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		setVisible(true);
		setLocationRelativeTo(null);
		addWindowListener(new AcideWindowListener());

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Saves the file editor panel configuration
		AcideWorkbenchManager.getInstance().saveFileEditorPanelConfiguration();
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());

		// EXAMINE PATH BUTTON
		_examinePathButton.addActionListener(new ExaminePathButtonAction());

		// SEPARATOR TEXT FIELD
		_separatorTextField.addActionListener(new SeparatorTextFieldAction());

		// COMPILER CHECK BOX
		_compilerCheckBox
				.addActionListener(new CompilerCheckBoxActionListener());

		// WINDOW
		addKeyListener(new AcideCompilerConfigurationWindowKeyListener());
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

			AcideProjectConfiguration.getInstance().setCompilerPath(
					_shellNameTextField.getText());
			AcideProjectConfiguration.getInstance().setCompilerArguments(
					_argumentsTextField.getText());
			AcideProjectConfiguration.getInstance().setFileExtension(
					_extensionTextField.getText());
			AcideProjectConfiguration.getInstance().setSeparatorFile(
					_separatorTextField.getText());
			AcideMainWindow.getInstance().setEnabled(true);

			if (AcideMainWindow.getInstance().getNewProjectWindowConfiguration() != null)
				AcideMainWindow.getInstance().getNewProjectWindowConfiguration()
						.setAreCompilerPathsDefined(true);

			// Closes the window
			dispose();
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

			String[] extensions = new String[] { "exe" };
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.addChoosableFileFilter(
							new AcideFileExtensionFilterManager(extensions,
									"Compiler source (*.exe)"));

			// Asks for the path to the user
			String path = AcideFileManager.getInstance().askAbsolutePath();
			_shellNameTextField.setText(path);
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
	 * ACIDE - A Configurable IDE compiler configuration window extension text field action listener.
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

				// Only this time
				AcideMainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
