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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel;

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.configuration.toolBar.consolePanelToolBar.AcideConsolePanelToolBarButtonConf;
import acide.files.AcideFileManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.toolBarPanel.consolePanelToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE add console panel tool bar button window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideAddConsolePanelToolBarButtonWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window image
	 * icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window name
	 * label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window name
	 * text field.
	 */
	private JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * action label.
	 */
	private JLabel _actionLabel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * action text field.
	 */
	private JTextField _actionTextField;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window help
	 * label.
	 */
	private JLabel _helpLabel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window hint
	 * text label.
	 */
	private JLabel _hintTextLabel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window hint
	 * text text field.
	 */
	private JTextField _hintTextTextField;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window image
	 * label.
	 */
	private JLabel _iconLabel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window image
	 * text field.
	 */
	private JTextField _iconTextField;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window extra
	 * parameter label.
	 */
	private JLabel _extraParameterLabel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window extra
	 * parameter combo box.
	 */
	private JComboBox _extraParameterComboBox;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window main
	 * panel.
	 */
	private JPanel _commandPanel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * examine button.
	 */
	private JButton _examineButton;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window is
	 * executed in system shell check box.
	 */
	private JCheckBox _isExecutedInSystemShellCheckBox;
	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * console panel configuration panel.
	 */
	private AcideConsolePanelConfigurationPanel _consolePanelConfigurationPanel;

	/**
	 * Creates a new ACIDE - A Configurable IDE add console panel tool bar
	 * button window.
	 * 
	 * @param consolePanelConfigurationPanel
	 *            console panel configuration panel for stores the changes in
	 *            its table.
	 */
	public AcideAddConsolePanelToolBarButtonWindow(
			AcideConsolePanelConfigurationPanel consolePanelConfigurationPanel) {

		super();

		// Stores the console panel configuration panel instance
		_consolePanelConfigurationPanel = consolePanelConfigurationPanel;

		// Builds the window components
		buildComponents();

		// Sets the listeners to the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE add console panel tool bar button
	 * window components.
	 */
	private void buildComponents() {

		// Creates the name label
		_nameLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s133"), JLabel.LEFT);

		// Creates the name text field
		_nameTextField = new JTextField();

		// Creates the action label
		_actionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s134"), JLabel.LEFT);

		// Creates the action text field
		_actionTextField = new JTextField();

		// Creates the help label
		_helpLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s146"), JLabel.CENTER);

		// Sets the help label font as italic
		_helpLabel.setFont(new Font(_nameLabel.getFont().getFontName(),
				Font.ITALIC, _nameLabel.getFont().getSize()));

		// Creates the hint text label
		_hintTextLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s135"), JLabel.LEFT);

		// Creates the hint text text field
		_hintTextTextField = new JTextField();

		// Creates the icon label
		_iconLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s136"), JLabel.LEFT);

		// Creates the icon text field
		_iconTextField = new JTextField();

		// Creates the extra parameter label
		_extraParameterLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s139"), JLabel.CENTER);

		// Creates the extra parameter combo box items
		String[] data = {
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1005"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1006"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1007"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1008") };

		// Creates the extra parameter combo box with the previous items
		_extraParameterComboBox = new JComboBox(data);

		// Creates the is executed in system shell check box
		_isExecutedInSystemShellCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s1069"));

		// Creates the examine button
		_examineButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s142"));

		// Sets the examine button tool tip text
		_examineButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s143"));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// Sets the accept button tool tip text
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s155"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s163"));

		// Creates the command panel
		_commandPanel = new JPanel(new GridBagLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE add console panel tool bar button
	 * window configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s138"));

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
	 * Adds the components to the ACIDE - A Configurable IDE add console panel
	 * tool bar button window with the layout.
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

		// Adds the name label to the command panel
		_commandPanel.add(_nameLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;
		constraints.ipady = 5;

		// Adds the name text field to the command panel
		_commandPanel.add(_nameTextField, constraints);

		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the action label to the command panel
		_commandPanel.add(_actionLabel, constraints);

		constraints.gridx = 1;
		constraints.ipady = 5;

		// Adds the action text field to the command panel
		_commandPanel.add(_actionTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.ipady = 0;
		constraints.gridwidth = 3;

		// Adds the help label to the command panel
		_commandPanel.add(_helpLabel, constraints);

		constraints.gridy = 3;
		constraints.gridwidth = 1;

		// Adds the hint text label to the command panel
		_commandPanel.add(_hintTextLabel, constraints);

		constraints.gridx = 1;
		constraints.ipady = 5;

		// Adds the hint text text field to the command panel
		_commandPanel.add(_hintTextTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 4;
		constraints.ipady = 0;

		// Adds the icon label to the command panel
		_commandPanel.add(_iconLabel, constraints);

		constraints.gridx = 1;
		constraints.ipady = 5;

		// Adds the icon text field to the command panel
		_commandPanel.add(_iconTextField, constraints);

		constraints.gridx = 2;
		constraints.ipady = 0;

		// Adds the examine button to the command panel
		_commandPanel.add(_examineButton, constraints);

		constraints.gridx = 0;
		constraints.gridwidth = 3;
		constraints.gridy = 5;

		// Adds the extra parameter label to the command panel
		_commandPanel.add(_extraParameterLabel, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 6;

		// Adds the extra parameter combo box to the command panel
		_commandPanel.add(_extraParameterComboBox, constraints);

		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 7;

		// Adds the is executed in system shell check box to the command panel
		_commandPanel.add(_isExecutedInSystemShellCheckBox, constraints);

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;

		// Adds the command panel to the window
		add(_commandPanel, constraints);

		constraints.gridy = 1;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the listeners to the ACIDE - A Configurable IDE add console panel
	 * tool bar button window components.
	 */
	private void setListeners() {

		// Sets the examine button action listener
		_examineButton.addActionListener(new ExamineButtonAction());

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE add console panel tool bar button
	 * window.
	 */
	private void closeWindow() {

		// Closes the window
		dispose();
	}

	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * examine button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineButtonAction implements ActionListener {

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

			// Ask the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenFile(true);

			if (absolutePath != null)

				// Updates the icon text field with the absolute path
				_iconTextField.setText(absolutePath);
		}
	}

	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * accept button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the name from the name text field
			String name = _nameTextField.getText();

			// Gets the action from the the
			String action = _actionTextField.getText();

			// Gets the hint text from the hint text text field
			String hintText = _hintTextTextField.getText();

			// Gets the icon from the icon text field
			String icon = _iconTextField.getText();

			// Gets the extra parameter string from the extra parameter combo
			// box
			String extraParameterString = (String) _extraParameterComboBox
					.getSelectedItem();

			// Gets the is executed in system shell flag from the is executed in
			// system shell check box
			boolean isExecutedInSystemShell = _isExecutedInSystemShellCheckBox
					.isSelected();

			// No empty name and command are accepted
			if (!name.matches("") && !action.matches("")) {

				// Creates the new row
				AcideConsolePanelToolBarButtonConf editableToolBarCommand = new AcideConsolePanelToolBarButtonConf(
						name, action, hintText, icon,
						AcideParameterType
								.fromStringToEnum(extraParameterString),
						isExecutedInSystemShell);

				// Adds the command
				_consolePanelConfigurationPanel
						.addCommand(editableToolBarCommand);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s167"));

			} else

			// Name text field is empty
			if (name.matches(""))

				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s997"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s995"), JOptionPane.ERROR_MESSAGE);

			// Action text field is empty
			else if (action.matches(""))
				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s998"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s995"), JOptionPane.ERROR_MESSAGE);

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE add console panel tool bar button window
	 * cancel button action listener.
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
}
