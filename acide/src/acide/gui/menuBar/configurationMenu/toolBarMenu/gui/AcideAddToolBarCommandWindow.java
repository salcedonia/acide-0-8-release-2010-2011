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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui;

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommand;
import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE add tool bar command window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideAddToolBarCommandWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE add tool bar command window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE add tool bar command window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window name text field.
	 */
	private final JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window action label.
	 */
	private JLabel _actionLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window action text field.
	 */
	private final JTextField _actionTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window help label.
	 */
	private JLabel _helpLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window hint text label.
	 */
	private JLabel _hintTextLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window hint text text
	 * field.
	 */
	private final JTextField _hintTextTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window image label.
	 */
	private JLabel _iconLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window image text field.
	 */
	private final JTextField _iconTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window extra parameter
	 * label.
	 */
	private JLabel _extraParameterLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window extra parameter
	 * combo box.
	 */
	private JComboBox _extraParameterComboBox;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window main panel.
	 */
	private JPanel _commandPanel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window examine button.
	 */
	private JButton _examineButton;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window tool bar
	 * configuration window.
	 */
	private AcideToolBarConfigurationWindow _toolBarConfigurationWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE add tool bar command window.
	 * 
	 * @param acideToolBarConfigurationWindow
	 */
	public AcideAddToolBarCommandWindow(
			AcideToolBarConfigurationWindow acideToolBarConfigurationWindow) {

		super();

		// Stores the tool bar configuration window instance
		_toolBarConfigurationWindow = acideToolBarConfigurationWindow;

		// Sets the layout
		setLayout(new GridBagLayout());

		// Enables the main window again
		AcideMainWindow.getInstance().setEnabled(false);
		
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

		// Sets the listeners to the window components
		setListeners();

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

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		
		// Adds the command panel to the window
		add(_commandPanel, constraints);

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);
		
		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
		
		constraints.gridy = 1;
		
		// Adds the button panel to the window
		add(_buttonPanel, constraints);

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
	 * Sets the listeners to the window components.
	 */
	private void setListeners() {

		// Sets the examine button action listener
		_examineButton.addActionListener(new ExamineButtonAction());

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE add tool bar command window.
	 */
	private void closeWindow() {

		// Set the main window enabled again
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();
		
		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);
		
		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * ACIDE - A Configurable IDE add tool bar command window examine button
	 * action listener.
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

			// Asks the path to the user
			String path = AcideFileManager.getInstance().askAbsolutePath();
			_iconTextField.setText(path);
		}
	}

	/**
	 * ACIDE - A Configurable IDE add tool bar command window accept button
	 * action listener.
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

			String name = _nameTextField.getText();
			String command = _actionTextField.getText();
			String helpText = _hintTextTextField.getText();
			String image = _iconTextField.getText();
			String extraParameterString = (String) _extraParameterComboBox
					.getSelectedItem();

			// No empty name and command are accepted
			if (!name.matches("") && !command.matches("")) {

				// Creates the new row
				AcideConsoleCommand editableToolBarCommand;

				// Sets the image
				if (image.equals(""))
					editableToolBarCommand = new AcideConsoleCommand(name, command,
							helpText,
							AcideParameterType
									.fromStringToEnum(extraParameterString));
				else
					editableToolBarCommand = new AcideConsoleCommand(name, command,
							helpText, image,
							AcideParameterType
									.fromStringToEnum(extraParameterString));

				// Adds the command
				_toolBarConfigurationWindow.addCommand(editableToolBarCommand);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s167"));

			} else

			// Name text field is empty
			if (name.matches(""))

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s997"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s995"), JOptionPane.ERROR_MESSAGE);

			// Action text field is empty
			else if (command.matches(""))
				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s998"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s995"), JOptionPane.ERROR_MESSAGE);

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE add tool bar command window cancel button
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
}
