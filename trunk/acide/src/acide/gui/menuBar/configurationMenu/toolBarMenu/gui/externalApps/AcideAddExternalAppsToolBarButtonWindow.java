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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui.externalApps;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import acide.configuration.toolBar.externalAppsToolBar.AcideExternalAppsToolBarButtonConf;
import acide.files.AcideFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.listeners.AcideStatusBarKeyboardListener;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE add external applications tool bar button window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideAddExternalAppsToolBarButtonWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window name text field.
	 */
	private JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window path label.
	 */
	private JLabel _pathLabel;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window path text field.
	 */
	private JTextField _pathTextField;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window hint text label.
	 */
	private JLabel _hintTextLabel;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window hint text text field.
	 */
	private JTextField _hintTextTextField;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window image label.
	 */
	private JLabel _iconLabel;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window image text field.
	 */
	private JTextField _iconTextField;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window main panel.
	 */
	private JPanel _commandPanel;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window examine path button.
	 */
	private JButton _examinePathButton;
	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window examine icon button.
	 */
	private JButton _examineIconButton;

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window.
	 */
	private AcideToolBarConfigurationWindow _toolBarConfigurationWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE add external applications tool
	 * bar button window.
	 * 
	 * @param toolBarConfigurationWindow
	 *            external applications configuration panel for stores the
	 *            changes in its table.
	 */
	public AcideAddExternalAppsToolBarButtonWindow(
			AcideToolBarConfigurationWindow toolBarConfigurationWindow) {

		super();

		// Stores the external applications configuration panel instance
		_toolBarConfigurationWindow = toolBarConfigurationWindow;

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
	 * Builds the ACIDE - A Configurable IDE add external applications tool bar
	 * button window components.
	 */
	private void buildComponents() {

		// Creates the name label
		_nameLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s133"), JLabel.LEFT);

		// Creates the name text field
		_nameTextField = new JTextField();

		// Creates the action label
		_pathLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s134"), JLabel.LEFT);

		// Creates the action text field
		_pathTextField = new JTextField();

		// Creates the examine path button
		_examinePathButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s142"));

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

		// Creates the examine icon button
		_examineIconButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s142"));

		// Sets the examine icon button tool tip text
		_examineIconButton.setToolTipText(AcideLanguageManager.getInstance()
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
	 * Sets the ACIDE - A Configurable IDE add external applications tool bar
	 * button window configuration.
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
	 * Adds the components to the ACIDE - A Configurable IDE add external
	 * applications tool bar button window with the layout.
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
		_commandPanel.add(_pathLabel, constraints);

		constraints.gridx = 1;
		constraints.ipady = 5;

		// Adds the action text field to the command panel
		_commandPanel.add(_pathTextField, constraints);

		constraints.gridx = 2;
		constraints.ipady = 0;

		// Adds the examine path button to the command panel
		_commandPanel.add(_examinePathButton, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;

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

		// Adds the examine icon button to the command panel
		_commandPanel.add(_examineIconButton, constraints);

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
	 * Sets the listeners to the ACIDE - A Configurable IDE add external
	 * applications tool bar button window components.
	 */
	private void setListeners() {

		// Sets the examine path button action listener
		_examinePathButton.addActionListener(new ExaminePathButtonAction());

		// Sets the examine icon button action listener
		_examineIconButton.addActionListener(new ExamineIconButtonAction());

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener(
				_toolBarConfigurationWindow));

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
	 * Closes the ACIDE - A Configurable IDE add external applications tool bar
	 * button window.
	 */
	private void closeWindow() {

		// Enables the tool bar configuration window
		_toolBarConfigurationWindow.setEnabled(true);

		// Closes the window
		dispose();

		// Brings it to the front
		_toolBarConfigurationWindow.setAlwaysOnTop(true);

		// But not always
		_toolBarConfigurationWindow.setAlwaysOnTop(false);

	}

	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window examine path button action listener.
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
					AcideFileOperation.OPEN, AcideFileTarget.FILES,
					AcideFileType.FILE, "", null);

			if (absolutePath != null) {

				// Updates the path text field with the absolute path
				_pathTextField.setText(absolutePath);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window examine button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineIconButtonAction implements ActionListener {

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
					AcideFileOperation.OPEN, AcideFileTarget.FILES,
					AcideFileType.FILE, "", null);

			if (absolutePath != null) {

				// Updates the icon text field with the absolute path
				_iconTextField.setText(absolutePath);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window accept button action listener.
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

			// Gets the path from the path text field
			String path = _pathTextField.getText();

			// Gets the hint text from the hint text text field
			String hintText = _hintTextTextField.getText();

			// Gets the icon from the icon text field
			String icon = _iconTextField.getText();

			// No empty name and command are accepted
			if (!name.matches("") && !path.matches("")) {

				// Creates the new row
				AcideExternalAppsToolBarButtonConf buttonConfiguration = new AcideExternalAppsToolBarButtonConf(
						name, path, hintText, icon);

				// Adds the button configuration in the external applications in
				// the tool bar configuration window
				_toolBarConfigurationWindow.getExternalAppsConfigurationPanel()
						.addButtonConfiguration(buttonConfiguration);

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
			else if (path.matches(""))
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
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window cancel button action listener.
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
	 * ACIDE - A Configurable IDE add external applications tool bar button
	 * window escape key action.
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
