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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.predetermineWindow;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import acide.configuration.workbench.lexiconAssigner.AcideLexiconAssigner;
import acide.files.AcideFileExtensionFilterManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE add lexicon assigner window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideAddLexiconAssignerWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window description label.
	 */
	private JLabel _descriptionLabel;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window description text
	 * field.
	 */
	private JTextField _descriptionTextField;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window extensions label.
	 */
	private JLabel _extensionsLabel;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window extensions text
	 * field.
	 */
	private JTextField _extensionsTextField;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window default lexicon
	 * configuration label.
	 */
	private JLabel _defaultLexiconConfigurationLabel;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window default lexicon
	 * configuration text field.
	 */
	private JTextField _defaultLexiconConfigurationTextField;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window examine button.
	 */
	private JButton _examineButton;
	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window predetermine
	 * lexicon window.
	 */
	private AcideDefaultLexiconsWindow _predetermineLexiconWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE add console panel tool bar
	 * button window.
	 * 
	 * @param predetermineLexiconWindow
	 *            predetermine lexicon window for stores the changes in its
	 *            table model.
	 */
	public AcideAddLexiconAssignerWindow(
			AcideDefaultLexiconsWindow predetermineLexiconWindow) {

		super();

		// Stores the predetermine lexicon window instance
		_predetermineLexiconWindow = predetermineLexiconWindow;

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
	 * Builds the ACIDE - A Configurable IDE add lexicon assigner window
	 * components.
	 */
	private void buildComponents() {

		// Creates the description label
		_descriptionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s1085"), JLabel.LEFT);

		// Creates the description text field
		_descriptionTextField = new JTextField();

		// Creates the extensions label
		_extensionsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s1086"), JLabel.LEFT);

		// Creates the extensions text field
		_extensionsTextField = new JTextField();

		// Creates the default lexicon configuration label
		_defaultLexiconConfigurationLabel = new JLabel(AcideLanguageManager
				.getInstance().getLabels().getString("s1087"), JLabel.LEFT);

		// Creates the default lexicon configuration text field
		_defaultLexiconConfigurationTextField = new JTextField();

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

		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE add lexicon assigner window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s1081"));

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

		// Adds the description label to the main panel
		_mainPanel.add(_descriptionLabel, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;
		constraints.ipady = 5;

		// Adds the description text field to the main panel
		_mainPanel.add(_descriptionTextField, constraints);

		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the extensions label to the main panel
		_mainPanel.add(_extensionsLabel, constraints);

		constraints.gridx = 1;
		constraints.ipady = 5;

		// Adds the extensions text field to the main panel
		_mainPanel.add(_extensionsTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;

		// Adds the default lexicon configuration label to the main panel
		_mainPanel.add(_defaultLexiconConfigurationLabel, constraints);

		constraints.gridx = 1;
		constraints.ipady = 5;

		// Adds the default lexicon configuration text field to the main panel
		_mainPanel.add(_defaultLexiconConfigurationTextField, constraints);

		constraints.gridx = 2;
		constraints.ipady = 0;

		// Adds the examine button to the main panel
		_mainPanel.add(_examineButton, constraints);

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;

		// Adds the main panel to the window
		add(_mainPanel, constraints);

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
	 * Closes the ACIDE - A Configurable IDE add lexicon assigner window.
	 */
	private void closeWindow() {

		// Brings the predetermined lexicon window to the front
		_predetermineLexiconWindow.setAlwaysOnTop(true);

		// Closes the window
		dispose();

		// But not always
		_predetermineLexiconWindow.setAlwaysOnTop(false);
	}

	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window examine button
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

			// Selects the extension for the project
			String[] extensions = new String[] { "xml" };

			File selectedFile = null;
			String absolutePath = null;
		
			// Creates the file chooser
			JFileChooser _fileChooser = new JFileChooser();

			try {

				// Sets the current directory to the default path
				_fileChooser.setCurrentDirectory(new File("./configuration/lexicon/"));

				// Adds the filter to the file chooser
				_fileChooser
						.addChoosableFileFilter(
								new AcideFileExtensionFilterManager(extensions,
										AcideLanguageManager.getInstance()
												.getLabels().getString("s327")));
				
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

						// Updates the default lexicon configuration text field
						_defaultLexiconConfigurationTextField.setText(absolutePath);

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
	 * ACIDE - A Configurable IDE add lexicon assigner window accept button
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

			// Gets the description from the description text field
			String description = _descriptionTextField.getText();

			// Gets the extensions from the extensions text field
			String extensions = _extensionsTextField.getText();

			// Gets the default lexicon configuration from the text field
			String defaultLexiconConfiguration = _defaultLexiconConfigurationTextField
					.getText();

			// No empty description, extension list and default lexicon are
			// accepted
			if (!description.matches("") && !extensions.matches("")
					&& !defaultLexiconConfiguration.matches("")) {

				// Creates the new row
				AcideLexiconAssigner lexiconAssigner = new AcideLexiconAssigner();

				// Sets is description
				lexiconAssigner.setDescription(description);

				// Gets the file extensions into a string array
				String[] extensionsString = extensions.toString().split(";");

				// Creates the parsed extensions
				ArrayList<String> parsedExtensions = new ArrayList<String>();

				// Parse the string array into an array list of strings
				for (int index = 0; index < extensionsString.length; index++)
					parsedExtensions.add(extensionsString[index]);

				// Sets its array list of extensions
				lexiconAssigner.setExtensionList(parsedExtensions);

				// Sets its lexicon configuration
				lexiconAssigner
						.setLexiconConfiguration(defaultLexiconConfiguration);

				// Adds the lexicon assigner
				_predetermineLexiconWindow.addLexiconAssigner(lexiconAssigner);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s167"));

			} else

			// Description text field is empty
			if (description.matches(""))

				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s1088"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s1081"), JOptionPane.ERROR_MESSAGE);

			// Extensions text field is empty
			else if (extensions.matches(""))
				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s1089"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s1081"), JOptionPane.ERROR_MESSAGE);
			// Default lexicon configuration text field is empty
			else if (defaultLexiconConfiguration.matches(""))
				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s1090"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s1081"), JOptionPane.ERROR_MESSAGE);

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE add lexicon assigner window cancel button
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
}