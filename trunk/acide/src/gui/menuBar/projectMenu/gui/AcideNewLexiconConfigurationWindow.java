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
package gui.menuBar.projectMenu.gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.AcideLanguageManager;
import operations.factory.AcideGUIFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.text.AcideValidExtensionsManager;
import gui.mainWindow.MainWindow;

/**
 * ACIDE - A Configurable IDE new lexicon configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideNewLexiconConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window name panel.
	 */
	private JPanel _namePanel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window configuration
	 * panel.
	 */
	private JPanel _configurationPanel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window extensions
	 * panel.
	 */
	private JPanel _extensionsPanel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window type panel.
	 */
	private JPanel _typePanel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window accept
	 * button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window cancel
	 * button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window name text
	 * field.
	 */
	private final JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window lexicon
	 * configuration button.
	 */
	private JButton _lexiconConfigurationButton;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window grammar
	 * configuration button.
	 */
	private JButton _grammarConfigurationButton;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window files label.
	 */
	private JLabel _filesLabel;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window files text
	 * field.
	 */
	private final JTextField _filesTextField;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window compiled
	 * radio button
	 */
	private final JRadioButton _compiledRadioButton;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window interpreted
	 * radio button
	 */
	private JRadioButton _interpretedRadioButton;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window button group
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window undo path.
	 */
	private String _undoPath = "";

	/**
	 * Creates a new ACIDE - A Configurable IDE new lexicon configuration
	 * window.
	 */
	public AcideNewLexiconConfigurationWindow() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s351"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// NAME PANEL
		_namePanel = new JPanel();
		_namePanel.setLayout(new GridBagLayout());
		_namePanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s353"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// CONFIGURATION PANEL
		_configurationPanel = new JPanel();
		_configurationPanel.setLayout(new GridBagLayout());
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s354"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// EXTENSIONS PANEL
		_extensionsPanel = new JPanel();
		_extensionsPanel.setLayout(new GridLayout(0, 1));
		_extensionsPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s355"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// TYPE PANEL
		_typePanel = new JPanel();
		_typePanel.setLayout(new GridLayout(1, 0));
		_typePanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s356"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// NAME
		_nameLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s357"), JLabel.CENTER);
		_nameTextField = new JTextField();
		_nameTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s358"));

		// LEXICON CONFIGURATION BUTTON
		_lexiconConfigurationButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s359"));
		_lexiconConfigurationButton.setHorizontalAlignment(JButton.CENTER);
		_lexiconConfigurationButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s360"));

		// GRAMMAR CONFIGURATION BUTTON
		_grammarConfigurationButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s361"));
		_grammarConfigurationButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s362"));
		_grammarConfigurationButton.setHorizontalAlignment(JButton.CENTER);

		// FILES
		_filesLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s363"), JLabel.CENTER);
		_filesTextField = new JTextField();
		_filesTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s364"));

		// COMPILED
		_compiledRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s365"));
		_compiledRadioButton.setHorizontalAlignment(JRadioButton.CENTER);

		// INTERPRETED
		_interpretedRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s366"));
		_interpretedRadioButton.setHorizontalAlignment(JRadioButton.CENTER);

		// RADIO GROUP
		_buttonGroup = new ButtonGroup();
		_buttonGroup.add(_compiledRadioButton);
		_buttonGroup.add(_interpretedRadioButton);

		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s367"));
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s368"));
		_acceptButton.setHorizontalAlignment(JButton.CENTER);

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s369"));
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s370"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// NAME PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_namePanel.add(_nameLabel, constraints);
		constraints.ipadx = 100;
		constraints.gridx = 1;
		_namePanel.add(_nameTextField, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_namePanel, constraints);

		// CONFIGURATION PANEL
		constraints.gridx = 0;
		constraints.gridy = 0;
		_configurationPanel.add(_lexiconConfigurationButton, constraints);
		// constraints.gridx = 1;
		// constraints.gridy = 0;
		// constraints.insets = new Insets(5, 40, 5, 5);
		// _configurationPanel.add(_grammarConfigurationButton, constraints);
		// constraints.insets = new Insets(5, 5, 5, 5);
		// constraints.gridx = 0;
		// constraints.gridy = 1;
		add(_configurationPanel, constraints);

		// EXTENSIONS PANEL
		_extensionsPanel.add(_filesLabel);
		_extensionsPanel.add(_filesTextField);
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_extensionsPanel, constraints);

		// TYPE PANEL
		_typePanel.add(_compiledRadioButton);
		_typePanel.add(_interpretedRadioButton);
		constraints.gridx = 0;
		constraints.gridy = 3;
		add(_typePanel, constraints);

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		constraints.gridx = 0;
		constraints.gridy = 4;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s352"));
		setIconImage(ICON.getImage());
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s373"));
	}

	/**
	 * Sets the components of the window components.
	 */
	private void setListeners() {

		// LEXICON CONFIGURATION BUTTON
		_lexiconConfigurationButton
				.addActionListener(new LexiconConfigurationButtonAction());

		// GRAMMAR CONFIGURATION BUTTON
		_grammarConfigurationButton
				.addActionListener(new GrammarConfigurationButtonAction());

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());
	}

	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window lexicon
	 * configuration button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class LexiconConfigurationButtonAction implements ActionListener {

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
				_undoPath = AcideResourceManager.getInstance().getProperty(
						"languagePath");
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Sets the new lexicon configuration name
			AcideLexiconConfiguration.getInstance().setName(
					_nameTextField.getText());

			// Shows the ACIDE - A Configurable IDE lexicon configuration window
			AcideGUIFactory.getInstance()
					.buildAcideLexiconConfigurationWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window grammar
	 * configuration button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class GrammarConfigurationButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Shows the ACIDE - A Configurable IDE grammar configuration window
			AcideGUIFactory.getInstance().buildAcideGrammarConfigurationWindow(
					false);
		}
	}

	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the name from the text field
			String fileName = _nameTextField.getText();

			// Adds the file extensions to the valid extensions
			AcideValidExtensionsManager.getInstance().tokenizeExtensions(
					_filesTextField.getText());

			// Save the configuration
			AcideLexiconConfiguration programmingLanguage = AcideLexiconConfiguration
					.getInstance();
			programmingLanguage.save(fileName,
					_compiledRadioButton.isSelected());

			// If there is an opened project window configuration
			if (MainWindow.getInstance().getNewProjectWindowConfiguration() != null) {

				// Set the lexicon configuration in the new project window
				MainWindow
						.getInstance()
						.getNewProjectWindowConfiguration()
						.setLexiconConfigurationName(
								programmingLanguage.getName());

				// Updates the lexicon configuration label in the new project
				// window
				MainWindow
						.getInstance()
						.getNewProjectWindowConfiguration()
						.setLexiconConfigurationNameLabel(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s599")
										+ MainWindow
												.getInstance()
												.getNewProjectWindowConfiguration()
												.getLexiconConfigurationName());

				// Updates the lexicon message in the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setLexiconMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s449")
										+ " " + programmingLanguage.getName());

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s371")
								+ _nameTextField.getText());
			}

			// Closes the configuration window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE new lexicon configuration window cancel button
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

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s372"));

			try {
				AcideLexiconConfiguration.getInstance().load(_undoPath);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Closes the configuration window
			dispose();
		}
	}
}
