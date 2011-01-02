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
package gui.menuBar.configurationMenu.lexiconMenu.gui;

import es.configuration.lexicon.AcideLexiconConfiguration;
import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.lexiconMenu.gui.panels.AcideDelimitersPanel;
import gui.menuBar.configurationMenu.lexiconMenu.gui.panels.AcideRemarksPanel;
import gui.menuBar.configurationMenu.lexiconMenu.gui.panels.AcideReservedWordsPanel;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import language.AcideLanguageManager;
import operations.lexicon.Remark;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE lexicon configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideLexiconConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window reserved words panel.
	 */
	private AcideReservedWordsPanel _reservedWordsPanel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window delimiters panel.
	 */
	private AcideDelimitersPanel _delimitersPanel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window remarks panel.
	 */
	private AcideRemarksPanel _remarksPanel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window right panel which
	 * contains the delimiters and remarks panels.
	 */
	private JPanel _delimitersAndRemarksPanel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window apply button.
	 */
	private JButton _applyButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window cancel button.
	 */
	private JButton _cancelButton;
	
	/**
	 * Creates a new lexicon configuration window.
	 */
	public AcideLexiconConfigurationWindow() {

		super();
		
		// Gets the labels
		final ResourceBundle _labels = AcideLanguageManager.getInstance().getLabels();
		
		// Updates the log
		AcideLog.getLog().info(_labels.getString("s376"));

		final String tempPath = AcideLexiconConfiguration.getInstance().saveTemp(
				AcideLexiconConfiguration.getInstance().getName(), false);

		// FRAME
		addWindowListener(new java.awt.event.WindowAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
			 * )
			 */
			@Override
			public void windowClosing(WindowEvent windowEvent) {
				try {
					
					// Loads the lexicon configuration in the project configuration
					AcideLexiconConfiguration.getInstance().load(
							AcideResourceManager.getInstance().getProperty(
									"languagePath"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		});

		// Sets the layout
		setLayout(new GridBagLayout());

		// RESERVED WORDS PANEL
		_reservedWordsPanel = new AcideReservedWordsPanel();

		// DELIMITERS PANEL
		_delimitersPanel = new AcideDelimitersPanel();

		// REMARKS PANEL
		_remarksPanel = new AcideRemarksPanel(_reservedWordsPanel);
		
		// RIGHT PANEL
		_delimitersAndRemarksPanel = new JPanel(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// APPLY BUTTON
		_applyButton = new JButton();
		_applyButton.setText(_labels.getString("s434"));
		_applyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the text from the remarks panel text field 
				Remark.getInstance().setContent(
						_remarksPanel.getRemarkSymbolTextField().getText());
				
				// Gets the color from the remarks panel preview text field 
				Remark.getInstance().setColor(
						_remarksPanel.getPreviewTextField().getForeground());

				// Gets the number of opened file editor panels
				int numFileEditorPanels = MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels();

				// Resets all the opened file editor panels with the new lexical configuration
				for (int index = 0; index < numFileEditorPanels; index++)
					MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(index)
							.resetDocument();

				// Closes the window
				dispose();
				
				// Updates the lexicon message in the status bar 
				MainWindow
						.getInstance()
						.getStatusBar()
						.setLexiconMessage(
								_labels.getString("s449")
										+ " "
										+ AcideLexiconConfiguration.getInstance()
												.getName());
			}
		});

		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(_labels.getString("s435"));
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {

				try {
					
					// Loads the temp file
					AcideLexiconConfiguration.getInstance().loadTemp(tempPath);
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
				
				// Closes the window
				dispose();
			}
		});

		// Adds the escape key listener to the cancel button
		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// Closes the window
				dispose();
			}
		};

		// When the user press the escape key then the cancel button is fired
		_cancelButton.registerKeyboardAction(actionListener, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Creates the tables to display
		addTablesToWindow();

		// Adds the components to the frame with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// RESERVED WORDS PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		add(_reservedWordsPanel, constraints);

		// RIGHT PANEL
		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_delimitersAndRemarksPanel.add(_delimitersPanel, constraints);
		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_delimitersAndRemarksPanel.add(_remarksPanel, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.ipady = 0;
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		add(_delimitersAndRemarksPanel, constraints);

		// APPLY BUTTON
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.gridwidth = 2;
		_buttonPanel.add(_applyButton);
		_buttonPanel.add(_cancelButton);
		add(_buttonPanel, constraints);

		// Add the window listener
		addWindowListener(new AcideWindowListener());
		
		// FRAME
		setTitle(_labels.getString("s377") + " - "
				+ AcideLexiconConfiguration.getInstance().getName());
		setIconImage(ICON.getImage());
		setVisible(true);
		setResizable(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
		
		// Updates the log
		AcideLog.getLog().info(_labels.getString("s420"));
	}

	/**
	 * Adds the list tables to the window with the layout.
	 */
	public void addTablesToWindow() {

		GridBagConstraints constraints = new GridBagConstraints();

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 7;
		constraints.gridwidth = 2;
		_reservedWordsPanel.add(_reservedWordsPanel.getWordListTable(), constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_delimitersPanel.add(_delimitersPanel.buildDelimiterListTable(), constraints);
	}
}