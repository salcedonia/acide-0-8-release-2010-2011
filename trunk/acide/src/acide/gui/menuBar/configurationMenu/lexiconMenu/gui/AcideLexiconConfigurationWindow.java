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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.delimiters.AcideDelimitersPanel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.remarks.AcideRemarksPanel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.reserverdWords.AcideReservedWordsPanel;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE lexicon configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideLexiconConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window reserved words
	 * panel.
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
	 * Temporal path while the lexicon configuration.
	 */
	private String _temporalPath;

	/**
	 * Creates a new lexicon configuration window.
	 */
	public AcideLexiconConfigurationWindow() {

		super();

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s376"));

		// Sets the layout
		setLayout(new GridBagLayout());
		
		// Enables the main window again
		AcideMainWindow.getInstance().setEnabled(false);
		
		// Gets the temporal path
		_temporalPath = AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getLexiconConfiguration()
				.saveTemp(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getLexiconConfiguration().getName(), false);

		// Creates the reserved words panel
		_reservedWordsPanel = new AcideReservedWordsPanel();

		// Creates the delimiters panel
		_delimitersPanel = new AcideDelimitersPanel();

		// Creates the remarks panel
		_remarksPanel = new AcideRemarksPanel(_reservedWordsPanel);

		// Creates the delimiters and remarks panel
		_delimitersAndRemarksPanel = new JPanel(new GridBagLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the apply button
		_applyButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s434"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s435"));

		// Sets the listeners of the window components
		setListeners();

		// Creates the tables to display
		addTablesToWindow();

		// Adds the components to the frame with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		constraints.ipady = 0;

		// Adds the reserved words panel to the window
		add(_reservedWordsPanel, constraints);

		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;

		// Adds the delimiters panel to the delimiters and remarks panel
		_delimitersAndRemarksPanel.add(_delimitersPanel, constraints);

		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;

		// Adds the remarks panel to the delimiters and remarks panel
		_delimitersAndRemarksPanel.add(_remarksPanel, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.ipady = 0;
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.ipadx = 0;

		// Adds the delimiters and remarks panel to the window
		add(_delimitersAndRemarksPanel, constraints);

		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.gridwidth = 2;

		// Adds the apply buttons to the button panel
		_buttonPanel.add(_applyButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		// Adds the button panel to the window
		add(_buttonPanel, constraints);

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s377")
				+ " - "
				+ AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getName());

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// Sets the window not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Sets the window visible
		setVisible(true);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s420"));
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the apply button action listener
		_applyButton.addActionListener(new ApplyButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the user press the escape key then the escape action is fired
		_cancelButton.registerKeyboardAction(new EscapeAction(), "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);
	}

	/**
	 * Adds the list tables to the window with the layout.
	 */
	public void addTablesToWindow() {

		// Adds the table to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 7;
		constraints.gridwidth = 2;

		// Adds the reserved words table to the reserved words panel
		_reservedWordsPanel.add(_reservedWordsPanel.buildTable(), constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 2;

		// Adds the delimiters table to the delimiters panel
		_delimitersPanel.add(_delimitersPanel.buildDelimiterListTable(),
				constraints);
	}

	/**
	 * ACIDE - A Configurable IDE lexicon configuration window escape action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
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

	/**
	 * ACIDE - A Configurable IDE lexicon configuration window apply button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ApplyButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the text from the remarks panel text field
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getLexiconConfiguration()

					.getRemarksManager()
					.setSymbol(
							_remarksPanel.getRemarkSymbolTextField().getText());

			// Gets the color from the remarks panel preview text field
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getLexiconConfiguration()
					.getRemarksManager()
					.setColor(
							_remarksPanel.getPreviewTextField().getForeground());

			// Gets the is case sensitive from the remarks panel is case
			// sensitive check box
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getLexiconConfiguration()
					.getRemarksManager()
					.setIsCaseSensitive(
							_remarksPanel.getIsCaseSensitiveCheckBox()
									.isSelected());

			// Gets the font style from the remarks panel preview text field
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getLexiconConfiguration()
					.getRemarksManager()
					.setFontStyle(
							_remarksPanel.getPreviewTextField().getFont()
									.getStyle());

			// Gets the selected file editor panel caret position
			int caretPosition = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getCaretPosition();

			// Resets the selected file editor text edition area
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.resetStyledDocument(caretPosition);

			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);

			// Updates the lexicon message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setLexiconMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s449")
									+ " "
									+ AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getLexiconConfiguration()
											.getName());
		}
	}

	/**
	 * ACIDE - A Configurable IDE lexicon configuration window cancel button
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
		public void actionPerformed(ActionEvent actionEvent) {

			try {

				// Loads the temp file
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.loadTemp(_temporalPath);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

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