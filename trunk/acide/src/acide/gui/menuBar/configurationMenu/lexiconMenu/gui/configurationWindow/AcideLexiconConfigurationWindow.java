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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.delimiters.AcideDelimitersPanel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.delimiters.utils.AcideDelimitersPanelTableModel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.remarks.AcideRemarksPanel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.reserverdWords.AcideReservedWordsPanel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideSaveLexiconAsMenuItemListener;
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
	 * ACIDE - A Configurable IDE lexicon configuration window tabbed pane.
	 */
	private JTabbedPane _tabbedPane;
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
	 * ACIDE - A Configurable IDE lexicon configuration window save as button.
	 */
	private JButton _saveAsButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration temporal file path.
	 */
	private String _temporalPath;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration window.
	 */
	public AcideLexiconConfigurationWindow() {

		super();

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s376"));

		// Gets the temporal path from the temporal file
		_temporalPath = AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getLexiconConfiguration()
				.saveTemporalFile(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getLexiconConfiguration().getName(), false);

		// Builds the window components
		buildComponents();

		// Adds the components to the window
		addComponents();

		// Sets the listeners of the window components
		setListeners();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE lexicon configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s377")
				+ " - "
				+ AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getName());

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// Does not anything on window closing
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

		// Sets the window not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Sets the window visible
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s420"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the tabbed pane
		_tabbedPane = new JTabbedPane();

		// Creates the reserved words panel
		_reservedWordsPanel = new AcideReservedWordsPanel();

		// Adds the reserved words panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s428"), _reservedWordsPanel);

		// Creates the delimiters panel
		_delimitersPanel = new AcideDelimitersPanel();

		// Adds the delimiters panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s429"), _delimitersPanel);

		// Creates the remarks panel
		_remarksPanel = new AcideRemarksPanel(_reservedWordsPanel);

		// Adds the remarks panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s430"), _remarksPanel);

		// Builds the button panel
		buildButtonPanel();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE lexicon
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new BorderLayout());

		// Creates the tables to display
		addTablesToWindow();

		// Adds the tabbed pane to the window
		add(_tabbedPane, BorderLayout.CENTER);

		// Adds the button panel to the window
		add(_buttonPanel, BorderLayout.SOUTH);
	}

	/**
	 * Builds the button panel and its components.
	 */
	private void buildButtonPanel() {

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the apply button
		_applyButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s434"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s435"));

		// Creates the save as button
		_saveAsButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s285"));

		// Adds the save as button to the button panel
		_buttonPanel.add(_saveAsButton);

		// Adds the apply buttons to the button panel
		_buttonPanel.add(_applyButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the save as button action listener
		_saveAsButton
				.addActionListener(new AcideSaveLexiconAsMenuItemListener());

		// Sets the apply button action listener
		_applyButton.addActionListener(new ApplyButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the user press the escape key then the escape action is fired
		_cancelButton.registerKeyboardAction(new EscapeAction(), "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Adds the window listener
		addWindowListener(new AcideLexiconConfigurationWindowListener());
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
		_delimitersPanel.add(_delimitersPanel.buildTable(), constraints);
	}

	/**
	 * <p>
	 * Applies the changes in the ACIDE - A Configurable IDE lexicon
	 * configuration and saves it automatically.
	 * </p>
	 * <p>
	 * Updates the the ACIDE - A Configurable IDE lexicon configuration on the
	 * opened file editors that have the current modified ACIDE - A Configurable
	 * IDE lexicon configuration. Aditionally, it also updates the ACIDE - A
	 * Configurable IDE console panel lexicon configuration if it matches with
	 * the current modified ACIDE - A Configurable IDE lexicon configuration.
	 * </p>
	 * <p>
	 * First, it modifies the selected file editor panel lexicon configuration
	 * and updates the configuration file. Then the others just load the
	 * configuration from the new file configuration in order to apply the
	 * changes on them.
	 * </p>
	 */
	private void applyChanges() {

		// Updates its delimiters manager
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getLexiconConfiguration()
				.getDelimitersManager()
				.setList(
						((AcideDelimitersPanelTableModel) _delimitersPanel
								.getTable().getModel()).getItems());

		// Updates its remarks manager symbol
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getRemarksManager()
				.setSymbol(_remarksPanel.getRemarkSymbolTextField().getText());

		// Updates its remarks manager foreground color
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getRemarksManager()
				.setColor(_remarksPanel.getPreviewTextField().getForeground());

		// Updates its remarks manager is case sensitive flag
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getLexiconConfiguration()
				.getRemarksManager()
				.setIsCaseSensitive(
						_remarksPanel.getIsCaseSensitiveCheckBox().isSelected());

		// Updates its remarks manager style
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getLexiconConfiguration()
				.getRemarksManager()
				.setFontStyle(
						_remarksPanel.getPreviewTextField().getFont()
								.getStyle());

		// Resets the selected file editor text edition area
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().resetStyledDocument();

		// Saves the changes automatically into its file
		saveLexicon();

		// Gets the lexicon configuration name
		String lexiconConfigurationName = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getLexiconConfiguration().getName();

		// Updates the opened file editors
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			// If the current checked file editor has the modified lexicon
			// configuration
			if (index != AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanelIndex()
					&&

					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index)
							.getLexiconConfiguration().getName()
							.matches(lexiconConfigurationName)) {

				// Loads the lexicon configuration
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(index)
						.getLexiconConfiguration()
						.load(AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(index)
								.getLexiconConfiguration().getPath());

				// Resets the selected file editor text edition area
				AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).resetStyledDocument();
			}
		}

		// If the current console panel lexicon configuration matches with the
		// selected editor
		if (AcideMainWindow.getInstance().getConsolePanel()
				.getLexiconConfiguration().getName()
				.matches(lexiconConfigurationName)) {

			// Loads the lexicon configuration in the ACIDE - A Configurable IDE
			// console panel
			AcideMainWindow
					.getInstance()
					.getConsolePanel()
					.getLexiconConfiguration()
					.load(AcideMainWindow.getInstance().getConsolePanel()
							.getLexiconConfiguration().getPath());

			// Applies the highlighting to the console panel
			AcideMainWindow.getInstance().getConsolePanel()
					.resetStyledDocument();
		}
	}

	/**
	 * Closes the ACIDE - A Configurable IDE lexicon configuration window.
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
	 * Asks to the user for saving the changes if any to apply them or cancel
	 * it.
	 */
	private void askForSavingChanges() {

		boolean isCancelSelected = false;

		// If there are changes in any of the panels
		if (_reservedWordsPanel.getAreThereChanges()
				|| _delimitersPanel.getAreThereChanges()
				|| _remarksPanel.getAreThereChanges()) {

			// Asks the user if wants to save the changes
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s1068"), AcideLanguageManager
							.getInstance().getLabels().getString("s1067"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not the cancel or the closed option
			if (returnValue != JOptionPane.CANCEL_OPTION
					&& returnValue != JOptionPane.CLOSED_OPTION) {

				// If it is yes
				if (returnValue == JOptionPane.YES_OPTION)

					// Applies the changes
					applyChanges();

				// If it is no
				else if (returnValue == JOptionPane.NO_OPTION)
					// Performs the cancel button action
					_cancelButton.doClick();
			} else
				isCancelSelected = true;
		}

		if (!isCancelSelected) {

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE lexicon configuration into its file.
	 */
	public void saveLexicon() {

		// Gets the lexicon configuration path
		String path = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getPath();

		// If it is ok
		if (!path.equals(" ")) {

			// Gets the lexicon name
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String name = path.substring(index + 1, path.length());

			if (name.contains(".")) {
				index = name.lastIndexOf(".");
				name = name.substring(0, index);
			}

			// Saves it
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getLexiconConfiguration()
					.save(name, false);

		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s92"));
		}
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

			// Asks to the user for saving the changes
			askForSavingChanges();
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

			// Applies the changes
			applyChanges();

			// Closes the window
			closeWindow();

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

				// Loads the temporal file into the lexicon configuration once
				// again
				// to restore the previous lexicon configuration
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.loadTemporalFile(_temporalPath);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE lexicon configuration window window listener.
	 * 
	 * @version 0.8
	 * @see WindowAdapter
	 */
	class AcideLexiconConfigurationWindowListener extends WindowAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
		 * )
		 */
		public void windowClosing(WindowEvent windowEvent) {

			// Asks to the user for saving the changes
			askForSavingChanges();
		}
	}
}