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
package acide.gui.statusBarPanel;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.statusBarPanel.listeners.AcideStatusBarPopupMenuListener;
import acide.gui.statusBarPanel.popup.AcideStatusBarPopupMenu;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

import acide.log.AcideLog;
import acide.utils.AcideOSChecker;

/**
 * ACIDE - A Configurable IDE status bar.
 * 
 * @version 0.8
 */
public class AcideStatusBar extends JPanel {

	/**
	 * ACIDE - A Configurable IDE status bar class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE status bar status message label.
	 */
	private JLabel _statusMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar status message panel.
	 */
	private JPanel _statusMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar line and column message label.
	 */
	private JLabel _lineAndColumnMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar line and column message panel.
	 */
	private JPanel _lineAndColumnMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar lexicon message label.
	 */
	private JLabel _lexiconMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar lexicon message panel.
	 */
	private JPanel _lexiconMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar grammar message label.
	 */
	private JLabel _grammarMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar grammar message panel.
	 */
	private JPanel _grammarMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar CAPS LOCK message label.
	 */
	private JLabel _capsLockMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar CAPS LOCK message panel.
	 */
	private JPanel _capsLockMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar NUM LOCK message label.
	 */
	private JLabel _numLockMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar NUM LOCK message panel.
	 */
	private JPanel _numLockMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar SCROLL LOCK message label.
	 */
	private JLabel _scrollLockMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar SCROLL LOCK message panel.
	 */
	private JPanel _scrollLockMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar timer message label.
	 */
	private JLabel _timerMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar timer message panel.
	 */
	private JPanel _timerMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar number of lines message label.
	 */
	private JLabel _numberOfLinesMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar number of lines message panel.
	 */
	private JPanel _numberOfLinesMessagePanel;
	/**
	 * ACIDE - A Configurable IDE status bar edition mode message label.
	 */
	private JLabel _editionModeMessageLabel;
	/**
	 * ACIDE - A Configurable IDE status bar edition mode message panel.
	 */
	private JPanel _editionModeMessagePanel;

	/**
	 * ACIDE - A Configurable IDE status bar popup menu of the status bar.
	 */
	private AcideStatusBarPopupMenu _popup;

	/**
	 * Creates a new ACIDE - A Configurable IDE status bar.
	 */
	public AcideStatusBar() {

		super();

		// Builds the status bar components
		buildComponents();

		// Adds the components to the status bar
		addComponents();

		// Sets the status bar listener
		setListeners();

		// Sets the hand cursor to the status bar components
		setCursors();
	}

	/**
	 * Sets the cursors to the ACIDE - A Configurable IDE status bar components.
	 */
	private void setCursors() {

		try {

			// Gets the hand cursor
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);

			// Sets the hand cursor
			_numLockMessageLabel.setCursor(cursor);

			// Sets the hand cursor
			_timerMessageLabel.setCursor(cursor);

			// Sets the hand cursor
			_capsLockMessageLabel.setCursor(cursor);

			// Sets the hand cursor to the LOCK
			_scrollLockMessageLabel.setCursor(cursor);

			// Sets the text cursor to the status message label
			_statusMessageLabel.setCursor(Cursor
					.getPredefinedCursor(Cursor.TEXT_CURSOR));
		} catch (HeadlessException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar components.
	 */
	private void buildComponents() {

		// Builds the status message panel
		buildStatusMessagePanel();

		// Builds the lexicon message panel
		buildLexiconMessagePanel();

		// Builds the grammar message panel
		buildGrammarMessagePanel();

		// Builds the line and column message panel
		buildLineAndColumnMessagePanel();

		// Builds the number of lines message panel
		buildNumberOfLinesMessagePanel();

		// Builds the CAPS LOCK message panel
		buildCapsLockMessagePanel();

		// Builds the NUM LOCK message panel
		buildNumLockMessagePanel();

		// Builds the SCROLL LOCK message panel
		buildScrollLockMessagePanel();

		// Builds the edition mode message panel
		buildEditionModeMessagePanel();

		// Builds the timer message panel
		buildTimerMessagePanel();

		// Builds the popup menu
		buildPopupMenu();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar timer message panel.
	 */
	private void buildTimerMessagePanel() {

		// Creates the timer message panel
		_timerMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the timer message panel border
		_timerMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the timer message label
		_timerMessageLabel = new JLabel(" ");

		// Sets the timer message label foreground color
		_timerMessageLabel.setForeground(Color.BLACK);

		// Adds the timer message label to the timer message panel
		_timerMessagePanel.add(_timerMessageLabel);

		// Sets the timer message label tool tip text
		_timerMessageLabel.setToolTipText(DateFormat.getInstance().format(
				new Date()));

		// Initializes the timer for updating the timer message label
		new Timer(1000, new ActionListener() {

			/**
			 * For formatting the date to display into a "HH:mm:ss" format.
			 */
			private DateFormat _dateFormat = new SimpleDateFormat("HH:mm:ss");

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Sets the timer message label with the current format time
				_timerMessageLabel.setText(_dateFormat.format(new Date()));
			}
		}).start();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE edition mode message panel.
	 */
	private void buildEditionModeMessagePanel() {

		// Creates the edition mode message panel
		_editionModeMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the edition mode message panel border
		_editionModeMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the edition mode message label
		_editionModeMessageLabel = new JLabel();

		// Updates the edition mode message label text
		if (AcideWorkbenchConfiguration.getInstance()
				.getFileEditorConfiguration().getEditionMode())
			_editionModeMessageLabel.setText("INS");
		else
			_editionModeMessageLabel.setText("   ");

		// Sets the edition mode message label tool tip text
		_editionModeMessageLabel.setToolTipText("INS");

		// Sets the edition mode message label foreground color
		_editionModeMessageLabel.setForeground(Color.BLACK);

		// Adds the edition mode message label to the edition mode message panel
		_editionModeMessagePanel.add(_editionModeMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar SCROLL LOCK message
	 * panel.
	 */
	private void buildScrollLockMessagePanel() {

		// Creates the SCROLL LOCK panel
		_scrollLockMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the SCROLL LOCK panel border
		_scrollLockMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the SCROLL LOCK label
		_scrollLockMessageLabel = new JLabel();

		// SCROLL LOCK ONLY VALID IN WINDOWS
		if (AcideOSChecker.isWindows()) {

			// Gets the SCROLL LOCK state
			boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_SCROLL_LOCK);

			// Updates the SCROLL LOCK message label text
			if (state)
				_scrollLockMessageLabel.setText("SCROLL");
			else
				_scrollLockMessageLabel.setText("     ");
		} else
			_scrollLockMessageLabel.setText("     ");

		// Sets the SCROLL LOCK message label tool tip text
		_scrollLockMessageLabel.setToolTipText("SCROLL");

		// Sets the SCROLL LOCK message label foreground color
		_scrollLockMessageLabel.setForeground(Color.BLACK);

		// Adds the SCROLL LOCK message label to the SCROLL LOCK message panel
		_scrollLockMessagePanel.add(_scrollLockMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar NUM LOCK message panel.
	 */
	private void buildNumLockMessagePanel() {

		// Creates the NUM LOCK message panel
		_numLockMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the NUM LOCK message panel border
		_numLockMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the NUM LOCK message label
		_numLockMessageLabel = new JLabel();

		// NUM LOCK ONLY VALID IN WINDOWS
		if (AcideOSChecker.isWindows()) {

			// Gets the NUM LOCK state
			boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_NUM_LOCK);

			// Updates the NUM LOCK message label text
			if (state)
				_numLockMessageLabel.setText("NUM");
			else
				_numLockMessageLabel.setText("   ");
		} else
			_numLockMessageLabel.setText("   ");

		// Sets the NUM LOCK message label tool tip text
		_numLockMessageLabel.setToolTipText("NUM");

		// Sets the NUM LOCK message label foreground color
		_numLockMessageLabel.setForeground(Color.BLACK);

		// Adds the NUM LOCK message label to the NUM LOCK message panel
		_numLockMessagePanel.add(_numLockMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar CAPS LOCK message panel.
	 */
	private void buildCapsLockMessagePanel() {

		// Creates the CAPS LOCK message panel
		_capsLockMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the CAPS LOCK message panel border
		_capsLockMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the CAPS LOCK message label
		_capsLockMessageLabel = new JLabel();

		// CAPS LOCK ONLY VALID IN WINDOWS
		if (AcideOSChecker.isWindows()) {

			// Gets the CAPS LOCK state
			boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_CAPS_LOCK);

			// Updates the CAPS LOCK message label text
			if (state)
				_capsLockMessageLabel.setText("CAPS");
			else
				_capsLockMessageLabel.setText("    ");
		} else
			_capsLockMessageLabel.setText("    ");

		// Sets the CAPS LOCK message label tool tip text
		_capsLockMessageLabel.setToolTipText("CAPS");

		// Sets the CAPS LOCK message label foreground color
		_capsLockMessageLabel.setForeground(Color.BLACK);

		// Adds the CAPS LOCK message label to the CAPS LOCK message panel
		_capsLockMessagePanel.add(_capsLockMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar number of lines message
	 * panel.
	 */
	private void buildNumberOfLinesMessagePanel() {

		// Creates the number of lines message panel
		_numberOfLinesMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the number of lines message panel border
		_numberOfLinesMessagePanel
				.setBorder(BorderFactory.createEtchedBorder());

		// Creates the number of lines message label
		_numberOfLinesMessageLabel = new JLabel(" ");

		// Adds the number of lines message label to the number of lines message
		// panel
		_numberOfLinesMessagePanel.add(_numberOfLinesMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar line and column message
	 * panel.
	 */
	private void buildLineAndColumnMessagePanel() {

		// Creates the line and column message panel
		_lineAndColumnMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the line and column message panel border
		_lineAndColumnMessagePanel
				.setBorder(BorderFactory.createEtchedBorder());

		// Creates the line and column message label
		_lineAndColumnMessageLabel = new JLabel(" ");

		// Adds the line and column message label to the line and column message
		// panel
		_lineAndColumnMessagePanel.add(_lineAndColumnMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar grammar message panel.
	 */
	private void buildGrammarMessagePanel() {

		// Creates the grammar message panel
		_grammarMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the grammar message panel border
		_grammarMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the lexicon message label
		_grammarMessageLabel = new JLabel(" ");

		// Adds the grammar message label to the grammar message panel
		_grammarMessagePanel.add(_grammarMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar lexicon message panel.
	 */
	private void buildLexiconMessagePanel() {

		// Creates the lexicon message panel
		_lexiconMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the lexicon message panel border
		_lexiconMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the lexicon message label
		_lexiconMessageLabel = new JLabel(" ");

		// Adds the lexicon message label to the lexicon message panel
		_lexiconMessagePanel.add(_lexiconMessageLabel);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar status message panel.
	 */
	private void buildStatusMessagePanel() {

		// Creates the status message panel
		_statusMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		// Sets the status message panel border
		_statusMessagePanel.setBorder(BorderFactory.createEtchedBorder());

		// Creates the status message label
		_statusMessageLabel = new JLabel(" ");

		// Adds the status message label to the status message panel
		_statusMessagePanel.add(_statusMessageLabel);
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE status bar.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.weightx = 0.5;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the status message panel to the status bar
		add(_statusMessagePanel, constraints);

		constraints.weightx = 0.0;
		constraints.gridx = 1;

		// Adds the grammar message panel to the status bar
		add(_grammarMessagePanel, constraints);

		constraints.gridx = 2;

		// Adds the lexicon message panel to the status bar
		add(_lexiconMessagePanel, constraints);

		constraints.gridx = 3;

		// Adds the line and column message message panel to the status bar
		add(_lineAndColumnMessagePanel, constraints);

		constraints.gridx = 4;

		// Adds the number of lines message panel to the status bar
		add(_numberOfLinesMessagePanel, constraints);

		constraints.gridx = 5;

		// Adds the CAPS LOCK message panel to the status bar
		add(_capsLockMessagePanel, constraints);

		constraints.gridx = 6;

		// Adds the NUM LOCK message panel to the status bar
		add(_numLockMessagePanel, constraints);

		constraints.gridx = 7;

		// Adds the SCROLL LOCK message panel to the status bar
		add(_scrollLockMessagePanel, constraints);

		constraints.gridx = 8;

		// Adds the edition mode message panel to the status bar
		add(_editionModeMessagePanel, constraints);

		// Adds the timer message panel to the status bar
		add(_timerMessagePanel);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE status bar listeners.
	 */
	private void setListeners() {

		// Sets the ACIDE - A Configurable IDE status bar popup menu listener
		_statusMessageLabel
				.addMouseListener(new AcideStatusBarPopupMenuListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE status bar popup menu.
	 */
	public void buildPopupMenu() {
		_popup = new AcideStatusBarPopupMenu();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar CAPS LOCK message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar CAPS LOCK message.
	 */
	public String getCapsLockMessage() {
		return _capsLockMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status barCAPS LOCK
	 * text field text.
	 * 
	 * @param capsLock
	 *            new value to set.
	 */
	public void setCapsLockMessage(String capsLock) {
		_capsLockMessageLabel.setText(capsLock);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar status message content.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar status message content.
	 */
	public String getStatusMessageContent() {
		return _statusMessageLabel.getText();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar status message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar status message.
	 */
	public JLabel getStatusMessage() {
		return _statusMessageLabel;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar status
	 * message.
	 * 
	 * @param message
	 *            new value to set.
	 */
	public void setStatusMessage(String message) {
		_statusMessageLabel.setText(message);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar NUM LOCK message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar NUM LOCK message.
	 */
	public String getNumLockMessage() {
		return _numLockMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar NUM LOCK
	 * message.
	 * 
	 * @param numLockMessage
	 *            new value to set.
	 */
	public void setNumLockMessage(String numLockMessage) {
		_numLockMessageLabel.setText(numLockMessage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar SCROLL LOCK message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar SCROLL LOCK message.
	 */
	public String getScrollLockMessage() {
		return _scrollLockMessageLabel.getText();
	}

	/**
	 * Sets a new value to ACIDE - A Configurable IDE status bar SCROLL LOCK
	 * message.
	 * 
	 * @param scrollLockMessage
	 *            new value to set.
	 */
	public void setScrollLockMessage(String scrollLockMessage) {
		_scrollLockMessageLabel.setText(scrollLockMessage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar timer message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar timer message.
	 */
	public String getTimerMessage() {
		return _timerMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar timer
	 * message.
	 * 
	 * @param timer
	 *            new value to set.
	 */
	public void setTimerMessage(String timer) {
		_timerMessageLabel.setText(timer);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar line column message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar line column message.
	 */
	public String getLineAndColumnMessage() {
		return _lineAndColumnMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar line and
	 * column message.
	 * 
	 * @param lineAndColumnMessage
	 *            new value to set.
	 */
	public void setLineAndColumnMessage(String lineAndColumnMessage) {
		_lineAndColumnMessageLabel.setText(lineAndColumnMessage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar lexicon message.
	 * 
	 * @return The ACIDE - A Configurable IDE status bar lexicon message.
	 */
	public String getLexiconMessage() {
		return _lexiconMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar lexicon
	 * message.
	 * 
	 * @param lexiconMessage
	 *            new value to set.
	 */
	public void setLexiconMessage(String lexiconMessage) {
		_lexiconMessageLabel.setText(lexiconMessage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar grammar message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar grammar message.
	 */
	public String getGrammarMessage() {
		return _grammarMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar grammar
	 * message.
	 * 
	 * @param grammarMessage
	 *            new value to set.
	 */
	public void setGrammarMessage(String grammarMessage) {
		_grammarMessageLabel.setText(grammarMessage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar popup menu.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar popup menu.
	 * @see AcideStatusBarPopupMenu
	 */
	public AcideStatusBarPopupMenu getPopupMenu() {
		return _popup;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar number of lines
	 * message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar number of lines
	 *         message.
	 */
	public String getNumberOfLinesMessage() {
		return _numberOfLinesMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar number of
	 * lines message.
	 * 
	 * @param numberOfLinesMessage
	 *            new value to set.
	 */
	public void setNumberOfLinesMessage(String numberOfLinesMessage) {
		_numberOfLinesMessageLabel.setText(numberOfLinesMessage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE status bar edition mode message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar edition mode message.
	 */
	public String getEditionModeMessage() {
		return _editionModeMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar insert
	 * message.
	 * 
	 * @param editionModeMessage
	 *            new value to set.
	 */
	public void setEditionModeMessage(String editionModeMessage) {
		_editionModeMessageLabel.setText(editionModeMessage);
	}

	/**
	 * Updates the ACIDE - A Configurable IDE status bar with the message of the
	 * type of file that is currently selected in the file editor.
	 */
	public void updateStatusMessageFromFileEditor() {

		// If there is a selected file editor panel
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel() != null) {

			// If it is COMPILABLE file
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().isCompilableFile())

				// If it is MAIN file
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())

					// Updates the status message in the status bar with
					// <MAIN>
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath()
											+ " <MAIN>");
				else

					// Updates the status message in the status bar with
					// <COMPILABLE>
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath()
											+ " <COMPILABLE>");
			else

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath());
		} else {

			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setStatusMessage(" ");
		}
	}
}
