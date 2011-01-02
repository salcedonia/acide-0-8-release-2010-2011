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
package gui.statusBarPanel;

import es.configuration.project.AcideProjectConfiguration;
import gui.mainWindow.MainWindow;
import gui.statusBarPanel.listeners.AcideStatusBarPopupMenuListener;
import gui.statusBarPanel.popup.AcideStatusBarPopupMenu;

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

import operations.log.AcideLog;
import utils.OSValidator;

/**																
 * ACIDE - A Configurable IDE status bar.
 *					
 * @version 0.8																													
 */
public class AcideStatusBar extends JPanel{

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
	 * ACIDE - A Configurable IDE status bar popup menu of the status bar.
	 */
	private AcideStatusBarPopupMenu _popup;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE status bar.
	 */
	@SuppressWarnings("deprecation")
	public AcideStatusBar() {

		super(new GridBagLayout());
		
		// STATUS MESSAGE
		_statusMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_statusMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_statusMessageLabel = new JLabel(" ");
		_statusMessageLabel.setText(_statusMessageLabel.getText());
		_statusMessagePanel.add(_statusMessageLabel);

		// LEXICON MESSAGE
		_lexiconMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_lexiconMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_lexiconMessageLabel = new JLabel(" ");
		_lexiconMessagePanel.add(_lexiconMessageLabel);

		// GRAMMAR MESSAGE
		_grammarMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_grammarMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_grammarMessageLabel = new JLabel(" ");
		_grammarMessagePanel.add(_grammarMessageLabel);
		
		// LINE COLUMN MESSAGE
		_lineAndColumnMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_lineAndColumnMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_lineAndColumnMessageLabel = new JLabel(" ");
		_lineAndColumnMessagePanel.add(_lineAndColumnMessageLabel);

		// NUMBER OF LINES MESSAGE
		_numberOfLinesMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_numberOfLinesMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_numberOfLinesMessageLabel = new JLabel(" ");
		_numberOfLinesMessagePanel.add(_numberOfLinesMessageLabel);
		
		// POPUP
		buildPopupMenu();

		// Listeners
		_statusMessageLabel.addMouseListener(new AcideStatusBarPopupMenuListener());

		boolean state;

		// CAPS LOCK
		_capsLockMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_capsLockMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_capsLockMessageLabel = new JLabel();
		_capsLockMessageLabel.setForeground(Color.BLUE);

		// CAPS LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {

			state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_CAPS_LOCK);

			if (state)
				_capsLockMessageLabel.setText("CAPS");
			else
				_capsLockMessageLabel.setText("    ");
		} else
			_capsLockMessageLabel.setText("    ");

		_capsLockMessageLabel.setToolTipText("CAPS");
		_capsLockMessageLabel.setForeground(Color.BLACK);
		_capsLockMessagePanel.add(_capsLockMessageLabel);

		// NUM LOCK
		_numLockMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_numLockMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_numLockMessageLabel = new JLabel();
		
		// NUM LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {

			state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_NUM_LOCK);
			if (state)
				_numLockMessageLabel.setText("NUM");
			else
				_numLockMessageLabel.setText("   ");
		} else
			_numLockMessageLabel.setText("   ");

		_numLockMessageLabel.setToolTipText("NUM");
		_numLockMessageLabel.setForeground(Color.BLACK);
		_numLockMessagePanel.add(_numLockMessageLabel);
		
		// SCROLL LOCK
		_scrollLockMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_scrollLockMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_scrollLockMessageLabel = new JLabel();

		// SCROLL LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {
			state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_SCROLL_LOCK);
			if (state)
				_scrollLockMessageLabel.setText("SCROLL");
			else
				_scrollLockMessageLabel.setText("     ");
		} else
			_scrollLockMessageLabel.setText("     ");

		_scrollLockMessageLabel.setToolTipText("SCROLL");
		_scrollLockMessageLabel.setForeground(Color.BLACK);
		_scrollLockMessagePanel.add(_scrollLockMessageLabel);

		// TIMER
		_timerMessagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		_timerMessagePanel.setBorder(BorderFactory.createEtchedBorder());
		_timerMessageLabel = new JLabel();
		_timerMessageLabel.setForeground(Color.BLACK);
		_timerMessageLabel.setText(" ");
		_timerMessagePanel.add(_timerMessageLabel);

		try {
			
			// Set the hand cursor
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
			_numLockMessageLabel.setCursor(cursor);
			_timerMessageLabel.setCursor(cursor);
			_capsLockMessageLabel.setCursor(cursor);
			_scrollLockMessageLabel.setCursor(cursor);
			
			// Set the text cursor
			cursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);
			_statusMessageLabel.setCursor(cursor);
		} catch (HeadlessException exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		_timerMessageLabel.setToolTipText(new Date().toLocaleString());

		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.weightx = 0.5;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_statusMessagePanel, constraints);
		constraints.weightx = 0.0;
		constraints.gridx = 1;
		add(_grammarMessagePanel, constraints);
		constraints.gridx = 2;
		add(_lexiconMessagePanel, constraints);
		constraints.gridx = 3;
		add(_lineAndColumnMessagePanel, constraints);
		constraints.gridx = 4;
		add(_numberOfLinesMessagePanel, constraints);
		constraints.gridx = 5;
		add(_capsLockMessagePanel, constraints);	
		constraints.gridx = 6;
		add(_numLockMessagePanel, constraints);
		constraints.gridx = 7;
		add(_scrollLockMessagePanel, constraints);
		
		// DATE FORMAT
		final DateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");
		new Timer(1000, new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_timerMessageLabel.setText(dateFormat.format(new Date()));
			}
		}).start();

		add(_timerMessagePanel);
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
	 * Sets a new value to the ACIDE - A Configurable IDE status barCAPS LOCK text field text.
	 * 
	 * @param capsLock
	 *            new value to set.
	 */
	public void setCapsLock(String capsLock) {
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
	 * Sets a new value to the ACIDE - A Configurable IDE status bar status message.
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
	 * Sets a new value to the ACIDE - A Configurable IDE status bar NUM LOCK message.
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
	 * Sets a new value to ACIDE - A Configurable IDE status bar SCROLL LOCK message.
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
	 * Sets a new value to the ACIDE - A Configurable IDE status bar timer message.
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
	 * Sets a new value to the ACIDE - A Configurable IDE status bar line and column message.
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
	 * Sets a new value to the ACIDE - A Configurable IDE status bar lexicon message.
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
	 * Sets a new value to the ACIDE - A Configurable IDE status bar grammar message.
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
	 * Returns the ACIDE - A Configurable IDE status bar number of lines message.
	 * 
	 * @return the ACIDE - A Configurable IDE status bar number of lines message.
	 */
	public String getNumberOfLinesMessage() {
		return _numberOfLinesMessageLabel.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE status bar number of lines message.
	 * 
	 * @param numberOfLinesMessage new value to set.
	 */
	public void setNumberOfLinesMessage(String numberOfLinesMessage) {
		_numberOfLinesMessageLabel.setText(numberOfLinesMessage);
	}
	
	/**
	 * Updates the ACIDE - A Configurable IDE status bar with the message of the type of file that is 
	 * currently selected in the file editor.
	 */
	public void updatesStatusBarFromFileEditor() {
		
		if (MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel() != null) {

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath());

			for (int i = 0; i < AcideProjectConfiguration.getInstance().getNumFilesFromList(); i++) {

				if (AcideProjectConfiguration.getInstance()
						.getFileAt(i)
						.getAbsolutePath()
						.equals(MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath()))

					if (AcideProjectConfiguration.getInstance()
							.getFileAt(i).isCompilableFile())

						if (AcideProjectConfiguration.getInstance()
								.getFileAt(i).isMainFile())

							// MAIN FILE
							MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											MainWindow.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getAbsolutePath()
													+ " <MAIN>");
						else

							// COMPILABLE FILE
							MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											MainWindow.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getAbsolutePath()
													+ " <COMPILABLE>");
					else

						// Updates the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath());
			}

			// Default configuration
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// Checks the type
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilableFile())

					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isMainFile())

						// MAIN FILE
						MainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath()
												+ " <MAIN>");
					else

						// COMPILABLE FILE
						MainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath()
												+ " <COMPILABLE>");
				else
					// Updates the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());
			}
		}
	}
}
