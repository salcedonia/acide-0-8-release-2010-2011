package gui.statusBar;

import gui.statusBar.listeners.StatusBarPopupMenuListener;
import gui.statusBar.popup.StatusBarPopupMenu;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.swing.Timer;
import javax.swing.Box;
import javax.swing.JTextField;

import operations.log.Log;

import utils.OSValidator;

/************************************************************************																
 * Status bar of ACIDE - A Configurable IDE										
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan Jos� Ortiz S�nchez										
 *         </ul>														
 *         <ul>															
 *         Delf�n Rup�rez Ca�as											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Mart�n L�zaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo G�mez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8																														
 ***********************************************************************/
public class StatusBar {

	/**
	 * Status bar box
	 */
	private Box _statusBar;
	/**
	 * Message to display
	 */
	private JTextField _message;
	/**
	 * Message for the LineCol
	 */
	private JTextField _messageLineCol;
	/**
	 * Message for the lexicon
	 */
	private JTextField _lexiconMessage;
	/**
	 * Message for the grammar
	 */
	private JTextField _grammarMessage;
	/**
	 * Message for the CAPS LOCK
	 */
	private JTextField _capsLock;
	/**
	 * Message for the NUM LOCK
	 */
	private JTextField _numLock;
	/**
	 * Message for the SCROLL LOCK
	 */
	private JTextField _scrollLock;
	/**
	 * Message for the time
	 */
	private JTextField _time;
	/**
	 * Popup menu of the status bar
	 */
	private StatusBarPopupMenu _popup;

	/**
	 * Class constructor
	 */
	@SuppressWarnings("deprecation")
	public StatusBar() {

		// STATUS BAR
		_statusBar = Box.createHorizontalBox();

		// LINE COLUMN MESSAGE
		_messageLineCol = new JTextField(8);
		_messageLineCol.setBackground(Color.LIGHT_GRAY);
		_messageLineCol.setEditable(false);
		_messageLineCol.setText("");

		// LEXICAL MESSAGE
		_lexiconMessage = new JTextField(30);
		_lexiconMessage.setBackground(Color.LIGHT_GRAY);
		_lexiconMessage.setEditable(false);
		_lexiconMessage.setText("");

		// SYNTACTIC MESSAGE
		_grammarMessage = new JTextField(25);
		_grammarMessage.setBackground(Color.LIGHT_GRAY);
		_grammarMessage.setEditable(false);
		_grammarMessage.setText("");

		// MESSAGE
		_message = new JTextField(57);
		_message.setBackground(Color.LIGHT_GRAY);
		_message.setEditable(false);
		_message.setText("");
		_message.setText(_message.getText());

		// POPUP
		buildPopupMenu();

		// Listeners
		_message.addMouseListener(new StatusBarPopupMenuListener());

		boolean state;

		// CAPS LOCK
		_capsLock = new JTextField(7);
		_capsLock.setCaretColor(Color.blue);

		// CAPS LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {

			state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_CAPS_LOCK);

			if (state)
				_capsLock.setText("CAPS");
			else
				_capsLock.setText("   ");
		} else
			_capsLock.setText("   ");

		_capsLock.setBackground(Color.LIGHT_GRAY);
		_capsLock.setEditable(false);
		_capsLock.setToolTipText("CAPS");
		_capsLock.setForeground(Color.BLACK);

		// NUM LOCK
		_numLock = new JTextField(5);

		// NUM LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {

			state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_NUM_LOCK);
			if (state)
				_numLock.setText("NUM");
			else
				_numLock.setText("   ");
		} else
			_numLock.setText("   ");

		_numLock.setBackground(Color.LIGHT_GRAY);
		_numLock.setEditable(false);
		_numLock.setToolTipText("NUM");
		_numLock.setForeground(Color.BLACK);

		// SCROLL LOCK
		_scrollLock = new JTextField(9);

		// SCROLL LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {
			state = Toolkit.getDefaultToolkit().getLockingKeyState(
					KeyEvent.VK_SCROLL_LOCK);
			if (state)
				_scrollLock.setText("SCROLL");
			else
				_scrollLock.setText("      ");
		} else
			_scrollLock.setText("      ");

		_scrollLock.setBackground(Color.LIGHT_GRAY);
		_scrollLock.setEditable(false);
		_scrollLock.setToolTipText("SCROLL");
		_scrollLock.setForeground(Color.BLACK);

		// TIME
		_time = new JTextField(10);
		_time.setForeground(Color.BLACK);
		_time.setText("       ");
		_time.setBackground(Color.LIGHT_GRAY);
		try {
			
			// Set the hand cursor
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
			_numLock.setCursor(cursor);
			_time.setCursor(cursor);
			_capsLock.setCursor(cursor);
			_scrollLock.setCursor(cursor);
			
			// Set the text cursor
			cursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);
			_message.setCursor(cursor);
		} catch (HeadlessException exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		_time.setToolTipText(new Date().toLocaleString());

		// ADD THE ELEMENTS
		_statusBar.add(_message);
		_statusBar.add(_grammarMessage);
		_statusBar.add(_lexiconMessage);
		_statusBar.add(_messageLineCol);
		_statusBar.add(Box.createGlue());
		_statusBar.add(_capsLock);
		_statusBar.add(_numLock);
		_statusBar.add(_scrollLock);

		// DATE FORMAT
		final DateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");
		new Timer(1000, new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_time.setText(dateFormat.format(new Date()));
			}
		}).start();

		_statusBar.add(_time);
		_statusBar.setVisible(true);

	}

	/**
	 * Builds the status bar popup menu
	 */
	public void buildPopupMenu() {
		_popup = new StatusBarPopupMenu();
	}

	/**
	 * Returns the CAPS LOCK text field
	 * 
	 * @return the CAPS LOCK text field
	 */
	public JTextField getCapsLock() {
		return _capsLock;
	}

	/**
	 * Sets a new value to CAPS LOCK text field
	 * 
	 * @param capsLock
	 *            new value to set
	 */
	public void setCapsLock(String capsLock) {
		_capsLock.setText(capsLock);
	}

	/**
	 * Returns the message
	 * 
	 * @return the message
	 */
	public String getMessage() {
		return _message.getText();
	}

	/**
	 * Returns the message text field
	 * 
	 * @return the message text field
	 */
	public JTextField getMessageTextField() {
		return _message;
	}

	/**
	 * Sets a new value to the message
	 * 
	 * @param message
	 *            new value to set
	 */
	public void setMessage(String message) {
		_message.setText(message);
	}

	/**
	 * Returns the NUM LOCK text field
	 * 
	 * @return the NUM LOCK text field
	 */
	public JTextField getNumLock() {
		return _numLock;
	}

	/**
	 * Sets a new value to the NUM LOCK
	 * 
	 * @param numLock
	 *            new value to set
	 */
	public void setNumLock(String numLock) {
		_numLock.setText(numLock);
	}

	/**
	 * Returns the status bar box
	 * 
	 * @return the status bar box
	 */
	public Box getStatusBar() {
		return _statusBar;
	}

	/**
	 * Sets a new value to the status bar box
	 * 
	 * @param bar
	 *            new value to set
	 */
	public void setStatusBar(Box bar) {
		_statusBar = bar;
	}

	/**
	 * Returns the SCROLL LOCK text field
	 * 
	 * @return the SCROLL LOCK text field
	 */
	public JTextField getScrollLock() {
		return _scrollLock;
	}

	/**
	 * Sets a new value to SCROLL LOCK
	 * 
	 * @param scrollLock
	 *            new value to set
	 */
	public void setScrollLock(String scrollLock) {
		_scrollLock.setText(scrollLock);
	}

	/**
	 * Returns the time text field
	 * 
	 * @return the time text field
	 */
	public JTextField getTime() {
		return _time;
	}

	/**
	 * Sets a new value to the time
	 * 
	 * @param time
	 *            new value to set
	 */
	public void setTime(String time) {
		_time.setText(time);
	}

	/**
	 * Returns the line column message
	 * 
	 * @return the line column message
	 */
	public JTextField getMessageLineCol() {
		return _messageLineCol;
	}

	/**
	 * Sets a new value to the line column message
	 * 
	 * @param messageLineCol
	 *            new value to set
	 */
	public void setMessageLineCol(JTextField messageLineCol) {
		_messageLineCol = messageLineCol;
	}

	/**
	 * Returns the lexicon message text field
	 * 
	 * @return The lexicon message text field
	 */
	public JTextField getLexiconMessage() {
		return _lexiconMessage;
	}

	/**
	 * Sets a new value to the lexical message
	 * 
	 * @param lexiconMessage
	 *            new value to set
	 */
	public void setLexiconMessage(String lexiconMessage) {
		_lexiconMessage.setText(lexiconMessage);
	}

	/**
	 * Returns the grammar message
	 * 
	 * @return The grammar message
	 */
	public JTextField getGrammarMessage() {
		return _grammarMessage;
	}

	/**
	 * Sets a new value to the grammar message
	 * 
	 * @param grammarMessage
	 *            new value to set
	 */
	public void setGrammarMessage(String grammarMessage) {
		_grammarMessage.setText(grammarMessage);
	}

	/**
	 * Returns the status bar popup menu
	 * 
	 * @return the status bar popup menu
	 * @see StatusBarPopupMenu
	 */
	public StatusBarPopupMenu getPopupMenu() {
		return _popup;
	}
}