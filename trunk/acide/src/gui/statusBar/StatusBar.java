package gui.statusBar;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.Timer;
import javax.swing.Box;
import javax.swing.JTextField;

import utils.OSValidator;

import language.Language;

/**
 * StatusBar of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class StatusBar {

	/**
	 * Status bar box.
	 */
	private Box _statusBar;
	/**
	 * Message to display.
	 */
	private JTextField _message;
	/**
	 * Message for the LineCol.
	 */
	private JTextField _messageLineCol;
	/**
	 * Message for the lexical.
	 */
	private JTextField _messagelexical;
	/**
	 * Message for the grammar.
	 */
	private JTextField _messageGrammar;
	/**
	 * Message for the CAPS LOCK.
	 */
	private JTextField _capsLock;
	/**
	 * Message for the NUM LOCK.
	 */
	private JTextField _numLock;
	/**
	 * Message for the SCROLL LOCK.
	 */
	private JTextField _scrollLock;
	/**
	 * Message for the time.
	 */
	private JTextField _time;
	/**
	 * PopupMenu of the status bar.
	 */
	private JPopupMenu _popup;
	/**
	 * Copy option for the popupMenu.
	 */
	private JMenuItem copy;

	/**
	 * Constructor of the class.
	 */
	@SuppressWarnings("deprecation")
	public StatusBar() {

		ResourceBundle labels = Language.getInstance().getLabels();

		// STATUS BAR
		_statusBar = Box.createHorizontalBox();

		// LINE COLUMN MESSAGE
		_messageLineCol = new JTextField(8);
		_messageLineCol.setBackground(Color.LIGHT_GRAY);
		_messageLineCol.setEditable(false);
		_messageLineCol.setText("");

		// LEXICAL MESSAGE
		_messagelexical = new JTextField(30);
		_messagelexical.setBackground(Color.LIGHT_GRAY);
		_messagelexical.setEditable(false);
		_messagelexical.setText("");

		// SYNTACTIC MESSAGE
		_messageGrammar = new JTextField(25);
		_messageGrammar.setBackground(Color.LIGHT_GRAY);
		_messageGrammar.setEditable(false);
		_messageGrammar.setText("");

		// MESSAGE
		_message = new JTextField(57);
		_message.setBackground(Color.LIGHT_GRAY);
		_message.setEditable(false);
		_message.setText("");
		_message.setText(_message.getText());

		// POPUP
		_popup = new JPopupMenu();
		copy = new JMenuItem(labels.getString("s187"));
		copy.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				_message.copy();
			}
		});
		_popup.add(copy);

		// LISTENERS
		_message.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent
			 * )
			 */
			public void mousePressed(MouseEvent arg0) {
				if (arg0.isPopupTrigger()) {
					_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
				}
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent
			 * )
			 */
			public void mouseReleased(MouseEvent arg0) {
				if (arg0.isPopupTrigger()) {
					_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
				}
			}
		});

		boolean state;
		
		// CAPS LOCK
		_capsLock = new JTextField(7);
		_capsLock.setCaretColor(Color.blue);

		// CAPS LOCK ONLY VALID IN WINDOWS
		if (OSValidator.isWindows()) {

			state = Toolkit.getDefaultToolkit().getLockingKeyState(KeyEvent.VK_CAPS_LOCK);
			
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

			state = Toolkit.getDefaultToolkit().getLockingKeyState(KeyEvent.VK_NUM_LOCK);
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
			state = Toolkit.getDefaultToolkit().getLockingKeyState(KeyEvent.VK_SCROLL_LOCK);
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
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
			_numLock.setCursor(cursor);
			_time.setCursor(cursor);
			_capsLock.setCursor(cursor);
			_scrollLock.setCursor(cursor);
			cursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);
			_message.setCursor(cursor);
		} catch (HeadlessException e1) {
			e1.printStackTrace();
		}
		_time.setToolTipText(new Date().toLocaleString());

		// ADD THE ELEMENTS
		_statusBar.add(_message);
		_statusBar.add(_messageGrammar);
		_statusBar.add(_messagelexical);
		_statusBar.add(_messageLineCol);
		_statusBar.add(Box.createGlue());
		_statusBar.add(_capsLock);
		_statusBar.add(_numLock);
		_statusBar.add(_scrollLock);

		// DATE FORMAT
		final DateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");
		new Timer(1000, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				_time.setText(dateFormat.format(new Date()));
			}
		}).start();

		_statusBar.add(_time);
		_statusBar.setVisible(true);

	}

	/**
	 * Returns the CAPS LOCK.
	 * 
	 * @return The CAPS LOCK.
	 */
	public JTextField getCapsLock() {
		return _capsLock;
	}

	/**
	 * Set a new value to CAPS LOCK
	 * 
	 * @param capsLock
	 *            New value to set.
	 */
	public void setCapsLock(String capsLock) {
		_capsLock.setText(capsLock);
	}

	/**
	 * Return the message.
	 * 
	 * @return The message.
	 */
	public String getMessage() {
		return _message.getText();
	}

	/**
	 * Set a new value to the message.
	 * 
	 * @param message
	 *            New value to set.
	 */
	public void setMessage(String message) {
		_message.setText(message);
	}

	/**
	 * Returns the NUM LOCK.
	 * 
	 * @return The NUM LOCK.
	 */
	public JTextField getNumLock() {
		return _numLock;
	}

	/**
	 * Set a new value to the NUM LOCK.
	 * 
	 * @param numLock
	 *            New value to set.
	 */
	public void setNumLock(String numLock) {
		_numLock.setText(numLock);
	}

	/**
	 * Returns the status bar box.
	 * 
	 * @return The status bar box.
	 */
	public Box getStatusBar() {
		return _statusBar;
	}

	/**
	 * Set a new value to the status bar box.
	 * 
	 * @param bar
	 *            New value to set.
	 */
	public void setStatusBar(Box bar) {
		_statusBar = bar;
	}

	/**
	 * Returns the SCROLL LOCK.
	 * 
	 * @return The SCROLL LOCK.
	 */
	public JTextField getScrollLock() {
		return _scrollLock;
	}

	/**
	 * Set a new value to SCROLL LOCK.
	 * 
	 * @param scrollLock
	 *            New value to set.
	 */
	public void setScrollLock(String scrollLock) {
		_scrollLock.setText(scrollLock);
	}

	/**
	 * Returns the time.
	 * 
	 * @return The time.
	 */
	public JTextField getTime() {
		return _time;
	}

	/**
	 * Set a new value to the time.
	 * 
	 * @param time
	 *            New value to set.
	 */
	public void setTime(String time) {
		_time.setText(time);
	}

	/**
	 * Returns the Line Column message.
	 * 
	 * @return The Line Column message.
	 */
	public JTextField getMessageLineCol() {
		return _messageLineCol;
	}

	/**
	 * Set a new value to the Line Column message.
	 * 
	 * @param messageLineCol
	 *            New value to set.
	 */
	public void setMessageLineCol(JTextField messageLineCol) {
		_messageLineCol = messageLineCol;
	}

	/**
	 * Return the lexical message.
	 * 
	 * @return The lexical message.
	 */
	public JTextField getMessagelexical() {
		return _messagelexical;
	}

	/**
	 * Set a new value to the lexical message.
	 * 
	 * @param messagelexical
	 *            New value to set.
	 */
	public void setMessagelexical(String messagelexical) {
		_messagelexical.setText(messagelexical);
	}

	/**
	 * Returns the grammar message.
	 * 
	 * @return The grammar message.
	 */
	public JTextField getMessageGrammar() {
		return _messageGrammar;
	}

	/**
	 * Set a new value to the grammar message.
	 * 
	 * @param messageGrammar
	 *            New value to set.
	 */
	public void setMessageGrammar(String messageGrammar) {
		_messageGrammar.setText(messageGrammar);
	}
}
