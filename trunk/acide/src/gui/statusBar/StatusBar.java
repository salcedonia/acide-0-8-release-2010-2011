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

import language.Language;

/**
 * 
 */
public class StatusBar {

	/**
	 * 
	 */
	private Box _statusBar;
	/**
	 * 
	 */
	private JTextField _message;
	/**
	 * 
	 */
	private JTextField _messageLineCol;
	/**
	 * 
	 */
	private JTextField _messagelexical;
	/**
	 * 
	 */
	private JTextField _capsLock;
	/**
	 * 
	 */
	private JTextField _numLock;
	/**
	 * 
	 */
	private JTextField _scrollLock;
	/*
	 * 
	 */
	private JTextField _time;
	/**
	 * 
	 */
	private JPopupMenu _popup;
	/**
     * 
     */
	private JTextField _messageGrammar;

	/**
	 * Constructor of the class.
	 */
	@SuppressWarnings("deprecation")
	public StatusBar() {
		
		ResourceBundle labels = Language.getInstance().getLabels();
		
		_popup = new JPopupMenu();
		_statusBar = Box.createHorizontalBox();
		_messageLineCol = new JTextField(8);
		_messageLineCol.setBackground(Color.LIGHT_GRAY);
		_messageLineCol.setEditable(false);
		_messageLineCol.setText("");
		// messageLineCol.setText(messageLineCol.getText());

		_messagelexical = new JTextField(30);
		_messagelexical.setBackground(Color.LIGHT_GRAY);
		_messagelexical.setEditable(false);
		_messagelexical.setText("");
		// messagelexical.setText(messagelexical.getText());

		_messageGrammar = new JTextField(25);
		_messageGrammar.setBackground(Color.LIGHT_GRAY);
		_messageGrammar.setEditable(false);
		_messageGrammar.setText("");

		_message = new JTextField(57);
		_message.setBackground(Color.LIGHT_GRAY);
		_message.setEditable(false);
		_message.setText("");
		_message.setText(_message.getText());
		_message.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
			 */
			public void mousePressed(MouseEvent arg0) {
				if (arg0.isPopupTrigger()) {
					_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
				}
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
			 */
			public void mouseReleased(MouseEvent arg0) {
				if (arg0.isPopupTrigger()) {
					_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
				}

			}
		});
		JMenuItem copy = new JMenuItem(labels.getString("s187"));
		copy.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				_message.copy();
			}
		});
		_popup.add(copy);
		_capsLock = new JTextField(7);
		_capsLock.setCaretColor(Color.blue);
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		boolean state = toolkit.getLockingKeyState(KeyEvent.VK_CAPS_LOCK);
		if (state)
			_capsLock.setText("CAPS");
		else
			_capsLock.setText("   ");
		_capsLock.setBackground(Color.LIGHT_GRAY);
		_capsLock.setEditable(false);
		_capsLock.setToolTipText("CAPS");
		_capsLock.setForeground(Color.BLACK);
		_numLock = new JTextField(5);
		state = toolkit.getLockingKeyState(KeyEvent.VK_NUM_LOCK);
		if (state)
			_numLock.setText("NUM");
		else
			_numLock.setText("   ");
		_numLock.setBackground(Color.LIGHT_GRAY);
		_numLock.setEditable(false);
		_numLock.setToolTipText("NUM");
		_numLock.setForeground(Color.BLACK);
		_scrollLock = new JTextField(9);
		state = toolkit.getLockingKeyState(KeyEvent.VK_SCROLL_LOCK);
		if (state)
			_scrollLock.setText("SCROLL");
		else
			_scrollLock.setText("      ");
		_scrollLock.setBackground(Color.LIGHT_GRAY);
		_scrollLock.setEditable(false);
		_scrollLock.setToolTipText("SCROLL");
		_scrollLock.setForeground(Color.BLACK);
		_time = new JTextField(10);
		_time.setForeground(Color.BLACK);
		_time.setText("       ");
		_time.setBackground(Color.LIGHT_GRAY);
		try {
			Cursor d = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
			_numLock.setCursor(d);
			_time.setCursor(d);
			_capsLock.setCursor(d);
			_scrollLock.setCursor(d);
			d = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);
			_message.setCursor(d);
		} catch (HeadlessException e1) {
			e1.printStackTrace();
		}
		_time.setToolTipText(new Date().toLocaleString());
		_statusBar.add(_message);
		_statusBar.add(_messageGrammar);
		_statusBar.add(_messagelexical);
		_statusBar.add(_messageLineCol);
		_statusBar.add(Box.createGlue());
		_statusBar.add(_capsLock);
		_statusBar.add(_numLock);
		_statusBar.add(_scrollLock);
		final DateFormat df = new SimpleDateFormat("HH:mm:ss");
		new Timer(1000, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				_time.setText(df.format(new Date()));
			}
		}).start();

		_statusBar.add(_time);
		_statusBar.setVisible(true);

	}

	/**
	 * 
	 * @return
	 */
	public JTextField getCapsLock() {
		return _capsLock;
	}

	/**
	 * 
	 * @param capsLock
	 */
	public void setCapsLock(String capsLock) {
		_capsLock.setText(capsLock);
	}

	/**
	 * 
	 * @return
	 */
	public String getMessage() {
		return _message.getText();
	}

	/**
	 * 
	 * @param message
	 */
	public void setMessage(String message) {
		_message.setText(message);
	}

	/**
	 * 
	 * @return
	 */
	public JTextField getNumLock() {
		return _numLock;
	}

	/**
	 * 
	 */
	public void setNumLock(String numLock) {
		_numLock.setText(numLock);
	}

	/**
	 * 
	 * @return
	 */
	public Box getStatusBar() {
		return _statusBar;
	}

	/**
	 * 
	 * @param bar
	 */
	public void setStatusBar(Box bar) {
		_statusBar = bar;
	}

	/**
	 * 
	 * @return
	 */
	public JTextField getScrollLock() {
		return _scrollLock;
	}

	/**
	 * 
	 * @param scrollLock
	 */
	public void setScrollLock(String scrollLock) {
		_scrollLock.setText(scrollLock);
	}

	/**
	 * 
	 * @return
	 */
	public JTextField getTime() {
		return _time;
	}

	/**
	 * 
	 * @param time
	 */
	public void setTime(String time) {
		_time.setText(time);
	}

	/**
	 * 
	 * @return
	 */
	public JTextField getMessageLineCol() {
		return _messageLineCol;
	}

	/**
	 * 
	 * @param messageLineCol
	 */
	public void setMessageLineCol(JTextField messageLineCol) {
		_messageLineCol = messageLineCol;
	}

	/**
	 * 
	 * @return
	 */
	public JTextField getMessagelexical() {
		return _messagelexical;
	}

	/**
	 * 
	 * @param messagelexical
	 */
	public void setMessagelexical(String messagelexical) {
		_messagelexical.setText(messagelexical);
	}

	/**
	 * 
	 * @return
	 */
	public JTextField getMessageGrammar() {
		return _messageGrammar;
	}

	/**
	 * 
	 * @param messageGrammar
	 */
	public void setMessageGrammar(String messageGrammar) {
		_messageGrammar.setText(messageGrammar);
	}
}
