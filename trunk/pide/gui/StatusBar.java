package gui;

import idioma.Idioma;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.Timer;
import javax.swing.Box;
import javax.swing.JTextField;
import javax.swing.border.Border;

/**
 * CLASE StatusBar: Clase que implementa StatusBar
 * 
 * 
 */
public class StatusBar {

	/**
	 * Atributo Status Bar
	 * 
	 */
	private Box sBar;

	/**
	 * 
	 */
	private JTextField message;
	
	/**
	 * 
	 */
	private JTextField messageLineCol;
	
	private JTextField messagelexical;

	/**
	 * 
	 */
	private JTextField capsLock;

	/**
	 * 
	 */
	private JTextField numLock;

	/**
	 * 
	 */
	private JTextField scrollLock;

	/*
	 * 
	 */
	private JTextField time;
	
	/**
	 * 
	 */
    private JPopupMenu jpopup;
    
    private JTextField messageGrammar;
    
	public StatusBar(){
		ResourceBundle labels = Idioma.getInstance().getLabels();
		jpopup=new JPopupMenu();
		sBar = Box.createHorizontalBox();
		messageLineCol = new JTextField(8);
		messageLineCol.setBackground(Color.LIGHT_GRAY); 
		messageLineCol.setEditable(false);
        messageLineCol.setText("");
        //messageLineCol.setText(messageLineCol.getText());
        
        messagelexical = new JTextField(30);
        messagelexical.setBackground(Color.LIGHT_GRAY); 
        messagelexical.setEditable(false);
        messagelexical.setText("");
        //messagelexical.setText(messagelexical.getText());
        
        messageGrammar = new JTextField(25);
        messageGrammar.setBackground(Color.LIGHT_GRAY); 
        messageGrammar.setEditable(false);
        messageGrammar.setText("");
        
		message = new JTextField(57);
		message.setBackground(Color.LIGHT_GRAY); 
        message.setEditable(false);
        message.setText("");
		message.setText(message.getText());
		message.addMouseListener(new MouseAdapter(){
     	public void mousePressed(MouseEvent arg0) {
				// TODO Auto-generated method stub
				if (arg0.isPopupTrigger()) {
		            jpopup.show(arg0.getComponent(),
		                       arg0.getX(), arg0.getY());
			}
			}
			public void mouseReleased(MouseEvent arg0) {
				// TODO Auto-generated method stub
				if (arg0.isPopupTrigger()) {
		            jpopup.show(arg0.getComponent(),
		                       arg0.getX(), arg0.getY());
			}

			}
		
		}		
		);
		JMenuItem copy = new JMenuItem(labels.getString("s187"));
		copy.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			 message.copy();
			}
		});
		jpopup.add(copy);	
	   	capsLock = new JTextField(7);
		capsLock.setCaretColor(Color.blue);
		Toolkit toolkit=Toolkit.getDefaultToolkit();
		boolean state = toolkit.getLockingKeyState(KeyEvent.VK_CAPS_LOCK);
		if (state) capsLock.setText("CAPS");
		else capsLock.setText("   ");
		capsLock.setBackground(Color.LIGHT_GRAY);
        capsLock.setEditable(false); 
	    capsLock.setToolTipText("CAPS");
        capsLock.setForeground(Color.BLACK);
	    numLock = new JTextField(5);
        state = toolkit.getLockingKeyState(KeyEvent.VK_NUM_LOCK);
		if  (state) numLock.setText("NUM");
		else  numLock.setText("   ");
		numLock.setBackground(Color.LIGHT_GRAY);
        numLock.setEditable(false);
        numLock.setToolTipText("NUM");
        numLock.setForeground(Color.BLACK);
        scrollLock = new JTextField(9);
        state = toolkit.getLockingKeyState(KeyEvent.VK_SCROLL_LOCK);
		if (state) scrollLock.setText("SCROLL");
	    else scrollLock.setText("      ");
		scrollLock.setBackground(Color.LIGHT_GRAY);
        scrollLock.setEditable(false);
        scrollLock.setToolTipText("SCROLL");
        scrollLock.setForeground(Color.BLACK);
        time = new JTextField(10);
        time.setForeground(Color.BLACK);
        time.setText("       ");
		time.setBackground(Color.LIGHT_GRAY);
       	try {
			Cursor d = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
			numLock.setCursor(d);
		   time.setCursor(d);
		   capsLock.setCursor(d);
		   scrollLock.setCursor(d);
		d = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);
		  message.setCursor(d);
	} catch (HeadlessException e1) {
		// TODO Auto-generated catch block
		e1.printStackTrace();
	} 
		time.setToolTipText(new Date().toLocaleString());
        sBar.add(message);
        sBar.add(messageGrammar);
        sBar.add(messagelexical);
        sBar.add(messageLineCol);
		sBar.add(Box.createGlue());
		sBar.add(capsLock);
		sBar.add(numLock);
		sBar.add(scrollLock);
		final DateFormat df = new SimpleDateFormat("HH:mm:ss");
        new Timer(1000,new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                time.setText(df.format(new Date()));
            }
        }).start();
 
		sBar.add(time);
		sBar.setVisible(true);
        
	  }
	public JTextField getCapsLock() {
		return capsLock;
	}

	public void setCapsLock(String capsLock) {
		this.capsLock.setText(capsLock);
	}

	public String getMessage() {
		return message.getText();
	}

	public void setMessage(String message) {
		this.message.setText(message);
	}

	public JTextField getNumLock() {
		return numLock;
	}

	public void setNumLock(String numLock) {
		this.numLock.setText(numLock);
	}

	public Box getSBar() {
		return sBar;
	}

	public void setSBar(Box bar) {
		sBar = bar;
	}

	public JTextField getScrollLock() {
		return scrollLock;
	}

	public void setScrollLock(String scrollLock) {
		this.scrollLock.setText(scrollLock);
	}
	public JTextField getTime() {
		return time;
	}
	public void setTime(String time) {
		this.time.setText(time);
	}
	public JTextField getMessageLineCol() {
		return messageLineCol;
	}
	public void setMessageLineCol(JTextField messageLineCol) {
		this.messageLineCol = messageLineCol;
	}
	public JTextField getMessageLexical() {
		return messagelexical;
	}
	public void setMessageLexical(String messagelexical) {
		this.messagelexical.setText(messagelexical);
	}
	public JTextField getMessageGrammar() {
		return messageGrammar;
	}
	public void setMessageGrammar(String messageGrammar) {
		this.messageGrammar.setText(messageGrammar);
	}	
	
}
