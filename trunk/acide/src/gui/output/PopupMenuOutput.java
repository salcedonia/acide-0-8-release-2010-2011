package gui.output;

import gui.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import language.Language;
import properties.PropertiesManager;

/**
 * Output popup menu.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class PopupMenuOutput extends JPopupMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * Cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * Paste menu item.
	 */
	private JMenuItem _paste;

	/**
	 * Constructor of the class.
	 */
	public PopupMenuOutput() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();

		// COPY
		_copy = new JMenuItem(labels.getString("s187"));
		_copy.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow.getInstance().getOutput().getTextComponent().copy();
			}
		});
		add(_copy);

		// CUT
		_cut = new JMenuItem(labels.getString("s188"));
		_cut.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				
				if (MainWindow.getInstance().getOutput().getTextComponent()
						.getSelectionStart() >= MainWindow.getInstance()
						.getOutput().getMaxCaretPosition())
					MainWindow.getInstance().getOutput().getTextComponent().cut();
			}
		});
		add(_cut);

		// PASTE
		_paste = new JMenuItem(labels.getString("s189"));
		_paste.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				
				if (MainWindow.getInstance().getOutput().getTextComponent()
						.getSelectionStart() >= MainWindow.getInstance()
						.getOutput().getMaxCaretPosition())
					MainWindow.getInstance().getOutput().getTextComponent().paste();
			}
		});
		add(_paste);
	}

	/**
	 * Returns the copy menu item.
	 * 
	 * @return The copy menu item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}
	
	/**
	 * Returns the copy menu item.
	 * 
	 * @return The copy menu item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}
	
	/**
	 * Returns the paste menu item.
	 * 
	 * @return The paste menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}
}
