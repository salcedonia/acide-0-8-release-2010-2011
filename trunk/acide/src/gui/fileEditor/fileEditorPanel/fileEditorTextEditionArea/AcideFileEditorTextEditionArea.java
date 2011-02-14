package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea;

import gui.fileEditor.fileEditorManager.utils.gui.LineNumberComponent;
import gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaAdjustmentListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaCaretListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaMouseClickListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaMouseDoubleClickListener;
import gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import gui.fileEditor.fileEditorPanel.popup.AcideEditorPanelPopupMenuListener;
import gui.listeners.AcideKeyboardListener;
import gui.listeners.AcideKeyboardListenerForMenus;
import gui.listeners.AcideSearchAndReplaceWindowMouseListener;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.text.StyleConstants;

import utils.AcideCaret;

/**
 * ACIDE - A Configurable IDE text edition area.
 * 
 * Creates the panel which contains the line number panel and the text
 * area for modifying the file contents.
 * 
 * @version 0.8
 */
public class AcideFileEditorTextEditionArea {

	/**
	 * ACIDE - A Configurable IDE text edition area class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE text edition area scroll pane.
	 */
	private JScrollPane _scrollPane;
	/**
	 * ACIDE - A Configurable IDE text edition area text pane.
	 */
	private JTextPane _textPane;
	/**
	 * ACIDE - A Configurable IDE text edition area vertical value.
	 */
	private int _verticalValue;
	/**
	 * ACIDE - A Configurable IDE text edition area horizontal value.
	 */
	private int _horizontalValue;
	/**
	 * ACIDE - A Configurable IDE text edition area line number panel.
	 */
	private LineNumberComponent _lineNumberPanel;
	/**
	 * ACIDE - A Configurable IDE text edition area matching brace position.
	 */
	private int _matchingBracePosition;
	/**
	 * ACIDE - A Configurable IDE text edition area mouse click listener.
	 */
	private AcideFileEditorTextEditionAreaMouseClickListener _mouseClickListener;
	
	/**
	 * Creates a new text edition area.
	 * 
	 * @param syntaxDocument
	 *            text syntax document.
	 */
	public AcideFileEditorTextEditionArea(AcideStyledDocument syntaxDocument) {

		// No matching braces yet
		_matchingBracePosition = -1;
		
		// Sets the vertical value to 0
		_verticalValue = 0;
		
		// Sets the horizontal value to 0
		_horizontalValue = 0;

		// Builds the text area for edition
		buildTextPane(syntaxDocument);
		
		// Builds the scroll pane
		buildScrollPane();
	}

	/**
	 * Builds the scroll pane and configures it with the text pane inside of it.
	 */
	public void buildScrollPane() {
		
		// Creates the scroll pane
		_scrollPane = new JScrollPane(_textPane);

		// Creates the line number
		_lineNumberPanel = new LineNumberComponent(_textPane);

		// Sets the left side of the scroll pane as the line number panel
		_scrollPane.setRowHeaderView(_lineNumberPanel);

		// Puts the mask over the scroll bars at the right and down part of the
		// panel
		// so they are used as part of the text pane
		_mouseClickListener = new AcideFileEditorTextEditionAreaMouseClickListener();

		// Adds the mouse listener to the vertical scroll bar
		_scrollPane.getVerticalScrollBar()
				.addMouseListener(_mouseClickListener);
		
		// Adds the mouse listener to the horizontal scroll bar
		_scrollPane.getHorizontalScrollBar().addMouseListener(
				_mouseClickListener);
		
		// Adds the mouse click listener to the scroll pane
		_scrollPane.addMouseListener(_mouseClickListener);

		// Adds the mouse click listener to all the components in the vertical bar
		Component verticalComponents[] = _scrollPane.getVerticalScrollBar()
				.getComponents();
		for (int index = 0; index < verticalComponents.length; index++)
			if (verticalComponents[index] instanceof BasicArrowButton)
				verticalComponents[index].addMouseListener(_mouseClickListener);

		// Adds the mouse click listener to all the components in the horizontal bar
		Component horizontalComponent[] = _scrollPane.getHorizontalScrollBar()
				.getComponents();
		for (int index = 0; index < horizontalComponent.length; index++)
			if (horizontalComponent[index] instanceof BasicArrowButton)
				horizontalComponent[index].addMouseListener(_mouseClickListener);
		
		// Sets the minimum size
		_scrollPane.setMinimumSize(new Dimension(0, 0));

		// Adds the adjustment listener
		_scrollPane.getVerticalScrollBar().addAdjustmentListener(
				new AcideFileEditorTextEditionAreaAdjustmentListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE text edition area text pane.
	 * 
	 * @param syntaxDocument
	 *            file editor manager syntax document.
	 * @return the ACIDE - A Configurable IDE text edition area text pane.
	 */
	protected void buildTextPane(AcideStyledDocument syntaxDocument) {

		_textPane = new JTextPane(syntaxDocument) {

			/**
			 * Class serial version UID.
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
			@Override
			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;
				super.setSize(d);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see javax.swing.JEditorPane#getScrollableTracksViewportWidth()
			 */
			@Override
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
			
			/*
			 * Unset the foreground color (if any) whenever the user enters text
			 * (if not for this, text entered after a paren would catch the paren's color)
			 */
			@Override
			public void replaceSelection(String content) {
				getInputAttributes().removeAttribute(StyleConstants.Foreground);
				super.replaceSelection(content);
			}
		};

		// Sets the font
		_textPane.setFont(new Font("monospaced", Font.PLAIN, 12));
		
		// Sets the ACIDE - A Configurable IDE caret
		_textPane.setCaret(new AcideCaret());

		// Sets the popup menu listener
		_textPane.addMouseListener(new AcideEditorPanelPopupMenuListener());
		
		// Sets the double click listener
		_textPane
				.addMouseListener(new AcideFileEditorTextEditionAreaMouseDoubleClickListener());
		
		// Adds the document listener
		_textPane.getDocument().addDocumentListener(new AcideFileEditorPanelDocumentListener());
		
		// Adds the caret listener
		_textPane.addCaretListener(new AcideFileEditorTextEditionAreaCaretListener());
		
		// Adds the general key listener
		_textPane.addKeyListener(new AcideKeyboardListener());
		
		// Adds the general key listener
		_textPane.addMouseListener(new AcideSearchAndReplaceWindowMouseListener());
		
		// Adds the key listener for menus
		_textPane.addKeyListener(new AcideKeyboardListenerForMenus());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area text pane.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area text pane.
	 */
	public JTextPane getTextPane() {
		return _textPane;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area matching brace position.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area matching brace position.
	 */
	public int getMatchingBracePosition() {
		return _matchingBracePosition;
	}

	/**
	 * Sets a new value to the text edition area matching brace position.
	 * 
	 * @param matchingBracePosition
	 *            new value to set.
	 */
	public void setMatchingBracePosition(int matchingBracePosition) {
		_matchingBracePosition = matchingBracePosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area horizontal
	 * value.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area horizontal
	 *         value.
	 */
	public int getHorizontalValue() {
		return _horizontalValue;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE text edition area
	 * horizontal value.
	 * 
	 * @param horizontalValue
	 *            new value to set.
	 */
	public void setHorizontalValue(int horizontalValue) {
		_horizontalValue = horizontalValue;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area vertical value.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area vertical value.
	 */
	public int getVerticalValue() {
		return _verticalValue;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE text edition area
	 * vertical value.
	 * 
	 * @param verticalValue
	 *            new value to set.
	 */
	public void setVerticalValue(int verticalValue) {
		_verticalValue = verticalValue;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area scroll pane.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area scroll pane.
	 */
	public JScrollPane getScrollPane() {
		return _scrollPane;
	}
}
