package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea;

import gui.fileEditor.fileEditorManager.utils.gui.LineNumberComponent;
import gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaAdjustmentListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaCaretListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaMouseClickListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorTextEditionAreaMouseDoubleClickListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideCaret;
import gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import gui.fileEditor.fileEditorPanel.popup.AcideEditorPanelPopupMenuListener;
import gui.listeners.AcideKeyboardListener;
import gui.listeners.AcideKeyboardListenerForMenus;
import gui.listeners.AcideMouseListener;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.plaf.basic.BasicArrowButton;

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
	 * ACIDE - A Configurable IDE text edition area document listener.
	 */
	private AcideFileEditorPanelDocumentListener _documentListener;
	/**
	 * ACIDE - A Configurable IDE text edition area caret listener.
	 */
	private AcideFileEditorTextEditionAreaCaretListener _caretListener;
	/**
	 * ACIDE - A Configurable IDE text edition area adjustment listener.
	 */
	private AcideFileEditorTextEditionAreaAdjustmentListener _adjustmentListener;
	
	/**
	 * Creates a new text edition area.
	 * 
	 * @param syntaxDocument
	 *            text syntax document.
	 */
	public AcideFileEditorTextEditionArea(AcideStyledDocument syntaxDocument) {

		_matchingBracePosition = -1;
		_verticalValue = 0;
		_horizontalValue = 0;

		// Builds the text area for edition
		buildEditor(syntaxDocument);

		// Sets the ACIDE - A Configurable IDE caret
		AcideCaret caret = new AcideCaret();
		caret.setBlinkRate(500);
		_textPane.setCaret(caret);

		// Sets the listeners
		_textPane.addMouseListener(new AcideEditorPanelPopupMenuListener());
		_textPane
				.addMouseListener(new AcideFileEditorTextEditionAreaMouseDoubleClickListener());
		
		_scrollPane = new JScrollPane(_textPane);

		// Creates the line number
		_lineNumberPanel = new LineNumberComponent(_textPane);

		// Sets the left side of the scroll pane as the line number panel
		_scrollPane.setRowHeaderView(_lineNumberPanel);

		// Puts the mask over the scroll bars at the right and down part of the
		// panel
		// so they are used as part of the text pane
		_mouseClickListener = new AcideFileEditorTextEditionAreaMouseClickListener();

		_scrollPane.getVerticalScrollBar()
				.addMouseListener(_mouseClickListener);
		_scrollPane.getHorizontalScrollBar().addMouseListener(
				_mouseClickListener);
		_scrollPane.addMouseListener(_mouseClickListener);

		Component verticalComponents[] = _scrollPane.getVerticalScrollBar()
				.getComponents();
		for (int i = 0; i < verticalComponents.length; i++)
			if (verticalComponents[i] instanceof BasicArrowButton)
				verticalComponents[i].addMouseListener(_mouseClickListener);

		Component horizontalComponent[] = _scrollPane.getHorizontalScrollBar()
				.getComponents();
		for (int i = 0; i < horizontalComponent.length; i++)
			if (horizontalComponent[i] instanceof BasicArrowButton)
				horizontalComponent[i].addMouseListener(_mouseClickListener);

		_documentListener = new AcideFileEditorPanelDocumentListener();
		_textPane.getDocument().addDocumentListener(_documentListener);
		_caretListener = new AcideFileEditorTextEditionAreaCaretListener();
		_textPane.addCaretListener(_caretListener);
		_scrollPane.setMinimumSize(new Dimension(0, 0));

		_adjustmentListener = new AcideFileEditorTextEditionAreaAdjustmentListener();
		_scrollPane.getVerticalScrollBar().addAdjustmentListener(
				_adjustmentListener);

		_textPane.addKeyListener(new AcideKeyboardListener());
		_textPane.addMouseListener(new AcideMouseListener());
		_textPane.addKeyListener(new AcideKeyboardListenerForMenus());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE text edition area text pane.
	 * 
	 * @param syntaxDocument
	 *            file editor manager syntax document.
	 * @return the ACIDE - A Configurable IDE text edition area text pane.
	 */
	protected void buildEditor(AcideStyledDocument syntaxDocument) {

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
		};

		_textPane.setFont(new Font("monospaced", Font.PLAIN, 12));
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
