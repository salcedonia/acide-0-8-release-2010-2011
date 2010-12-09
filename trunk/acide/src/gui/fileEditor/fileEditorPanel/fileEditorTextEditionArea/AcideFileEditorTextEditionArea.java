package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea;

import gui.fileEditor.fileEditorManager.utils.gui.LineNumberComponent;
import gui.fileEditor.fileEditorManager.utils.logic.SyntaxDocument;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideTextEditorTextEditionAreaAdjustmentListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideTextEditorTextEditionAreaCaretListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideTextEditorTextEditionAreaKeyboardListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideTextEditorTextEditionAreaMouseClickListener;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideTextEditorTextEditionAreaMouseDoubleClickListener;
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

/************************************************************************
 * Text edition area of ACIDE - A Configurable IDE. Creates the panel which
 * contains the line number panel and the editable text area for modifying the
 * file contents.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 ***********************************************************************/
public class AcideFileEditorTextEditionArea {

	/**
	 * Text edition area class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Text edition area scroll pane.
	 */
	private JScrollPane _scrollPane;
	/**
	 * Text edition area text pane.
	 */
	private JTextPane _textPane;
	/**
	 * Text edition area vertical value.
	 */
	private int _verticalValue;
	/**
	 * Text edition area horizontal value.
	 */
	private int _horizontalValue;
	/**
	 * Text edition area line number panel.
	 */
	private LineNumberComponent _lineNumberPanel;
	/**
	 * Text edition area brace matcher.
	 */
	private int _braceMatcher;
	/**
	 * Text edition area mouse click listener.
	 */
	private AcideTextEditorTextEditionAreaMouseClickListener _mouseClickListener;
	/**
	 * Text edition area document document listener.
	 */
	private AcideFileEditorPanelDocumentListener _documentListener;
	/**
	 * Text edition area caret listener.
	 */
	private AcideTextEditorTextEditionAreaCaretListener _caretListener;
	/**
	 * Text edition area adjustment listener.
	 */
	private AcideTextEditorTextEditionAreaAdjustmentListener _adjustmentListener;

	/**
	 * Creates a new text edition area.
	 * 
	 * @param syntaxDocument
	 *            text syntax document.
	 */
	public AcideFileEditorTextEditionArea(SyntaxDocument syntaxDocument) {

		_braceMatcher = -1;
		_verticalValue = 0;
		_horizontalValue = 0;

		// Builds the text area for edition
		buildEditor(syntaxDocument);
		
		_textPane
				.addKeyListener(new AcideTextEditorTextEditionAreaKeyboardListener());
		_textPane.addMouseListener(new AcideEditorPanelPopupMenuListener());
		_textPane
				.addMouseListener(new AcideTextEditorTextEditionAreaMouseDoubleClickListener());
		
		// Adds the line painter
		//LinePainter linePainter = new LinePainter(_textPane);
		_scrollPane = new JScrollPane(_textPane);

		// Creates the line number
		_lineNumberPanel = new LineNumberComponent(_textPane);

		// Sets the left side of the scroll pane as the line number panel
		_scrollPane.setRowHeaderView(_lineNumberPanel);

		// Puts the mask over the scroll bars at the right and down part of the
		// panel
		// so they are used as part of the text pane
		_mouseClickListener = new AcideTextEditorTextEditionAreaMouseClickListener();

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
		_caretListener = new AcideTextEditorTextEditionAreaCaretListener();
		_textPane.addCaretListener(_caretListener);
		_scrollPane.setMinimumSize(new Dimension(0, 0));

		_adjustmentListener = new AcideTextEditorTextEditionAreaAdjustmentListener();
		_scrollPane.getVerticalScrollBar().addAdjustmentListener(
				_adjustmentListener);

		_textPane.addKeyListener(new AcideKeyboardListener());
		_textPane.addMouseListener(new AcideMouseListener());
		_textPane.addKeyListener(new AcideKeyboardListenerForMenus());
	}

	/**
	 * Builds the text edition area text pane.
	 * 
	 * @param syntaxDocument
	 *            file editor manager syntax document.
	 * @return the text edition area text pane.
	 */
	protected void buildEditor(SyntaxDocument syntaxDocument) {

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
	 * Returns the text edition area text pane.
	 * 
	 * @return the text edition area text pane.
	 */
	public JTextPane getTextPane() {
		return _textPane;
	}

	/**
	 * Returns the text edition area brace matcher.
	 * 
	 * @return the text edition area brace matcher.
	 */
	public int getBraceMatcher() {
		return _braceMatcher;
	}

	/**
	 * Sets a new value to the text edition area brace matcher.
	 * 
	 * @param braceMatcher
	 *            new value to set.
	 */
	public void setBraceMatcher(int braceMatcher) {
		_braceMatcher = braceMatcher;
	}

	/**
	 * Returns the text edition area horizontal value.
	 * 
	 * @return the text edition area horizontal value.
	 */
	public int getHorizontalValue() {
		return _horizontalValue;
	}

	/**
	 * Sets a new value to the horizontal value.
	 * 
	 * @param horizontalValue
	 *            new value to set.
	 */
	public void setHorizontalValue(int horizontalValue) {
		_horizontalValue = horizontalValue;
	}

	/**
	 * Returns the vertical value.
	 * 
	 * @return the vertical value.
	 */
	public int getVerticalValue() {
		return _verticalValue;
	}

	/**
	 * Sets a new value to the text edition area vertical value.
	 * 
	 * @param verticalValue
	 *            new value to set.
	 */
	public void setVerticalValue(int verticalValue) {
		_verticalValue = verticalValue;
	}

	/**
	 * Returns the text edition area scroll pane.
	 * 
	 * @return the text edition area scroll pane.
	 */
	public JScrollPane getScrollPane() {
		return _scrollPane;
	}
}
