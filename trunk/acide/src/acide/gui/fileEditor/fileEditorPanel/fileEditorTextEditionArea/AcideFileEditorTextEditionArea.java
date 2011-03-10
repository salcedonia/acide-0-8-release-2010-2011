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
package acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea;

import acide.configuration.fileEditor.AcideFileEditorConfiguration;
import acide.gui.fileEditor.fileEditorManager.utils.gui.AcideLineNumberComponent;
import acide.gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorAdjustmentListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorCaretListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorMouseListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorScrollPaneMouseListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideTextPane;
import acide.gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import acide.gui.fileEditor.fileEditorPanel.popup.AcideEditorPanelPopupMenuListener;
import acide.gui.listeners.AcideKeyboardListener;

import acide.gui.listeners.AcideKeyboardListenerForMenus;
import acide.gui.listeners.AcideSearchAndReplaceWindowMouseListener;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JScrollPane;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.text.StyleConstants;

/**
 * ACIDE - A Configurable IDE text edition area.
 * 
 * Creates the panel which contains the line number panel and the text area for
 * modifying the file contents.
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
	private AcideTextPane _textPane;
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
	private AcideLineNumberComponent _lineNumberPanel;
	/**
	 * ACIDE - A Configurable IDE text edition area matching element position.
	 */
	private int _matchingElementPosition;

	/**
	 * Creates a new text edition area.
	 * 
	 * @param styledDocument
	 *            styled document.
	 */
	public AcideFileEditorTextEditionArea(AcideStyledDocument styledDocument) {

		// No matching braces yet
		_matchingElementPosition = -1;

		// Sets the vertical value to 0
		_verticalValue = 0;

		// Sets the horizontal value to 0
		_horizontalValue = 0;

		// Builds the text area for edition
		buildTextPane(styledDocument);

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
		_lineNumberPanel = new AcideLineNumberComponent(_textPane);

		// Sets the left side of the scroll pane as the line number panel
		_scrollPane.setRowHeaderView(_lineNumberPanel);

		// Puts the mask over the scroll bars at the right and down part of the
		// panel
		// so they are used as part of the text pane
		AcideFileEditorScrollPaneMouseListener scrollPaneMouseListener = new AcideFileEditorScrollPaneMouseListener();

		// Adds the mouse listener to the vertical scroll bar
		_scrollPane.getVerticalScrollBar().addMouseListener(
				scrollPaneMouseListener);

		// Adds the mouse listener to the horizontal scroll bar
		_scrollPane.getHorizontalScrollBar().addMouseListener(
				scrollPaneMouseListener);

		// Adds the mouse click listener to the scroll pane
		_scrollPane.addMouseListener(scrollPaneMouseListener);

		// Adds the mouse click listener to all the components in the vertical
		// bar
		Component verticalComponents[] = _scrollPane.getVerticalScrollBar()
				.getComponents();
		for (int index = 0; index < verticalComponents.length; index++)
			if (verticalComponents[index] instanceof BasicArrowButton)
				verticalComponents[index]
						.addMouseListener(scrollPaneMouseListener);

		// Adds the mouse click listener to all the components in the horizontal
		// bar
		Component horizontalComponent[] = _scrollPane.getHorizontalScrollBar()
				.getComponents();
		for (int index = 0; index < horizontalComponent.length; index++)
			if (horizontalComponent[index] instanceof BasicArrowButton)
				horizontalComponent[index]
						.addMouseListener(scrollPaneMouseListener);

		// Sets the minimum size
		_scrollPane.setMinimumSize(new Dimension(0, 0));

		// Adds the adjustment listener
		_scrollPane.getVerticalScrollBar().addAdjustmentListener(
				new AcideFileEditorAdjustmentListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE text edition area text pane.
	 * 
	 * @param styledDocument
	 *            file editor manager styled document.
	 * @return the ACIDE - A Configurable IDE text edition area text pane.
	 */
	protected void buildTextPane(AcideStyledDocument styledDocument) {

		_textPane = new AcideTextPane(styledDocument) {

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
			 * (if not for this, text entered after a paren would catch the
			 * paren's color)
			 */
			@Override
			public void replaceSelection(String content) {
				getInputAttributes().removeAttribute(StyleConstants.Foreground);
				super.replaceSelection(content);
			}
		};

		// Sets the configuration from the configuration file
		setConfiguration();

		// Sets the text pane listeners
		setTextPaneListeners();
	}

	/**
	 * Sets the configuration from the configuration file.
	 */
	public void setConfiguration() {

		// Sets the font from the file editor configuration
		_textPane.setFont(new Font(AcideFileEditorConfiguration.getInstance()
				.getFontName(), AcideFileEditorConfiguration.getInstance()
				.getFontStyle(), AcideFileEditorConfiguration.getInstance()
				.getFontSize()));

		// Sets the foreground color from the file editor configuration
		_textPane.setForeground(AcideFileEditorConfiguration.getInstance()
				.getForegroundColor());

		// Sets the Background color from the file editor configuration
		_textPane.setBackground(AcideFileEditorConfiguration.getInstance()
				.getBackgroundColor());

		// Sets the caret color from the file editor configuration
		_textPane.setCaretColor(AcideFileEditorConfiguration.getInstance()
				.getForegroundColor());
	}

	/**
	 * Sets the text pane listeners.
	 */
	public void setTextPaneListeners() {

		// Sets the popup menu listener
		_textPane.addMouseListener(new AcideEditorPanelPopupMenuListener());

		// Sets the double click listener
		_textPane.addMouseListener(new AcideFileEditorMouseListener());

		// Adds the document listener
		_textPane.getDocument().addDocumentListener(
				new AcideFileEditorPanelDocumentListener());

		// Adds the caret listener
		_textPane.addCaretListener(new AcideFileEditorCaretListener());

		// Adds the general key listener
		_textPane.addKeyListener(new AcideKeyboardListener());

		// Adds the search and replace window mouse listener key listener
		_textPane
				.addMouseListener(new AcideSearchAndReplaceWindowMouseListener());

		// Adds the key listener for menus
		_textPane.addKeyListener(new AcideKeyboardListenerForMenus());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area text pane.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area text pane.
	 */
	public AcideTextPane getTextPane() {
		return _textPane;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area line number
	 * panel.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area line number
	 *         panel.
	 */
	public AcideLineNumberComponent getLineNumberPanel() {
		return _lineNumberPanel;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area matching brace
	 * position.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area matching element
	 *         position.
	 */
	public int getMatchingElementPosition() {
		return _matchingElementPosition;
	}

	/**
	 * Sets a new value to the text edition area matching element position.
	 * 
	 * @param matchingElementPosition
	 *            new value to set.
	 */
	public void setMatchingElementPosition(int matchingElementPosition) {
		_matchingElementPosition = matchingElementPosition;
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
