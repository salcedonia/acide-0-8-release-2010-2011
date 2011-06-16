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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.text.StyleConstants;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.fileEditor.fileEditorManager.utils.gui.AcideLineNumberComponent2;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorAdjustmentListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorCaretListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorFocusListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorKeyboardListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorMouseListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorScrollPaneKeyListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners.AcideFileEditorScrollPaneMouseListener;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideFileEditorMouseWheelController;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideFileEditorStyledDocument;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideTextComponent;
import acide.gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import acide.gui.fileEditor.fileEditorPanel.popup.AcideEditorPanelPopupMenuListener;
import acide.gui.listeners.AcideMenuBarKeyboardListener;
import acide.gui.listeners.AcideSearchReplaceWindowKeyboardListener;
import acide.gui.listeners.AcideSearchReplaceWindowMouseListener;
import acide.gui.listeners.AcideStatusBarKeyboardListener;
import acide.gui.mainWindow.AcideMainWindow;

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
	 * ACIDE - A Configurable IDE text edition area text component.
	 */
	private AcideTextComponent _textComponent;
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
	private AcideLineNumberComponent2 _lineNumberPanel;
	/**
	 * ACIDE - A Configurable IDE text edition area matching element position.
	 */
	private int _matchingElementPosition;
	/**
	 * ACIDE - A Configurable IDE file editor mouse wheel controller.
	 */
	private AcideFileEditorMouseWheelController _mouseWheelController;

	/**
	 * Creates a new text edition area.
	 * 
	 * @param styledDocument
	 *            styled document.
	 */
	public AcideFileEditorTextEditionArea(
			AcideFileEditorStyledDocument styledDocument) {

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

		// Creates the scroll pane with the text component
		_scrollPane = new JScrollPane(_textComponent);

		// Creates the line number with the text component
		_lineNumberPanel = new AcideLineNumberComponent2(_textComponent);

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

		// Configures the mouse wheel controller line by line
		setMouseWheelController(new AcideFileEditorMouseWheelController(_scrollPane, 1));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE text edition area text pane.
	 * 
	 * @param styledDocument
	 *            file editor manager styled document.
	 * @return the ACIDE - A Configurable IDE text edition area text pane.
	 */
	protected void buildTextPane(AcideFileEditorStyledDocument styledDocument) {

		// Creates the text component
		_textComponent = new AcideTextComponent(styledDocument) {

			/**
			 * ACIDE - A Configurable IDE text component class serial version
			 * UID.
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

				return AcideWorkbenchConfiguration.getInstance()
						.getFileEditorConfiguration().getLineWrapping();
			}
	         
			/*
			 * Unset the foreground color (if any) whenever the user enters text
			 * (if not for this, text entered after a parent would catch the
			 * paren's color)
			 */
			@Override
			public void replaceSelection(String content) {
				getInputAttributes().removeAttribute(StyleConstants.Foreground);
				super.replaceSelection(content);
			}
		};

		// Updates the name
		_textComponent.setName((String) styledDocument.getProperty("name"));

		// Sets the look and feel from the configuration file
		setLookAndFeel();

		// Sets the text component key bindings
		setTextComponentKeyBindings();

		// Sets the text pane listeners
		setTextPaneListeners();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file editor text edition area key
	 * bindings.
	 */
	private void setTextComponentKeyBindings() {

		// This avoids the interferences with the shorcut at the menu bar
		_textComponent.getInputMap().put(KeyStroke.getKeyStroke("control A"),
				"none");

		// Adds the CTRL+Z action to the text component
		_textComponent.getInputMap().put(KeyStroke.getKeyStroke("control Z"),
				"Undo");

		// Puts the undo action with the CTRL+Z
		_textComponent.getActionMap().put(
				"Undo",
				AcideMainWindow.getInstance().getMenu().getEditMenu()
						.getUndoAction());

		// Adds the CTRL+Y action to the text component
		_textComponent.getInputMap().put(KeyStroke.getKeyStroke("control Y"),
				"Redo");

		// Puts the undo action with the CTRL+Y
		_textComponent.getActionMap().put(
				"Redo",
				AcideMainWindow.getInstance().getMenu().getEditMenu()
						.getRedoAction());
	}

	/**
	 * Sets the configuration from the ACIDE - A Configurable IDE file editor
	 * configuration.
	 */
	public void setLookAndFeel() {

		// Sets the font from the file editor configuration
		_textComponent.setFont(new Font(AcideWorkbenchConfiguration
				.getInstance().getFileEditorConfiguration().getFontName(),
				AcideWorkbenchConfiguration.getInstance()
						.getFileEditorConfiguration().getFontStyle(),
				AcideWorkbenchConfiguration.getInstance()
						.getFileEditorConfiguration().getFontSize()));

		// Sets the foreground color from the file editor configuration
		_textComponent.setForeground(AcideWorkbenchConfiguration.getInstance()
				.getFileEditorConfiguration().getForegroundColor());

		// Sets the Background color from the file editor configuration
		_textComponent.setBackground(AcideWorkbenchConfiguration.getInstance()
				.getFileEditorConfiguration().getBackgroundColor());

		// Sets the caret color from the file editor configuration
		_textComponent.setCaretColor(AcideWorkbenchConfiguration.getInstance()
				.getFileEditorConfiguration().getForegroundColor());
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file editor text edition area text
	 * component listeners.
	 */
	public void setTextPaneListeners() {

		// Sets the ACIDE - A Configurable IDE file editor panel popup menu
		// listener
		_textComponent
				.addMouseListener(new AcideEditorPanelPopupMenuListener());

		// Sets the ACIDE - A Configurable IDE file editor panel mouse listener
		_textComponent.addMouseListener(new AcideFileEditorMouseListener());

		// Adds the ACIDE - A Configurable IDE file editor panel document
		// listener
		_textComponent.getDocument().addDocumentListener(
				new AcideFileEditorPanelDocumentListener());

		// Adds the ACIDE - A Configurable IDE file editor panel caret listener
		_textComponent.addCaretListener(new AcideFileEditorCaretListener());

		// Adds the ACIDE - A Configurable IDE search and replace window
		// keyboard listener
		_textComponent
				.addKeyListener(new AcideSearchReplaceWindowKeyboardListener());

		// Adds the ACIDE - A Configurable IDE search and replace window mouse
		// listener
		_textComponent
				.addMouseListener(new AcideSearchReplaceWindowMouseListener());

		// Adds the ACIDE - A Configurable IDE status bar keyboard listener
		_textComponent.addKeyListener(new AcideStatusBarKeyboardListener());

		// Adds the ACIDE - A Configurable IDE file editor keyboard listener
		_textComponent.addKeyListener(new AcideFileEditorKeyboardListener());

		// Adds the ACIDE - A Configurable IDE focus listener
		_textComponent.addFocusListener(new AcideFileEditorFocusListener());

		// Adds the ACIDE - A Configurable IDE file editor scroll pane listener
		_textComponent
				.addKeyListener(new AcideFileEditorScrollPaneKeyListener());

		// Adds the ACIDE - A Configurable IDE menu bar keyboard listener
		_textComponent.addKeyListener(new AcideMenuBarKeyboardListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area text component.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area text component.
	 */
	public AcideTextComponent getTextComponent() {
		return _textComponent;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text edition area line number
	 * panel.
	 * 
	 * @return the ACIDE - A Configurable IDE text edition area line number
	 *         panel.
	 */
	public AcideLineNumberComponent2 getLineNumberPanel() {
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

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor mouse wheel
	 * controller.
	 * 
	 * @param mouseWheelController
	 *            new value to set.
	 */
	public void setMouseWheelController(
			AcideFileEditorMouseWheelController mouseWheelController) {
		_mouseWheelController = mouseWheelController;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor mouse wheel controller.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor mouse wheel controller.
	 */
	public AcideFileEditorMouseWheelController getMouseWheelController() {
		return _mouseWheelController;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "File editor: " + _textComponent.getName();
	}
}
