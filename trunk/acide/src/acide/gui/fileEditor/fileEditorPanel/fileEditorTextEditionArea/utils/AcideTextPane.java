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
package acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils;

import acide.configuration.fileEditor.AcideFileEditorConfiguration;
import acide.gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;

import javax.swing.JTextPane;


/**
 * ACIDE - A Configurable IDE text pane.
 */
public class AcideTextPane extends JTextPane {

	/**
	 * ACIDE - A Configurable IDE text pane serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE text pane flag which indicates if the insert
	 * or the over write mode is selected or not.
	 */
	private boolean _isOvertypeMode;
	/**
	 * ACIDE - A Configurable IDE text pane insert mode caret.
	 */
	private AcideInsertModeCaret _insertModeCaret;
	/**
	 * ACIDE - A Configurable IDE text pane overwrite mode caret.
	 */
	private AcideOverwriteModeCaret _overwriteModeCaret;

	/**
	 * Creates a new ACIDE - A Configurable IDE text pane.
	 * @param styledDocument 
	 */
	public AcideTextPane(AcideStyledDocument styledDocument) {

		super(styledDocument);

		// Creates the insert mode caret
		_insertModeCaret = new AcideInsertModeCaret();
		
		// Creates the overwrite mode caret
		_overwriteModeCaret = new AcideOverwriteModeCaret();

		// By default the insert mode is selected
		setEditionMode(AcideFileEditorConfiguration.getInstance().getEditionMode());
	}

	/**
	 * Returns the overwrite/insert mode flag.
	 * 
	 * @return
	 */
	public boolean isOverwriteMode() {
		return _isOvertypeMode;
	}

	/**
	 * Sets the caret to use depending on overwrite/insert mode.
	 * 
	 * @param isOverwriteMode new value to set.
	 */
	public void setEditionMode(boolean isOverwriteMode) {
		
		_isOvertypeMode = isOverwriteMode;
		
		// Gets the caret position
		int caretPosition = getCaretPosition();
		
		if (isOverwriteMode()) {
			setCaret(_overwriteModeCaret);
		} else {
			setCaret(_insertModeCaret);
		}
		setCaretPosition(caretPosition);
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.JTextPane#replaceSelection(java.lang.String)
	 */
	public void replaceSelection(String text) {
		
		// Implement overwrite mode by selecting the character at the current
		// caret position
		
		if (isOverwriteMode()) {
			
			int caretPosition = getCaretPosition();
			
			if (getSelectedText() == null && caretPosition < getDocument().getLength()) {
				moveCaretPosition(caretPosition + 1);
			}
		}
		
		super.replaceSelection(text);
	}
}
