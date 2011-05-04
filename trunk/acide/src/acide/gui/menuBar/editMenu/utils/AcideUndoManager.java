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
package acide.gui.menuBar.editMenu.utils;

import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.text.JTextComponent;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideTextComponent;

/**
 * ACIDE - A Configurable IDE undo manager.
 * 
 * @version 0.8
 * @see UndoManager
 * @see UndoableEditListener
 * @see DocumentListener
 */
public class AcideUndoManager extends UndoManager implements
		UndoableEditListener, DocumentListener {

	/**
	 * ACIDE - A Configurable IDE undo manager class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE undo manager unique class instance.
	 */
	private static AcideUndoManager _instance;
	/**
	 * ACIDE - A Configurable IDE undo manager text component.
	 */
	private JTextComponent _textComponent;

	/**
	 * Returns the ACIDE - A Configurable IDE undo manager unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE undo manager unique class
	 *         instance.
	 */
	public static AcideUndoManager getInstance() {

		if (_instance == null)
			_instance = new AcideUndoManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE undo manager.
	 */
	public AcideUndoManager() {
		super();
	}

	/**
	 * Adds a new undoable event to the ACIDE - A Configurable IDE undo manager
	 * of the new file which has been opened in the file editor panel.
	 * 
	 * @param textComponent
	 *            new value to set.
	 */
	public void setTextComponent(AcideTextComponent textComponent) {

		// Stores the text component
		_textComponent = textComponent;

		// Adds the undoable edit listener to support undoable events on it
		_textComponent.getDocument().addUndoableEditListener(
				new UndoableEditListener() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see
					 * javax.swing.event.UndoableEditListener#undoableEditHappened
					 * (javax.swing.event.UndoableEditEvent)
					 */
					@Override
					public void undoableEditHappened(
							UndoableEditEvent undoableEditEvent) {

						UndoableEdit edit = undoableEditEvent.getEdit();

						if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
								.getType() == DefaultDocumentEvent.EventType.CHANGE))) {

							// Sets the edit property over the general undo
							// manager
							AcideUndoManager.getInstance().addEdit(
									undoableEditEvent.getEdit());
						}
					}
				});
	}
	
	/*
	 * Add a DocumentLister before the undo is done so we can position the Caret
	 * correctly as each edit is undone.
	 */
	@Override
	public void undo() {

		// Adds a document listener
		_textComponent.getDocument().addDocumentListener(this);

		// Performs the undo action
		super.undo();

		// Removes the document listener
		_textComponent.getDocument().removeDocumentListener(this);
	}

	/*
	 * Adds a DocumentLister before the redo is done so we can position the
	 * Caret correctly as each edit is redone.
	 */
	@Override
	public void redo() {

		// Adds a document listener
		_textComponent.getDocument().addDocumentListener(this);

		// Performs the redo action
		super.redo();

		// Removes the document listener
		_textComponent.getDocument().removeDocumentListener(this);

	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void insertUpdate(final DocumentEvent documentEvent) {
		
		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				// Gets the offset
				int offset = documentEvent.getOffset() + documentEvent.getLength();
				offset = Math.min(offset, _textComponent.getDocument()
						.getLength());
				
				// Updates the caret position
				_textComponent.setCaretPosition(offset);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void removeUpdate(final DocumentEvent documentEvent) {
		
		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				// Updates the caret position
				_textComponent.setCaretPosition(documentEvent.getOffset());
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {
	}
}
