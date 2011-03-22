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

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE undo manager.
 * 
 * @version 0.8
 * @see UndoManager
 */
public class AcideUndoManager extends UndoManager implements
		UndoableEditListener{

	/**
	 * ACIDE - A Configurable IDE undo manager class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE undo manager unique class instance.
	 */
	private static AcideUndoManager _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE undo manager unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE undo manager unique class instance.
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
	 * Adds a new undoable event to the ACIDE - A Configurable IDE undo manager of the new file which
	 * has been opened in the file editor panel.
	 */
	public void update() {

		// Gets the default styled document
		DefaultStyledDocument document = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getStyledDocument();

		// Adds the undoable edit listener to support undoable events on it
		document.addUndoableEditListener(new UndoableEditListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see javax.swing.event.UndoableEditListener#undoableEditHappened
			 * (javax.swing.event.UndoableEditEvent)
			 */
			@Override
			public void undoableEditHappened(UndoableEditEvent undoableEditEvent) {

				UndoableEdit edit = undoableEditEvent.getEdit();

				if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
						.getType() == DefaultDocumentEvent.EventType.CHANGE))) {

					// Sets the edit property over the general undo manager
					AcideUndoManager.getInstance().addEdit(
							undoableEditEvent.getEdit());
				}
			}
		});
	}
}
