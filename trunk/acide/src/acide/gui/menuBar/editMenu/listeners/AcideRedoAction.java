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
package acide.gui.menuBar.editMenu.listeners;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoManager;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.undo.CannotRedoException;

import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE redo action.
 * 
 * @version 0.8
 * @see AbstractAction
 */
public class AcideRedoAction extends AbstractAction {

	/**
	 * ACIDE - A Configurable IDE redo action serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new ACIDE - A Configurable IDE redo action.
	 * 
	 * @param name
	 *            action name.
	 */
	public AcideRedoAction(String name) {

		super(name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// Gets the selected file editor panel index
			int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// If there are opened editors
			if (selectedFileEditorPanelIndex != -1) {

				// If it can redo
				if (AcideUndoManager.getInstance().canRedo())

					// Redo
					AcideUndoManager.getInstance().redo();

				// Updates the redo menu item option
				AcideMainWindow.getInstance().getMenu().getEditMenu()
						.getUndoMenuItem()
						.setEnabled(AcideUndoManager.getInstance().canRedo());
			}
		} catch (CannotRedoException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
