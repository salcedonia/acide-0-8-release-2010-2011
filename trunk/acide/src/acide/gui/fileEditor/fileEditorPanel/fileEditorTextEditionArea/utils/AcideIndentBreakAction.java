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

import java.awt.event.ActionEvent;

import javax.swing.UIManager;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.log.AcideLog;

/**
 * <p>
 * ACIDE - A Configurable IDE indent break action.
 * </p>
 * <p>
 * Allows the indents breaks on the ACIDE - A Configurable IDE text panes. When
 * the user presses the enter key the text will continue in the same position
 * than the line where the enter key was pressed down to keep the order.
 * </p>
 * 
 * @version 0.8
 * @see TextAction
 */
public class AcideIndentBreakAction extends TextAction {

	/**
	 * ACIDE - A Configurable IDE indent break action serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates this object with the appropriate identifier.
	 */
	public AcideIndentBreakAction() {
		super(DefaultEditorKit.insertBreakAction);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the target
		JTextComponent target = getTextComponent(actionEvent);
		if (target == null)
			return;
		if ((!target.isEditable()) || (!target.isEnabled())) {
			UIManager.getLookAndFeel().provideErrorFeedback(target);
			return;
		}
		try {

			// If the option is activated in the file editor configuration
			if (AcideWorkbenchConfiguration.getInstance()
					.getFileEditorConfiguration().getAutomaticIndent()) {

				// Determine which line we are on
				Document doc = target.getDocument();
				Element rootElement = doc.getDefaultRootElement();
				int selectionStart = target.getSelectionStart();
				int line = rootElement.getElementIndex(selectionStart);
				// Get the text for this line
				int start = rootElement.getElement(line).getStartOffset();
				int end = rootElement.getElement(line).getEndOffset();
				int length = end - start;
				String text = doc.getText(start, length);
				int offset = 0;
				// Get the number of white spaces characters at the start of the
				// line
				for (offset = 0; offset < length; offset++) {
					char c = text.charAt(offset);
					if (c != ' ' && c != '\t')
						break;
				}

				// When splitting the text include white space at start of line
				// else do default processing
				if (selectionStart - start > offset)
					target.replaceSelection("\n" + text.substring(0, offset));
				else
					target.replaceSelection("\n");
			} else
				// Just a simple return carriage
				target.replaceSelection("\n");
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

		}
	}
}