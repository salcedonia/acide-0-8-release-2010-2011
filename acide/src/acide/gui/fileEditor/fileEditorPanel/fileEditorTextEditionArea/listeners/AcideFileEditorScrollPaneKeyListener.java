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
package acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JViewport;

import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideTextComponent;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * <p>
 * ACIDE - A Configurable IDE file editor panel arrow keys listener.
 * </p>
 * <p>
 * If the control key is down, the arrow keys handles the scroll pane scroll
 * bars of the active text edition area in the file editor panel.
 * </p>
 * 
 * @version 0.8
 * @see KeyListener
 */
public class AcideFileEditorScrollPaneKeyListener implements KeyListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {
		dispatchEvent(keyEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent keyEvent) {
		// dispatchEvent(keyEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent keyEvent) {
		// dispatchEvent(keyEvent);
	}

	/**
	 * Dispatches the key event.
	 * 
	 * @param keyEvent
	 *            key event.
	 */
	private void dispatchEvent(KeyEvent keyEvent) {

		// Gets the active text edition area
		AcideTextComponent textPane = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getActiveTextEditionArea();

		// Gets the view port from the text pane parent
		JViewport viewport = (JViewport) textPane.getParent();

		// Gets the view rectangle from it
		Rectangle rectangle = viewport.getViewRect();

		// Gets the location
		Point point = rectangle.getLocation();

		// Calculates the increment for the line by line scrolling
		int increment = textPane.getFontMetrics(textPane.getFont()).getHeight() / 2;

		// Control + Up key
		if (keyEvent.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_UP) {

			/*
			 * NOTE: The code above is for setting the scrolling by window int
			 * increment = textPane.getScrollableBlockIncrement(rectangle,
			 * SwingConstants.VERTICAL, -1);
			 */

			// Calculates the new position
			point.y = (point.y - increment >= 0) ? point.y - increment : 0;
		}

		// Control + Down key
		if (keyEvent.isControlDown()
				&& keyEvent.getKeyCode() == KeyEvent.VK_DOWN) {

			/*
			 * NOTE: The code above is for setting the scrolling by window int
			 * increment = textPane.getScrollableBlockIncrement(rectangle,
			 * SwingConstants.VERTICAL, 1);
			 */

			// Calculates the maximum value for the y coordinate
			int maxY = viewport.getView().getHeight() - rectangle.height;

			// Calculates the new position
			point.y = (point.y + increment <= maxY) ? point.y + increment
					: maxY;
		}

		// Updates the location
		viewport.setViewPosition(point);
	}
}
