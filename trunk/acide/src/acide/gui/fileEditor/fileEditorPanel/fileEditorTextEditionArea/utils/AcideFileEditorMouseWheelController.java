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

import java.awt.event.*;

import javax.swing.*;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * <p>
 * ACIDE - A Configurable IDE file editor mouse wheel controller.
 * </p>
 * <p>
 * This class allows you to control the units scrolled for each mouse wheel
 * rotation relative to the unit increment value of the scroll bar. Specifying a
 * scroll amount of 1, is equivalent to clicking the unit scroll button of the
 * scroll bar once.
 * </p>
 */
public class AcideFileEditorMouseWheelController implements MouseWheelListener {
	
	/**
	 * Scroll pane to apply the mouse wheel on.
	 */
	private JScrollPane _scrollPane;
	/**
	 * The scroll amount.
	 */
	private int _scrollAmount = 0;
	/**
	 * Array wich contains the real listeners.
	 */
	private MouseWheelListener[] _realListeners;

	/**
	 * Convenience constructor to create the class with a scroll amount of 1.
	 * 
	 * @param scrollPane
	 *            the scroll pane being used by the mouse wheel
	 */
	public AcideFileEditorMouseWheelController(JScrollPane scrollPane) {
		this(scrollPane, 1);
	}

	/**
	 * Create the class with the specified scroll amount.
	 * 
	 * @param scrollAmount
	 *            the scroll amount to by used for this scroll pane
	 * @param scrollPane
	 *            the scroll pane being used by the mouse wheel
	 */
	public AcideFileEditorMouseWheelController(JScrollPane scrollPane, int scrollAmount) {
		this._scrollPane = scrollPane;
		setScrollAmount(scrollAmount);
		install();
	}

	/**
	 * Get the scroll amount
	 * 
	 * @returns the scroll amount.
	 */
	public int getScrollAmount() {
		return _scrollAmount;
	}

	/**
	 * Set the scroll amount. Controls the amount the scrollpane will scroll for
	 * each mouse wheel rotation. The amount is relative to the unit increment
	 * value of the scrollbar being scrolled.
	 * 
	 * @param scrollAmount
	 *            an integer value. A value of zero will use the default scroll
	 *            amount for your OS.
	 */
	public void setScrollAmount(int scrollAmount) {
		this._scrollAmount = scrollAmount;
	}

	/**
	 * Install this class as the default listener for MouseWheel events.
	 */
	public void install() {
		if (_realListeners != null)
			return;

		// Keep track of original listeners so we can use them to
		// redispatch an altered MouseWheelEvent

		_realListeners = _scrollPane.getMouseWheelListeners();

		for (MouseWheelListener mwl : _realListeners) {
			_scrollPane.removeMouseWheelListener(mwl);
		}

		// Intercept events so they can be redispatched

		_scrollPane.addMouseWheelListener(this);
	}

	/**
	 * Remove the class as the default listener and reinstall the original
	 * listeners.
	 */
	public void uninstall() {
		if (_realListeners == null)
			return;

		// Remove this class as the default listener

		_scrollPane.removeMouseWheelListener(this);

		// Install the default listeners

		for (MouseWheelListener mwl : _realListeners) {
			_scrollPane.addMouseWheelListener(mwl);
		}

		_realListeners = null;
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseWheelListener#mouseWheelMoved(java.awt.event.MouseWheelEvent)
	 */
	@Override
	public void mouseWheelMoved(MouseWheelEvent mouseWheelEvent) {
		
		if (_scrollAmount != 0) {

			if (mouseWheelEvent.isControlDown()) {

				// Performs the font zoom action
				fontZoom(mouseWheelEvent);
			} else {
				
				// Create an altered event to redispatch
				mouseWheelEvent = createScrollAmountEvent(mouseWheelEvent);
			}
		}

		// Redispatches the event to original MouseWheelListener
		for (MouseWheelListener mwl : _realListeners) {
			mwl.mouseWheelMoved(mouseWheelEvent);
		}
	}

	/**
	 * Resets the scroll amount.
	 * 
	 * @param e
	 *            mouse wheel event.
	 * 
	 * @return the mouse wheel event with the reset scroll amount.
	 */
	private MouseWheelEvent createScrollAmountEvent(MouseWheelEvent e) {
		// Reset the scroll amount

		MouseWheelEvent mwe = new MouseWheelEvent(e.getComponent(), e.getID(),
				e.getWhen(), e.getModifiers(), e.getX(), e.getY(),
				e.getXOnScreen(), e.getYOnScreen(), e.getClickCount(),
				e.isPopupTrigger(), e.getScrollType(), _scrollAmount,
				e.getWheelRotation());

		return mwe;
	}
	
	/**
	 * Performs the font zoom operation based on the mouse wheel value.
	 * 
	 * @param mouseWheelEvent
	 *            mouse wheel event.
	 */
	private void fontZoom(MouseWheelEvent mouseWheelEvent) {

		// Apply the changes to the opened file editor panels
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			// Sets the new font style
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(index)
					.zoomFont(mouseWheelEvent.getScrollAmount(),
							mouseWheelEvent.getWheelRotation() < 0);

			// Resets the selected file editor text edition area
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().resetStyledDocument();
		}

		// Saves the file editor configuration
		AcideWorkbenchConfiguration.getInstance().getFileEditorConfiguration()
				.save();
	}
}
