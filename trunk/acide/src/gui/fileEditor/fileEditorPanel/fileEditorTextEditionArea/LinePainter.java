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
package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import operations.log.AcideLog;

/**
 * Track the movement of the Caret by painting a background line at the current
 * caret position.
 * 
 * @version 0.8
 * @see JComponent
 */
public class LinePainter extends JComponent implements
		Highlighter.HighlightPainter, CaretListener, MouseListener,
		MouseMotionListener {

	/**
	 * Line painter class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Line painter text component.
	 */
	private JTextComponent _textComponent;
	/**
	 * Line painter color.
	 */
	private Color _color;
	/**
	 * Line painter last view.
	 */
	private Rectangle _lastView;

	/**
	 * The line color will be calculated automatically by attempting to make the
	 * current selection lighter by a factor of 1.2.
	 * 
	 * @param textComponent
	 *            text component that requires background line painting.
	 */
	public LinePainter(JTextComponent textComponent) {
		this(textComponent, null);
		setLighter(textComponent.getSelectionColor());
	}

	/**
	 * Manually control the line color.
	 * 
	 * @param textComponent
	 *            text component that requires background line painting.
	 * @param color
	 *            the color of the background line.
	 */
	public LinePainter(JTextComponent textComponent, Color color) {

		_textComponent = textComponent;
		setColor(color);

		// Add listeners so we know when to change highlighting

		//textComponent.addCaretListener(this);
		textComponent.addMouseListener(this);
		textComponent.addMouseMotionListener(this);

		// Turn highlighting on by adding a dummy highlight

		try {
			textComponent.getHighlighter().addHighlight(0, 0, this);
		} catch (BadLocationException badLocationException) {

			// Updates the log
			AcideLog.getLog().error(badLocationException.getMessage());
			badLocationException.printStackTrace();
		}
	}

	/**
	 * You can reset the line color at any time.
	 * 
	 * @param color
	 *            the color of the background line.
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Calculate the line color by making the selection color lighter.
	 * 
	 * @return the color of the background line.
	 */
	public void setLighter(Color color) {
		setColor(new Color(235, 235, 255));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.text.Highlighter.HighlightPainter#paint(java.awt.Graphics,
	 * int, int, java.awt.Shape, javax.swing.text.JTextComponent)
	 */
	@Override
	public void paint(Graphics g, int p0, int p1, Shape bounds, JTextComponent c) {
		try {

			Rectangle r = _textComponent.getUI().modelToView(c, c.getCaretPosition());
			g.setColor(_color);
			g.fillRect(0, r.y, c.getWidth(), r.height);

			if (_lastView == null)
				_lastView = r;

		} catch (BadLocationException badLocationException) {

			// Updates the log
			// AcideLog.getLog().error(badLocationException.getMessage());
			// badLocationException.printStackTrace();
		}
	}

	/**
	 * Caret position has changed, remove the highlight.
	 */
	private void resetHighlight() {

		// Uses invokeLater to make sure updates to the Document are completed,
		// otherwise Undo processing causes the modelToView method to loop.
		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				try {

					int offset = _textComponent.getCaretPosition();
					Rectangle currentView = _textComponent.modelToView(offset);

					// Removes the highlighting from the previously highlighted
					// line
					if (_lastView != null)
						if (_lastView.y != currentView.y) {
							_textComponent
									.repaint(0, _lastView.y,
											_textComponent.getWidth(),
											_lastView.height);
							_lastView = currentView;
						}
				} catch (BadLocationException badLocationException) {

					// Updates the log
					// AcideLog.getLog().error(badLocationException.getMessage());
					// badLocationException.printStackTrace();
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
	 */
	@Override
	public void caretUpdate(CaretEvent caretEvent) {
		resetHighlight();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		resetHighlight();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
	 */
	public void mouseEntered(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseExited(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.MouseEvent
	 * )
	 */
	@Override
	public void mouseDragged(MouseEvent mouseEvent) {
		resetHighlight();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseMotionListener#mouseMoved(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseMoved(MouseEvent mouseEvent) {
	}
}
