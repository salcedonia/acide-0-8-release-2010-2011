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

import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;

/**
 * ACIDE - A Configurable IDE caret. A wider caret to highlight in a more
 * intense way the caret position in the edition text areas.
 * 
 * @version 0.8
 */
public class AcideCaret extends DefaultCaret {

	/**
	 * ACIDE - A Configurable IDE caret class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Dot bias to paint the caret.
	 */
	transient Position.Bias _dotBias;
	
	/**
	 * Creates a new instance of ACIDE - A Configurable IDE caret.
	 */
	public AcideCaret() {
		super();
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.text.DefaultCaret#paint(java.awt.Graphics)
	 */
	public void paint(Graphics g) {
		
		if (isVisible()) {
			try {
				
				JTextComponent component = getComponent();
				Rectangle r = component.getUI().modelToView(component, getDot(), _dotBias);

				if ((r == null) || ((r.width == 0) && (r.height == 0))) {
					return;
				}
				if (width > 0 && height > 0
						&& !this._contains(r.x, r.y, r.width, r.height)) {
					// We seem to have gotten out of sync and no longer
					// contain the right location, adjust accordingly.
					Rectangle clip = g.getClipBounds();

					if (clip != null && !clip.contains(this)) {
						// Clip doesn't contain the old location, force it
						// to be repainted lest we leave a caret around.
						repaint();
					}
					// This will potentially cause a repaint of something
					// we're already repainting, but without changing the
					// semantics of damage we can't really get around
					this.damage(r);
				}
				g.setColor(component.getCaretColor());
				g.fillRect(r.x, r.y, 2, r.height);
				
			} catch (BadLocationException badLocationException) {
				// Updates the log
				//AcideLog.getLog().error(badLocationException.getMessage());
				//badLocationException.printStackTrace();
			}
		}

	}

	/**
	 * Rectangle.contains returns false if passed a rectangle with a w or h == 0,
	 * this won't (assuming X,Y are contained with this rectangle).
	 * 
	 * @param X x coordinate.
	 * @param Y y coordinate.
	 * @param W text area width.
	 * @param H text area height.
	 * 
	 * @return true if it contains it and false in other case.
	 */
	private boolean _contains(int X, int Y, int W, int H) {
		
		int w = this.width;
		int h = this.height;
		
		if ((w | h | W | H) < 0) {
			// At least one of the dimensions is negative...
			return false;
		}
		// Note: if any dimension is zero, tests below must return false...
		int x = this.x;
		int y = this.y;
		if (X < x || Y < y) {
			return false;
		}
		if (W > 0) {
			w += x;
			W += X;
			if (W <= X) {
				// X+W overflowed or W was zero, return false if...
				// either original w or W was zero or
				// x+w did not overflow or
				// the overflowed x+w is smaller than the overflowed X+W
				if (w >= x || W > w)
					return false;
			} else {
				// X+W did not overflow and W was not zero, return false if...
				// original w was zero or
				// x+w did not overflow and x+w is smaller than X+W
				if (w >= x && W > w)
					return false;
			}
		} else if ((x + w) < X) {
			return false;
		}
		if (H > 0) {
			h += y;
			H += Y;
			if (H <= Y) {
				if (h >= y || H > h)
					return false;
			} else {
				if (h >= y && H > h)
					return false;
			}
		} else if ((y + h) < Y) {
			return false;
		}
		return true;
	}
}