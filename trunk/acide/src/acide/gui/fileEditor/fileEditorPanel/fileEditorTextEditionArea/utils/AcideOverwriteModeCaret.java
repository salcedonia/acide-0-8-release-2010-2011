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

import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;

/**
 * ACIDE - A Configurable IDE overwrite mode caret.
 * 
 * Paints a horizontal line the width of a column and 1 pixel high in the text
 * edition areas when the overwrite mode is selected.
 */
public class AcideOverwriteModeCaret extends DefaultCaret {
	
	/**
	 * ACIDE - A Configurable IDE overwrite mode caret serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new instance of ACIDE - A Configurable IDE overwrite mode caret.
	 */
	public AcideOverwriteModeCaret() {
	
		super();

		// Sets the blink rate to 500 miliseconds
		setBlinkRate(500);
	}
	
	/*
	 * The over write caret will simply be a horizontal line one pixel high
	 * (once we determine where to paint it) (non-Javadoc)
	 * 
	 * @see javax.swing.text.DefaultCaret#paint(java.awt.Graphics)
	 */
	public void paint(Graphics g) {
		if (isVisible()) {
			try {
				JTextComponent component = getComponent();
				TextUI mapper = component.getUI();
				Rectangle r = mapper.modelToView(component, getDot());
				g.setColor(component.getCaretColor());
				int width = g.getFontMetrics().charWidth('w') - 2;		
				g.fillRect(r.x, r.y, width, r.height);
			} catch (BadLocationException e) {
			}
		}
	}

	/*
	 * Damage must be overridden whenever the paint method is overridden (The
	 * damaged area is the area the caret is painted in. We must consider the
	 * area for the default caret and this caret)
	 * 
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.text.DefaultCaret#damage(java.awt.Rectangle)
	 */
	protected synchronized void damage(Rectangle r) {
		if (r != null) {
			JTextComponent component = getComponent();
			x = r.x;
			y = r.y;
			width = component.getFontMetrics(component.getFont())
					.charWidth('w');
			height = r.height;
			repaint();
		}
	}
}
