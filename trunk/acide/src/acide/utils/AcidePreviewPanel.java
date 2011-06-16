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
package acide.utils;

import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

/**
 * <p>
 * ACIDE - A Configurable IDE preview panel.
 * </p>
 * <p>
 * Preview panel which displays the selected font name with the selected style
 * in the ACIDE - A Configurable IDE console display options window.
 * </p>
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcidePreviewPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE preview panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE preview panel font name.
	 */
	private String _fontName;
	/**
	 * ACIDE - A Configurable IDE preview panel font style.
	 */
	private int _fontStyle;
	/**
	 * ACIDE - A Configurable IDE preview panel font size.
	 */
	private int _fontSize;
	/**
	 * ACIDE - A Configurable IDE preview panel foreground color.
	 */
	private Color _foregroundColor;
	/**
	 * ACIDE - A Configurable IDE preview panel background color.
	 */
	private Color _backgroundColor;

	/**
	 * Creates a new ACIDE - A Configurable IDE preview panel.
	 * 
	 * @param font
	 *            font to set.
	 * @param style
	 *            font style.
	 * @param size
	 *            font size.
	 * @param background
	 *            background color.
	 * @param foreground
	 *            foreground color.
	 */
	public AcidePreviewPanel(String font, int style, int size,
			Color foreground, Color background) {

		setPreferredSize(new Dimension(400, 100));
		_backgroundColor = background;
		_foregroundColor = foreground;
		setBackground(_backgroundColor);
		setForeground(_foregroundColor);
		_fontName = font;
		_fontStyle = style;
		_fontSize = size;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	@Override
	public void paintComponent(Graphics g) {

		// Paints the background color
		super.paintComponent(g);

		// For the antialiasing
		Graphics2D g2 = (Graphics2D) g;

		String text = "Font(\"" + _fontName + "\", "
				+ fontStyleCodeToFontStyleString(_fontStyle) + ", " + _fontSize
				+ ");";
		Font f = new Font(_fontName, _fontStyle, _fontSize);
		g2.setFont(f);

		// Finds the size of the text so it can center it
		FontMetrics fm = g2.getFontMetrics(f);
		Rectangle2D rect = fm.getStringBounds(text, g2);
		int textHeight = (int) (rect.getHeight());
		int textWidth = (int) (rect.getWidth());

		// Centers the text
		int x = (this.getWidth() - textWidth) / 2;
		int y = (this.getHeight() - textHeight) / 2 + fm.getAscent();

		g2.drawString(text, x, y);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE preview panel font
	 * name.
	 * 
	 * @param fontName
	 *            new value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
		repaint();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE preview panel font name.
	 * 
	 * @return the ACIDE - A Configurable IDE preview panel font name.
	 */
	public String getFontName() {
		return _fontName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE preview panel font
	 * size.
	 * 
	 * @param fontSize
	 *            new value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
		repaint();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE preview panel font size.
	 * 
	 * @return the ACIDE - A Configurable IDE preview panel font size.
	 */
	public int getFontSize() {
		return _fontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE preview panel font style.
	 * 
	 * @return the ACIDE - A Configurable IDE preview panel font style.
	 */
	public int getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE preview panel font
	 * style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
		repaint();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE preview panel
	 * foreground color.
	 * 
	 * @param foregroundColor
	 *            new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;
		setForeground(foregroundColor);
		repaint();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE preview panel
	 * background color.
	 * 
	 * @param backgroundColor
	 *            new value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
		setBackground(backgroundColor);
		repaint();
	}

	/**
	 * Utility method for converting font codes to name.
	 * 
	 * @param styleCode
	 *            Style code to convert.
	 * @return the style code converted into a string.
	 */
	public static String fontStyleCodeToFontStyleString(int styleCode) {

		String styleName;

		switch (styleCode) {

		case Font.PLAIN:
			styleName = "Font.PLAIN";
			break;

		case Font.ITALIC:
			styleName = "Font.ITALIC";
			break;

		case Font.BOLD:
			styleName = "Font.BOLD";
			break;

		case Font.BOLD + Font.ITALIC:
			styleName = "Font.BOLD+Font.ITALIC";
			break;

		default:
			throw new IllegalArgumentException(
					"fontStyleCodeToFontStyleString: Unknown font code: "
							+ styleCode);
		}
		return styleName;
	}
}
