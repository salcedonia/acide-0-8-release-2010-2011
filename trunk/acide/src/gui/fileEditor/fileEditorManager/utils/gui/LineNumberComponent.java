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
package gui.fileEditor.fileEditorManager.utils.gui;

import java.awt.*;
import java.util.ResourceBundle;

import javax.swing.*;

import language.AcideLanguageManager;

import operations.log.AcideLog;

/**
 * Handles the creation and destruction of the different tabs of editors of
 * ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 * @see JTabbedPane
 */
public class LineNumberComponent extends JComponent{
	
	/**
	 * Line number component class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Default background color.
	 */
	private final static Color DEFAULT_BACKGROUND = new Color(215, 215, 255);
	/**
	 * Default foreground color.
	 */
	private final static Color DEFAULT_FOREGROUND = new Color(125, 125, 155);
	/**
	 * Line number default font.
	 */
	private final static Font DEFAULT_FONT = new Font("monospaced", Font.BOLD, 12);
	/**
	 * Line number height.
	 */
	private final static int HEIGHT = Integer.MAX_VALUE - 1000000;
	/**
	 * Line number margin.
	 */
	private final static int MARGIN = 5;
	/**
	 * Line number font metrics.
	 */
	private FontMetrics _fontMetrics;
	/**
	 * Line height.
	 */
	private int _lineHeight;
	/**
	 * Line number current digits.
	 */
	private int _currentDigits;
	/**
	 * Component of the panel.
	 */
	private JComponent _component;
	/**
	 * Component font height.
	 */
	private int _componentFontHeight;
	/**
	 * Component font ascent.
	 */
	private int _componentFontAscent;

	/**
	 * Creates a new line component from a component given as 
	 * a parameter.
	 * 
	 * @param component new component.
	 */
	public LineNumberComponent(JComponent component){
		
		ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();
		try {
			
			if (component == null){
				
				setFont( DEFAULT_FONT );
				_component = this;
			}
			else{
				setFont( DEFAULT_FONT );
				//setFont(component.getFont());
				_component = component;
			}

			setBackground(DEFAULT_BACKGROUND);
			setForeground(DEFAULT_FOREGROUND);
			setPreferredWidth(99);
			setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, DEFAULT_FOREGROUND));
		} catch (RuntimeException exception) {
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s320"));
			exception.printStackTrace();
		}
	}

	/**
	 * Sets the preferred width of the panel. Updates sizes when number of digits 
	 * in the line number changes.
	 * 
	 * @param lines number of lines.
	 */
	public void setPreferredWidth(int lines){
		
		int digits = String.valueOf(lines).length();

		if (digits != _currentDigits && digits > 1){
			
			_currentDigits = digits;
			int width = _fontMetrics.charWidth( '0' ) * digits;
			Dimension d = getPreferredSize();
			d.setSize(2 * MARGIN + width, HEIGHT);
			setPreferredSize( d );
			setSize( d );
		}
	}

	/**
	 * Sets the font of the panel.
	 * 
	 * @param font new value to set.
	 */
	public void setFont(Font font){
		
		super.setFont(font);
		_fontMetrics = getFontMetrics( getFont() );
		_componentFontHeight = _fontMetrics.getHeight();
		_componentFontAscent = _fontMetrics.getAscent();
	}

	/**
	 * Returns the line height.
	 * 
	 * @param the line height.
	 */
	public int getLineHeight(){
		
		if (_lineHeight == 0)
			return _componentFontHeight;
		else
			return _lineHeight;
	}

	/**
	 * Sets the new value to the line height
	 * 
	 * @param lineHeight new value to set
	 */
	public void setLineHeight(int lineHeight){
		
		if (lineHeight > 0)
			_lineHeight = lineHeight;
	}

	/**
	 * Returns the start offset
	 * 
	 * @return the start offset
	 */
	public int getStartOffset(){
		return _component.getInsets().top + _componentFontAscent;
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	public void paintComponent(Graphics g){
		
		int lineHeight = getLineHeight();
		int startOffset = getStartOffset();
		Rectangle drawHere = g.getClipBounds();

		// PAINT THE BACKGROUND
		g.setColor( getBackground() );
		g.fillRect(drawHere.x, drawHere.y, drawHere.width, drawHere.height);

		// DETERMINE THE NUMBER OF LINES TO DRAW IN THE FOREGROUND
		g.setColor( getForeground() );
		int startLineNumber = (drawHere.y / lineHeight) + 1;
		int endLineNumber = startLineNumber + (drawHere.height / lineHeight);

		int start = (drawHere.y / lineHeight) * lineHeight + startOffset;

		for (int i = startLineNumber; i <= endLineNumber; i++){
			
			String lineNumber = String.valueOf(i);
			int stringWidth = _fontMetrics.stringWidth( lineNumber );
			int rowWidth = getSize().width;
			g.drawString(lineNumber, rowWidth - stringWidth - MARGIN, start);
			start += lineHeight;
		}

		int rows = _component.getSize().height / _componentFontHeight;
		setPreferredWidth( rows );
	}
}
