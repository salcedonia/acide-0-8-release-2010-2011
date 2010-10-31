package gui.editor.utils;


import java.awt.*;
import java.util.ResourceBundle;

import javax.swing.*;

import language.Language;

import operations.log.Log;

import org.apache.log4j.Logger;

/**
 * Handle the creation and destruction of the different tabs of editors of the
 * application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class LineNumber extends JComponent{
	
	/**
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Color for the default background.
	 */
	private final static Color DEFAULT_BACKGROUND = new Color(204, 204, 255);
	/**
	 * Color for the default foreground.
	 */
	private final static Color DEFAULT_FOREGROUND = Color.black;
	/**
	 * Default font for the LineNumber.
	 */
	private final static Font DEFAULT_FONT = new Font("monospaced", Font.PLAIN, 12);
	/**
	 * Height for the line.
	 */
	private final static int HEIGHT = Integer.MAX_VALUE - 1000000;
	/**
	 * Margin for the line.
	 */
	private final static int MARGIN = 5;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	/**
	 * Font metrics for the line number.
	 */
	private FontMetrics _fontMetrics;
	/**
	 * Height for the line.
	 */
	private int _lineHeight;
	/**
	 * Current digits.
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
	 * Constructor of the class
	 * 
	 * @param component Component to add.
	 */
	public LineNumber(JComponent component){
		
		ResourceBundle labels = Language.getInstance().getLabels();
		try {
			if (component == null){
				
				setFont( DEFAULT_FONT );
				_component = this;
			}
			else{
				setFont( component.getFont() );
				_component = component;
			}

			setBackground( DEFAULT_BACKGROUND );
			setForeground( DEFAULT_FOREGROUND );
			setPreferredWidth( 99 );
		} catch (RuntimeException e) {
			_logger.info(labels.getString("s320"));
			e.printStackTrace();
		}
	}

	/**
	 * Set the preferred width of the panel. Update sizes when number of digits 
	 * in the line number changes.
	 * 
	 * @param lines Number of lines.
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
	 * Set the font of the panel.
	 * 
	 * @param font New value to set.
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
	 * @param The line height.
	 */
	public int getLineHeight(){
		
		if (_lineHeight == 0)
			return _componentFontHeight;
		else
			return _lineHeight;
	}

	/**
	 * Set the new value to the line height.
	 * 
	 * @param lineHeight New value to set.
	 */
	public void setLineHeight(int lineHeight){
		
		if (lineHeight > 0)
			_lineHeight = lineHeight;
	}

	/**
	 * Returns the start offset.
	 * 
	 * @return The start offset.
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
