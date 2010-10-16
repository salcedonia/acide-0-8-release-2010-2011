package gui.editor;


import java.awt.*;
import java.util.ResourceBundle;

import javax.swing.*;

import language.Language;

import operations.log.Log;

import org.apache.log4j.Logger;

/**
 * 
 */
public class LineNumber extends JComponent{
	
	/**
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private final static Color DEFAULT_BACKGROUND = new Color(204, 204, 255);
	/**
	 * 
	 */
	private final static Color DEFAULT_FOREGROUND = Color.black;
	/**
	 * 
	 */
	private final static Font  DEFAULT_FONT = new Font("monospaced", Font.PLAIN, 12);
	/**
	 * 
	 */
	private final static int HEIGHT = Integer.MAX_VALUE - 1000000;
	/**
	 * 
	 */
	private final static int MARGIN = 5;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private FontMetrics _fontMetrics;
	/**
	 * 
	 */
	private int _lineHeight;
	/**
	 * 
	 */
	private int _currentDigits;
	/**
	 * 
	 */
	private JComponent _component;
	/**
	 * 
	 */
	private int _componentFontHeight;
	/**
	 * 
	 */
	private int _componentFontAscent;

	/**
	 * Constructor of the class
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
	 * 
	 */
	public void setPreferredWidth(int lines){
		
		int digits = String.valueOf(lines).length();

		//  Update sizes when number of digits in the line number changes
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
	 * 
	 */
	public void setFont(Font font){
		
		super.setFont(font);
		_fontMetrics = getFontMetrics( getFont() );
		_componentFontHeight = _fontMetrics.getHeight();
		_componentFontAscent = _fontMetrics.getAscent();
	}

	/**
	 * 
	 */
	public int getLineHeight(){
		
		if (_lineHeight == 0)
			return _componentFontHeight;
		else
			return _lineHeight;
	}

	/**
	 * 
	 */
	public void setLineHeight(int lineHeight){
		
		if (lineHeight > 0)
			_lineHeight = lineHeight;
	}

	/**
	 * 
	 * @return
	 */
	public int getStartOffset(){
		return _component.getInsets().top + _componentFontAscent;
	}

	/**
	 * 
	 */
	public void paintComponent(Graphics g){
		
		int lineHeight = getLineHeight();
		int startOffset = getStartOffset();
		Rectangle drawHere = g.getClipBounds();

		// Paint the background

		g.setColor( getBackground() );
		g.fillRect(drawHere.x, drawHere.y, drawHere.width, drawHere.height);

		//  Determine the number of lines to draw in the foreground.

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
