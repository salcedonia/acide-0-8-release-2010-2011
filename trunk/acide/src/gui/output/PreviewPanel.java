package gui.output;

import javax.swing.JPanel;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

/**
 * Font panel for the preview of the fonts in the output editor GUI of the
 * application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class PreviewPanel extends JPanel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Font name.
	 */
	private String _fontName;
	/**
	 * Font style.
	 */
	private int _fontStyle;
	/**
	 * Font size.
	 */
	private int _fontSize;
	/**
	 * Foreground color.
	 */
	private Color _foregroundColor;
	/**
	 * Background color.
	 */
	private Color _backgroundColor;

	/**
	 * Constructor of the class.
	 * 
	 * @param font Font to set.
	 * @param style Font style.
	 * @param size Font size.
	 * @param background Background color.
	 * @param foreground Foreground color.
 	 */
	public PreviewPanel(String font, int style, int size, Color foreground, Color background) {
		
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
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	public void paintComponent(Graphics g) {
		
		// PAINT BACKGROUND
		super.paintComponent(g);
	
		// FOR THE ANTIALIASING
		Graphics2D g2 = (Graphics2D) g; 

		String text = "Font(\"" + _fontName + "\", "
				+ fontStyleCodeToFontStyleString(_fontStyle) + ", " + _fontSize
				+ ");";
		Font f = new Font(_fontName, _fontStyle, _fontSize);
		g2.setFont(f);
							
		// FIND THE SIZE OF THE TEXT SO IT CAN CENTER IT
		FontMetrics fm = g2.getFontMetrics(f);
		Rectangle2D rect = fm.getStringBounds(text, g2);
		int textHeight = (int) (rect.getHeight());
		int textWidth = (int) (rect.getWidth());

		// CENTER TEXT
		int x = (this.getWidth() - textWidth) / 2;
		int y = (this.getHeight() - textHeight) / 2 + fm.getAscent();

		g2.drawString(text, x, y);
	}

	/**
	 * Set a new value to the font name.
	 * 
	 * @param fontName New value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
		repaint();
	}

	/**
	 * Returns the font name.
	 * 
	 * @return The font name.
	 */
	public String getFontName(){		
		return _fontName;
	}
	
	/**
	 * Set a new value to the font size.
	 * 
	 * @param fontSize New value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
		repaint();
	}

	/**
	 * Returns the font size.
	 * 
	 * @return The font size.
	 */
	public int getFontSize(){
		return _fontSize;
	}
	
	/**
	 * Returns the font style.
	 * 
	 * @return The font style.
	 */
	public int getFontStyle(){
		return _fontStyle;
	}
	
	/**
	 * Set a new value to the font style.
	 * 
	 * @param fontStyle New value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
		repaint();
	}

	/**
	 * Set a new value to the foreground color.
	 * 
	 * @param foregroundColor New value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;	
		setForeground(foregroundColor);
		repaint();
	}
	
	/**
	 * Set a new value to the background color.
	 * 
	 * @param backgroundColor New value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;	
		setBackground(backgroundColor);
		repaint();
	}
	
	/**
	 * Utility method for converting font codes to name.
	 * 
	 * @param styleCode Style code to convert.
	 *  
	 * @return The style code converted into a string.
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
		
		case Font.BOLD+Font.ITALIC:
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
