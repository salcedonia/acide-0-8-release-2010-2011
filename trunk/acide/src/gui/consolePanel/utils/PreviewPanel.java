package gui.consolePanel.utils;

import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

/************************************************************************																
 * Preview panel which displays the selected font name with the selected
 * style in the ACIDE - A Configurable IDE console display options window.
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see JFrame																													
 ***********************************************************************/
public class PreviewPanel extends JPanel {

	/**
	 * Preview panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Preview panel font name.
	 */
	private String _fontName;
	/**
	 * Preview panel font style.
	 */
	private int _fontStyle;
	/**
	 * Preview panel font size.
	 */
	private int _fontSize;
	/**
	 * Preview panel foreground color.
	 */
	private Color _foregroundColor;
	/**
	 * Preview panel background color.
	 */
	private Color _backgroundColor;

	/**
	 * Creates a new preview panel.
	 * 
	 * @param font font to set.
	 * @param style font style.
	 * @param size font size.
	 * @param background background color.
	 * @param foreground foreground color.
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
	@Override
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
	 * Sets a new value to the preview panel font name.
	 * 
	 * @param fontName new value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
		repaint();
	}

	/**
	 * Returns the preview panelfont name.
	 * 
	 * @return the preview panelfont name.
	 */
	public String getFontName(){		
		return _fontName;
	}
	
	/**
	 * Sets a new value to the preview panel font size.
	 * 
	 * @param fontSize new value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
		repaint();
	}

	/**
	 * Returns the preview panel font size.
	 * 
	 * @return the preview panel font size.
	 */
	public int getFontSize(){
		return _fontSize;
	}
	
	/**
	 * Returns the preview panel font style.
	 * 
	 * @return the preview panel font style.
	 */
	public int getFontStyle(){
		return _fontStyle;
	}
	
	/**
	 * Sets a new value to the preview panel font style.
	 * 
	 * @param fontStyle new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
		repaint();
	}

	/**
	 * Sets a new value to the preview panel foreground color.
	 * 
	 * @param foregroundColor new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;	
		setForeground(foregroundColor);
		repaint();
	}
	
	/**
	 * Sets a new value to the preview panel background color.
	 * 
	 * @param backgroundColor new value to set.
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
