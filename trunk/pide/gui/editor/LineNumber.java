package gui.editor;

import idioma.Idioma;

import java.awt.*;
import java.util.ResourceBundle;

import javax.swing.*;

import operaciones.log.Log;

import org.apache.log4j.Logger;

/**
 * Crea los numeros de linea asociados a cada editor de texto
 *
 */
public class LineNumber extends JComponent
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final static Color DEFAULT_BACKGROUND = new Color(204, 204, 255);
	private final static Color DEFAULT_FOREGROUND = Color.black;
	private final static Font  DEFAULT_FONT = new Font("monospaced", Font.PLAIN, 12);

	//  altura del LineNumber
	private final static int HEIGHT = Integer.MAX_VALUE - 1000000;

	//  margenes del LineNumber
	private final static int MARGIN = 5;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	
	private FontMetrics fontMetrics;
	private int lineHeight;
	private int currentDigits;
	private JComponent component;
	private int componentFontHeight;
	private int componentFontAscent;

	/**
	 *	Constructora de LineNumber
	 */
	public LineNumber(JComponent component)
	{
		ResourceBundle labels = Idioma.getInstance().getLabels();
		try {
			if (component == null)
			{
				setFont( DEFAULT_FONT );
				this.component = this;
			}
			else
			{
				setFont( component.getFont() );
				this.component = component;
			}

			setBackground( DEFAULT_BACKGROUND );
			setForeground( DEFAULT_FOREGROUND );
			setPreferredWidth( 99 );
		} catch (RuntimeException e) {
			logger.info(labels.getString("s320"));
			e.printStackTrace();
		}
	}

	/**
	 *  Calcula al ancho necesario para mostrar el maximo numero de linea
	 */
	public void setPreferredWidth(int lines)
	{
		int digits = String.valueOf(lines).length();

		//  Update sizes when number of digits in the line number changes

		if (digits != currentDigits && digits > 1)
		{
			currentDigits = digits;
			int width = fontMetrics.charWidth( '0' ) * digits;
			Dimension d = getPreferredSize();
			d.setSize(2 * MARGIN + width, HEIGHT);
			setPreferredSize( d );
			setSize( d );
		}
	}

	/**
	 *  Resetea las variables dependientes del tipo de fuente utilizado
	 */
	public void setFont(Font font)
	{
		super.setFont(font);
		fontMetrics = getFontMetrics( getFont() );
		componentFontHeight = fontMetrics.getHeight();
		componentFontAscent = fontMetrics.getAscent();
	}

	/**
	 * Calcula el alto de la fuente utilizada
	 */
	public int getLineHeight()
	{
		if (lineHeight == 0)
			return componentFontHeight;
		else
			return lineHeight;
	}

	/**
	 *  Instancia el alto en funcion de la fuente
	 */
	public void setLineHeight(int lineHeight)
	{
		if (lineHeight > 0)
			this.lineHeight = lineHeight;
	}

	public int getStartOffset()
	{
		return component.getInsets().top + componentFontAscent;
	}

	public void paintComponent(Graphics g)
	{
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

		for (int i = startLineNumber; i <= endLineNumber; i++)
		{
			String lineNumber = String.valueOf(i);
			int stringWidth = fontMetrics.stringWidth( lineNumber );
			int rowWidth = getSize().width;
			g.drawString(lineNumber, rowWidth - stringWidth - MARGIN, start);
			start += lineHeight;
		}

		int rows = component.getSize().height / componentFontHeight;
		setPreferredWidth( rows );
	}
}
