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
package acide.gui.fileEditor.fileEditorManager.utils.gui;

import java.awt.*;
import java.beans.*;
import java.util.HashMap;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.text.*;

import acide.log.AcideLog;

/**
 * <p>
 * ACIDE - A Configurable IDE line number component.
 * </p>
 * <p>
 * This class will display line numbers for a related text component. The text
 * component must use the same line height for each line. TextLineNumber
 * supports wrapped lines and will highlight the line number of the current line
 * in the text component.
 * </p>
 * <p>
 * This class was designed to be used as a component added to the row header of
 * a JScrollPane.
 * </p>
 */
public class AcideLineNumberComponent extends JPanel implements CaretListener,
		DocumentListener, PropertyChangeListener {
	/**
	 * 
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
	 * Left constant.
	 */
	public final static float LEFT = 0.0f;
	/**
	 * Center constant.
	 */
	public final static float CENTER = 0.5f;
	/**
	 * Right constant.
	 */
	public final static float RIGHT = 1.0f;
	/**
	 * Border constant.
	 */
	private final static Border OUTER = new MatteBorder(0, 0, 0, 2,
			DEFAULT_FOREGROUND);
	/**
	 * Height constant.
	 */
	private final static int HEIGHT = Integer.MAX_VALUE - 1000000;
	/**
	 * Text component this TextTextLineNumber component is in sync with.
	 */
	private JTextComponent _textComponent;
	/**
	 * Properties that can be changed.
	 */
	private boolean _updateFont;
	/**
	 * Border gap.
	 */
	private int _borderGap;
	/**
	 * Current line foreground. By default is Color.RED
	 */
	private Color _currentLineForeground;
	/**
	 * Digit alignment.
	 */
	private float _digitAlignment;
	/**
	 * Minimum display digits.
	 */
	private int _minimumDisplayDigits;
	/**
	 * Keep history information to reduce the number of times the component
	 * needs to be repainted.
	 */
	private int _lastDigits;
	/**
	 * Last height.
	 */
	private int _lastHeight;
	/**
	 * Last line.
	 */
	private int _lastLine;
	/**
	 * Fonts for the component.
	 */
	private HashMap<String, FontMetrics> _fonts;

	/**
	 * Create a line number component for a text component. This minimum display
	 * width will be based on 3 digits.
	 * 
	 * @param component
	 *            the related text component.
	 */
	public AcideLineNumberComponent(JTextComponent component) {
		this(component, 3);
	}

	/**
	 * Create a line number component for a text component.
	 * 
	 * @param component
	 *            the related text component.
	 * @param minimumDisplayDigits
	 *            the number of digits used to calculate the minimum width of
	 *            the component.
	 */
	public AcideLineNumberComponent(JTextComponent component,
			int minimumDisplayDigits) {

		_textComponent = component;

		// Sets the background color
		setBackground(DEFAULT_BACKGROUND);

		// Sets the foreground color
		setForeground(DEFAULT_FOREGROUND);

		// Sets the components font
		setFont(component.getFont());

		// Sets the border gap as 5
		setBorderGap(5);

		// Sets the current line foreground as red
		setCurrentLineForeground(Color.RED);

		// Sets the digit alignment to right
		setDigitAlignment(RIGHT);

		// Sets the minimum display digits
		setMinimumDisplayDigits(minimumDisplayDigits);

		// Adds a document listener to the component
		component.getDocument().addDocumentListener(this);

		// Adds a caret listener to the component
		component.addCaretListener(this);

		// Adds a property change listener to the component
		component.addPropertyChangeListener("font", this);
	}

	/**
	 * Gets the update font property.
	 * 
	 * @return the update font property.
	 */
	public boolean getUpdateFont() {
		return _updateFont;
	}

	/**
	 * Set the update font property. Indicates whether this Font should be
	 * updated automatically when the Font of the related text component is
	 * changed.
	 * 
	 * @param updateFont
	 *            when true update the Font and repaint the line numbers,
	 *            otherwise just repaint the line numbers.
	 */
	public void setUpdateFont(boolean updateFont) {
		_updateFont = updateFont;
	}

	/**
	 * Gets the border gap.
	 * 
	 * @return the border gap in pixels.
	 */
	public int getBorderGap() {
		return _borderGap;
	}

	/**
	 * The border gap is used in calculating the left and right insets of the
	 * border. Default value is 5.
	 * 
	 * @param borderGap
	 *            the gap in pixels.
	 */
	public void setBorderGap(int borderGap) {

		// Stores the border gap
		_borderGap = borderGap;

		// Creates the inner border
		Border inner = new EmptyBorder(0, borderGap, 0, borderGap);

		// Sets the border
		setBorder(new CompoundBorder(OUTER, inner));

		// Sets the last digits to 0
		_lastDigits = 0;

		// Sets the preferred width
		setPreferredWidth();
	}

	/**
	 * Gets the current line rendering Color.
	 * 
	 * @return the Color used to render the current line number.
	 */
	public Color getCurrentLineForeground() {
		return _currentLineForeground == null ? getForeground()
				: _currentLineForeground;
	}

	/**
	 * The Color used to render the current line digits. Default is Coolor.RED.
	 * 
	 * @param currentLineForeground
	 *            the Color used to render the current line.
	 */
	public void setCurrentLineForeground(Color currentLineForeground) {
		_currentLineForeground = currentLineForeground;
	}

	/**
	 * Gets the digit alignment
	 * 
	 * @return the alignment of the painted digits.
	 */
	public float getDigitAlignment() {
		return _digitAlignment;
	}

	/**
	 * Specify the horizontal alignment of the digits within the component.
	 * Common values would be:
	 * <ul>
	 * <li>TextLineNumber.LEFT
	 * <li>TextLineNumber.CENTER
	 * <li>TextLineNumber.RIGHT (default)
	 * </ul>
	 * 
	 * @param digitAlignment
	 *            the Color used to render the current line.
	 */
	public void setDigitAlignment(float digitAlignment) {
		_digitAlignment = digitAlignment > 1.0f ? 1.0f
				: digitAlignment < 0.0f ? -1.0f : digitAlignment;
	}

	/**
	 * Gets the minimum display digits.
	 * 
	 * @return the minimum display digits.
	 */
	public int getMinimumDisplayDigits() {
		return _minimumDisplayDigits;
	}

	/**
	 * Specify the minimum number of digits used to calculate the preferred
	 * width of the component. Default is 3.
	 * 
	 * @param minimumDisplayDigits
	 *            the number digits used in the preferred width calculation
	 */
	public void setMinimumDisplayDigits(int minimumDisplayDigits) {

		// Stores the minimum display digits
		_minimumDisplayDigits = minimumDisplayDigits;

		// Sets the preferred width
		setPreferredWidth();
	}

	/**
	 * Calculate the width needed to display the maximum line number
	 */
	private void setPreferredWidth() {

		try {

			// Gets the root element
			Element root = _textComponent.getDocument().getDefaultRootElement();

			// Gets the number of lines
			int lines = root.getElementCount();

			// Gets the number of digits
			int digits = Math.max(String.valueOf(lines).length(),
					_minimumDisplayDigits);

			// Update sizes when number of digits in the line number changes

			if (_lastDigits != digits) {
				_lastDigits = digits;
				FontMetrics fontMetrics = getFontMetrics(getFont());
				int width = fontMetrics.charWidth('0') * digits;
				Insets insets = getInsets();
				int preferredWidth = insets.left + insets.right + width;

				Dimension d = getPreferredSize();
				d.setSize(preferredWidth, HEIGHT);
				setPreferredSize(d);
				setSize(d);
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	@Override
	public void paintComponent(Graphics g) {

		// Determine the width of the space available to draw the line number
		try {

			super.paintComponent(g);

			FontMetrics fontMetrics = _textComponent
					.getFontMetrics(_textComponent.getFont());
			Insets insets = getInsets();
			int availableWidth = getSize().width - insets.left - insets.right;

			// Determine the rows to draw within the clipped bounds.

			Rectangle clip = g.getClipBounds();
			int rowStartOffset = _textComponent
					.viewToModel(new Point(0, clip.y));
			int endOffset = _textComponent.viewToModel(new Point(0, clip.y
					+ clip.height));

			while (rowStartOffset <= endOffset) {

				if (isCurrentLine(rowStartOffset))
					g.setColor(getCurrentLineForeground());
				else
					g.setColor(getForeground());

				// Get the line number as a string and then determine the
				// "X" and "Y" offsets for drawing the string.

				String lineNumber = getTextLineNumber(rowStartOffset);
				int stringWidth = fontMetrics.stringWidth(lineNumber);
				int x = getOffsetX(availableWidth, stringWidth) + insets.left;
				int y = getOffsetY(rowStartOffset, fontMetrics);
				g.drawString(lineNumber, x, y);

				// Move to the next row

				rowStartOffset = Utilities.getRowEnd(_textComponent,
						rowStartOffset) + 1;
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}
	}

	/**
	 * We need to know if the caret is currently positioned on the line we are
	 * about to paint so the line number can be highlighted.
	 * 
	 * @param rowStartOffset
	 *            row start offset.
	 */
	private boolean isCurrentLine(int rowStartOffset) {

		try {

			// Gets the caret position
			int caretPosition = _textComponent.getCaretPosition();

			// Gets the root element
			Element root = _textComponent.getDocument().getDefaultRootElement();

			if (root.getElementIndex(rowStartOffset) == root
					.getElementIndex(caretPosition))
				return true;
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}

		return false;
	}

	/**
	 * Get the line number to be drawn. The empty string will be returned when a
	 * line of text has wrapped.
	 * 
	 * @param rowStartOffset
	 *            row start offset.
	 */
	protected String getTextLineNumber(int rowStartOffset) {

		try {

			// Gets the root element
			Element root = _textComponent.getDocument().getDefaultRootElement();

			// Gets the element index
			int index = root.getElementIndex(rowStartOffset);

			// Gets the line
			Element line = root.getElement(index);

			if (line.getStartOffset() == rowStartOffset)
				return String.valueOf(index + 1);
			else
				return "";

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}

		return "";
	}

	/**
	 * Determine the X offset to properly align the line number when drawn.
	 * 
	 * @param availableWidth
	 *            available width.
	 * @param stringWidth
	 *            string width.
	 */
	private int getOffsetX(int availableWidth, int stringWidth) {
		return (int) ((availableWidth - stringWidth) * _digitAlignment);
	}

	/**
	 * Determine the Y offset for the current row.
	 * 
	 * @param rowStartOffset
	 *            row start offset.
	 * @param fontMetrics
	 *            font metrics.
	 */
	private int getOffsetY(int rowStartOffset, FontMetrics fontMetrics)
			throws BadLocationException {

		// Get the bounding rectangle of the row
		try {
			Rectangle r = _textComponent.modelToView(rowStartOffset);
			int lineHeight = fontMetrics.getHeight();
			int y = r.y + r.height;
			int descent = 0;

			// The text needs to be positioned above the bottom of the bounding
			// rectangle based on the descent of the font(s) contained on the
			// row.

			if (r.height == lineHeight) // default font is being used
			{
				descent = fontMetrics.getDescent();
			} else {
				// We need to check all the attributes for font changes
				if (_fonts == null)
					_fonts = new HashMap<String, FontMetrics>();

				Element root = _textComponent.getDocument()
						.getDefaultRootElement();
				int index = root.getElementIndex(rowStartOffset);
				Element line = root.getElement(index);

				for (int i = 0; i < line.getElementCount(); i++) {
					Element child = line.getElement(i);
					AttributeSet as = child.getAttributes();
					String fontFamily = (String) as
							.getAttribute(StyleConstants.FontFamily);
					Integer fontSize = (Integer) as
							.getAttribute(StyleConstants.FontSize);
					String key = fontFamily + fontSize;

					FontMetrics fm = _fonts.get(key);

					if (fm == null) {
						Font font = new Font(fontFamily, Font.PLAIN, fontSize);
						fm = _textComponent.getFontMetrics(font);
						_fonts.put(key, fm);
					}

					descent = Math.max(descent, fm.getDescent());
				}
			}

			return y - descent;

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}

		return -1;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
	 */
	@Override
	public void caretUpdate(CaretEvent e) {

		// Get the line the caret is positioned on

		try {

			// Gets the caret position
			int caretPosition = _textComponent.getCaretPosition();

			// Gets the element root
			Element root = _textComponent.getDocument().getDefaultRootElement();

			// Gets the current line
			int currentLine = root.getElementIndex(caretPosition);

			// Need to repaint so the correct line number can be
			// highlighted

			if (_lastLine != currentLine) {

				// Repaints the component
				repaint();

				// Now the last line is the current line
				_lastLine = currentLine;
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent e) {
		documentChanged();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	public void insertUpdate(DocumentEvent e) {
		documentChanged();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent e) {
		documentChanged();
	}

	/**
	 * A document change may affect the number of displayed lines of text.
	 * Therefore the lines numbers will also change.
	 */
	private void documentChanged() {

		// Preferred size of the component has not been updated at the time
		// the DocumentEvent is fired

		try {
			SwingUtilities.invokeLater(new Runnable() {

				/*
				 * (non-Javadoc)
				 * 
				 * @see java.lang.Runnable#run()
				 */
				@Override
				public void run() {

					int preferredHeight = _textComponent.getPreferredSize().height;

					// Document change has caused a change in the number of
					// lines.
					// Repaint to reflect the new line numbers

					if (_lastHeight != preferredHeight) {
						setPreferredWidth();
						repaint();
						_lastHeight = preferredHeight;
					}
				}
			});

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.getMessage();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.
	 * PropertyChangeEvent)
	 */
	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (event.getNewValue() instanceof Font) {

			if (_updateFont) {

				// Gets the new font
				Font newFont = (Font) event.getNewValue();

				// Sets the font
				setFont(newFont);

				// Sets the last digits as 0
				_lastDigits = 0;

				// Sets the preferred size
				setPreferredWidth();
			} else {

				// Repaints the component
				repaint();
			}
		}
	}
}
