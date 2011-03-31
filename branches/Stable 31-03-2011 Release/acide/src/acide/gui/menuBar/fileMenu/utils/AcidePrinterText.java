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
package acide.gui.menuBar.fileMenu.utils;

import java.awt.Color;
import java.awt.Container;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.font.LineMetrics;
import java.awt.geom.Rectangle2D;
import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.Printable;
import java.util.ArrayList;
import java.util.Date;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * ACIDE - A Configurable IDE printer text.
 * 
 * @version 0.8
 * @see Printable
 */
public class AcidePrinterText implements Printable {

	/**
	 * ACIDE - A Configurable IDE printer text root view to be printed.
	 */
	private View _root;
	/**
	 * ACIDE - A Configurable IDE printer text paper plus page orientation.
	 */
	private PageFormat _pageFormat;
	/**
	 * ACIDE - A Configurable IDE printer text number of pages in the document.
	 */
	private int _numberOfPages;
	/**
	 * ACIDE - A Configurable IDE printer text coordinates of upper-left of print area.
	 */
	private double _printX, _printY;
	/**
	 * ACIDE - A Configurable IDE printer text width of the printable area.
	 */
	private double _printWidth;
	/**
	 * ACIDE - A Configurable IDE printer text height of the printable area.
	 */
	private double _printHeight;
	/**
	 * ACIDE - A Configurable IDE printer text rectangle in which the document is painted.
	 */
	private Rectangle _drawRect;
	/**
	 * How lenient are we with the bottom margin in widow and orphan prevention?
	 */
	static final double MARGIN_ADJUST = .97;
	/**
	 * ACIDE - A Configurable IDE printer text font we use for printing page numbers.
	 */
	static final Font _headerFont = new Font("Arial", Font.BOLD, 12);
	/**
	 * ACIDE - A Configurable IDE printer text maximum number of lines.
	 */
	final int MAX_LINES = 1;
	/**
	 * Indicates if shows the page in the printed page or not.
	 */
	private boolean _showPage;
	/**
	 * Indicates if shows the date in the printed page or not.
	 */
	private boolean _showDate;
	/**
	 * This is the starting offset of the page we're currently working on.
	 */
	private double _pageStart = 0;

	/**
	 * This constructor allows the contents of any JTextComponent to be printed,
	 * using any specified PageFormat object.
	 */
	public AcidePrinterText(JTextComponent textComponent, PageFormat format,
			boolean pag, boolean date) {

		// Remember the page format, and ask it for the printable area
		_pageFormat = format;
		_printX = format.getImageableX();
		_printY = format.getImageableY();
		_printWidth = format.getImageableWidth();
		_printHeight = format.getImageableHeight();
		_showPage = pag;
		_showDate = date;
		// Get the document and its root Element from the text component
		Document document = textComponent.getDocument();
		Element rootElement = document.getDefaultRootElement();
		// Get the EditorKit and its ViewFactory from the text component
		EditorKit editorKit = textComponent.getUI().getEditorKit(textComponent);
		ViewFactory viewFactory = editorKit.getViewFactory();

		// Use the ViewFactory to create a root View object for the document
		// This is the object we'll print.
		_root = viewFactory.create(rootElement);

		// The Swing text architecture requires us to call setParent() on
		// our root View before we use it for anything. In order to do this,
		// we need a View object that can serve as the parent. We use a
		// custom implementation defined below.
		_root.setParent(new ParentView(_root, viewFactory, textComponent));

		// Tell the view how wide the page is; it has to format itself
		// to fit within this width. The height doesn't really matter here
		_root.setSize((float) _printWidth, (float) _printHeight);

		// Now that the view has formatted itself for the specified width,
		// Ask it how tall it is.
		double documentHeight = _root.getPreferredSpan(View.Y_AXIS);

		// Set up the rectangle that tells the view where to draw itself
		// We'll use it in other methods of this class.
		_drawRect = new Rectangle((int) _printX, (int) _printY,
				(int) _printWidth, (int) documentHeight);

		// Now if the document is taller than one page, we have to
		// figure out where the page breaks are.
		if (documentHeight > _printHeight)
			paginate(_root, _drawRect);

		// Once we've broken it into pages, figure out how man pages.
		_numberOfPages = pageLengths.size() + 1;
	}

	/**
	 * This method loops through the children of the specified view, recursing
	 * as necessary, and inserts pages breaks when needed. It makes a
	 * rudimentary attempt to avoid "widows" and "orphans".
	 */
	protected void paginate(View v, Rectangle2D allocation) {
		// Figure out how tall this view is, and tell it to allocate
		// that space among its children
		double myheight = v.getPreferredSpan(View.Y_AXIS);
		v.setSize((float) _printWidth, (float) myheight);

		// Now loop through each of the children
		int numkids = v.getViewCount();
		for (int i = 0; i < numkids; i++) {
			View kid = v.getView(i); // this is the child we're working with
			// Figure out its size and location
			Shape kidshape = v.getChildAllocation(i, allocation);
			if (kidshape == null)
				continue;
			Rectangle2D kidbox = kidshape.getBounds2D();

			// This is the Y coordinate of the bottom of the child
			double kidpos = kidbox.getY() + kidbox.getHeight() - _pageStart;

			// If this is the first child of a group, then we want to ensure
			// that it doesn't get left by itself at the bottom of a page.
			// I.e. we want to prevent "widows"
			if ((numkids > 1) && (i == 0)) {
				// If it is not near the end of the page, then just move
				// on to the next child
				if (kidpos < _printY + _printHeight * MARGIN_ADJUST)
					continue;

				// Otherwise, the child is near the bottom of the page, so
				// break the page before this child and place this child on
				// the new page.
				breakPage(kidbox.getY());
				continue;
			}

			// If this is the last child of a group, we don't want it to
			// appear by itself at the top of a new page, so allow it to
			// squeeze past the bottom margin if necessary. This helps to
			// prevent "orphans"
			if ((numkids > 1) && (i == numkids - 1)) {
				// If it fits normally, just move on to the next one
				if (kidpos < _printY + _printHeight)
					continue;

				// Otherwise, if it fits with extra space, then break the
				// at the end of the group
				if (kidpos < _printY + _printHeight / MARGIN_ADJUST) {
					breakPage(allocation.getY() + allocation.getHeight());
					continue;
				}
			}

			// If the child is not the first or last of a group, then we use
			// the bottom margin strictly. If the child fits on the page,
			// then move on to the next child.
			if (kidpos < _printY + _printHeight)
				continue;

			// If we get here, the child doesn't fit on this page. If it has
			// no children, then break the page before this child and continue.
			if (kid.getViewCount() == 0) {
				breakPage(kidbox.getY());
				continue;
			}

			// If we get here, then the child did not fit on the page, but it
			// has kids of its own, so recurse to see if any of those kids
			// will fit on the page.
			paginate(kid, kidbox);
		}
	}

	// For a document of n pages, this list stores the lengths of pages
	// 0 through n-2. The last page is assumed to have a full length
	ArrayList<Double> pageLengths = new ArrayList<Double>();

	// For a document of n pages, this list stores the starting offset of
	// pages 1 through n-1. The offset of page 0 is always 0
	ArrayList<Double> pageOffsets = new ArrayList<Double>();

	/**
	 * Break a page at the specified Y coordinate. Store the necessary
	 * information into the pageLengths and pageOffsets lists
	 */
	void breakPage(double y) {
		double pageLength = y - _pageStart - _printY;
		_pageStart = y - _printY;
		pageLengths.add(new Double(pageLength));
		pageOffsets.add(new Double(_pageStart));
	}

	/**
	 * Return the number of pages. This is a Pageable method.
	 * 
	 * @return the number of pages.
	 */
	public int getNumberOfPages() {
		return _numberOfPages;
	}

	/**
	 * Return the PageFormat object for the specified page. This implementation
	 * uses the computed length of the page in the returned PageFormat object.
	 * The PrinterJob will use this as a clipping region, which will prevent
	 * extraneous parts of the document from being drawn in the top and bottom
	 * margins.
	 */
	public PageFormat getPageFormat(int pagenum) {
		// On the last page, just return the user-specified page format
		if (pagenum == _numberOfPages - 1)
			return _pageFormat;

		// Otherwise, look up the height of this page and return an
		// appropriate PageFormat.
		double pageLength = ((Double) pageLengths.get(pagenum)).doubleValue();
		PageFormat f = (PageFormat) _pageFormat.clone();
		Paper p = f.getPaper();
		if (f.getOrientation() == PageFormat.PORTRAIT)
			p.setImageableArea(_printX, _printY, _printWidth, pageLength);
		else
			p.setImageableArea(_printY, _printX, pageLength, _printWidth);
		f.setPaper(p);
		return f;
	}

	/**
	 * This Printable method returns the Printable object for the specified
	 * page. Since this class implements both Pageable and Printable, it just
	 * returns this.
	 */
	public Printable getPrintable(int pagenum) {
		return this;
	}

	/**
	 * This is the basic Printable method that prints a specified page.
	 */
	@SuppressWarnings("deprecation")
	public int print(Graphics g, PageFormat format, int pageIndex) {
		// Return an error code on attempts to print past the end of the doc
		if (pageIndex >= _numberOfPages)
			return NO_SUCH_PAGE;

		// Cast the Graphics object so we can use Java2D operations
		Graphics2D g2 = (Graphics2D) g;

		// Display a page number centered in the area of the top margin.
		// Set a new clipping region so we can draw into the top margin
		// But remember the original clipping region so we can restore it
		Shape originalClip = g.getClip();
		g.setClip(new Rectangle(0, 0, (int) _printWidth, (int) _printY));
		// Compute the header to display, measure it, then display it
		String numString = "";
		String res = "";
		String dat = "";
		if (_showPage == true) {
			numString = "- " + (pageIndex + 1) + " -";
		}
		if (_showDate == true) {
			Date d = new Date();
			dat = d.toLocaleString();
			dat = dat.substring(0, dat.indexOf(" "));

		}
		res = numString + "      " + dat;
		Rectangle2D numBounds = // Get the width and height of the string
		_headerFont.getStringBounds(res, g2.getFontRenderContext());
		LineMetrics metrics = // Get the ascent and descent of the font
		_headerFont.getLineMetrics(res, g2.getFontRenderContext());
		g2.setFont(_headerFont); // Set the font
		g2.setColor(Color.black); // Print with black ink

		g2.drawString(
				res, // Display the string
				(int) (_printX + (_printWidth - numBounds.getWidth()) / 2),
				(int) ((_printY - numBounds.getHeight()) / 2 + metrics
						.getAscent()));

		g2.setClip(originalClip); // Restore the clipping region

		// Figure out the staring position of the page within the document
		double pageStart = 0.0;
		if (pageIndex > 0)
			pageStart = ((Double) pageOffsets.get(pageIndex - 1)).doubleValue();

		// Scroll so that the appropriate part of the document is lined up
		// with the upper-left corner of the page
		g2.translate(0.0, -pageStart);

		// Now paint the entire document. The PrinterJob will have
		// established a clipping region, so that only the desired portion
		// of the document will actually be drawn on this sheet of paper.
		_root.paint(g2, _drawRect);
		// Finally return a success code
		return PAGE_EXISTS;
	}

	/**
	 * This inner class is a concrete implementation of View, with a couple of
	 * key method implementations. An instance of this class is used as the
	 * parent of the root View object we want to print
	 *  
	 * @version 0.8
	 * @see View
	 */
	static class ParentView extends View {

		/**
		 * The ViewFactory for the hierarchy of views.
		 */
		private ViewFactory _viewFactory;
		/**
		 * The Container for the hierarchy of views.
		 */
		private Container _container;

		/**
		 * Class constructor.
		 * 
		 * @param v
		 *            view.
		 * @param viewFactory
		 *            view factory of the window.
		 * @param container
		 *            container.
		 */
		public ParentView(View v, ViewFactory viewFactory, Container container) {
			super(v.getElement());
			_viewFactory = viewFactory;
			_container = container;
		}

		/**
		 * These methods return key pieces of information required by the View
		 * hierarchy.
		 * 
		 * @return key pieces of information required by the View hierarchy.
		 */
		public ViewFactory getViewFactory() {
			return _viewFactory;
		}

		/**
		 * Returns the container.
		 * 
		 * @return the container.
		 */
		public Container getContainer() {
			return _container;
		}

		/**
		 * These methods are abstract in View, so we've got to provide dummy
		 * implementations of them here, even though they're never used.
		 * 
		 * @param g
		 *            graphics.
		 * @param allocation
		 *            shape.
		 */
		public void paint(Graphics g, Shape allocation) {
		}

		/**
		 * Returns the preferred span.
		 * 
		 * @param axis
		 *            axis of the span.
		 * @return the preferred span.
		 */
		public float getPreferredSpan(int axis) {
			return 0.0f;
		}

		/**
		 * Returns 0.
		 * 
		 * @param x
		 *            X coordinate.
		 * @param y
		 *            Y coordinate.
		 * @param a
		 *            Shape.
		 * @param bias
		 *            position bias array.
		 * 
		 * @return 0
		 */
		public int viewToModel(float x, float y, Shape a, Position.Bias[] bias) {
			return 0;
		}

		/**
		 * Returns the shape.
		 * 
		 * @param a
		 *            shape.
		 * @param b
		 *            bias.
		 * @return the shape.
		 */
		public Shape modelToView(int pos, Shape a, Position.Bias b)
				throws BadLocationException {
			return a;
		}
	}

	/**
	 * Sets a new value to showPage flag
	 * 
	 * @param showPage
	 *            new value to set
	 */
	public void setShowPage(boolean showPage) {
		_showPage = showPage;
	}

	/**
	 * Sets a new value to showDate flag
	 * 
	 * @param showDate
	 *            new value to set
	 */
	public void setDate(boolean showDate) {
		_showDate = showDate;
	}
}
