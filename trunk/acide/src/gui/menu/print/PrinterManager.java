package gui.menu.print;

import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.text.AttributedString;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.swing.text.JTextComponent;

/**
 * Printer Manager of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class PrinterManager {

	/**
	 * Printer job.
	 */
	private PrinterJob _printerJob;
	/**
	 * Page format.
	 */
	private PageFormat _pageFormat;
	/**
	 * Printed text component.
	 */
	private JTextComponent _printedTextComponent;
	/**
	 * Style of the document.
	 */
	private AttributedString _style;
	/**
	 * Show page flag.
	 */
	private boolean _showPage;
	/**
	 * Show date flag.
	 */
	private boolean _showDate;
	/**
	 * Printer text.
	 */
	private PrinterText _printerText;

	/**
	 * Constructor of the class.
	 * 
	 * @param component Text component.
	 * @param showPage Show page flag.
	 * @param showDate Show date flag.
	 */
	public PrinterManager(JTextComponent component, boolean showPage, boolean showDate) {
		_printedTextComponent = component;
		_showPage = showPage;
		_showDate = showDate;
	}

	/**
	 * Return the page style
	 * @return
	 */
	public AttributedString getStyle() {
		return _style;
	}

	/**
	 * Set a new value to the style.
	 * 
	 * @param style New value to set.
	 */
	public void setStyle(AttributedString style) {
		_style = style;
	}

	/**
	 * Configures the page to print it.
	 */
	public void configurePage() {

		_style = new AttributedString(_printedTextComponent.getText());
		_pageFormat = null;
		// Get the PrinterJob object
		_printerJob = PrinterJob.getPrinterJob();
		new HashPrintRequestAttributeSet();
		// Get the default page format, then allow the user to modify it
		_pageFormat = _printerJob.pageDialog(_printerJob.defaultPage());
		// Tell the PrinterJob what to print
		_printerText = new PrinterText(_printedTextComponent, _pageFormat, _showPage, _showDate);

		_printerJob.setPrintable(_printerText, _pageFormat);
	}

	/**
	 * Print the page.
	 */
	public void print() {
		try {
			_printerJob.print();

		} catch (PrinterException e) {
			e.printStackTrace();
			System.out.println("Printing error: " + e);
		}
	}

	/**
	 * Set a new value to the show date flag.
	 * 
	 * @param showDate New value to set.
	 */
	public void setDate(boolean showDate) {
		_showDate = showDate;
		_printerText.setDate(_showDate);
	}
	
	/**
	 * Set a new value to the show page flag.
	 * 
	 * @param showPage New value to set.
	 */
	public void setShowPage(boolean showPage) {
		_showPage = showPage;
		_printerText.setShowPage(_showPage);
	}

	/**
	 * Returns the page format.
	 * 
	 * @return The page format.
	 */
	public Object getPageFormat() {
		return _pageFormat;
	}
}
