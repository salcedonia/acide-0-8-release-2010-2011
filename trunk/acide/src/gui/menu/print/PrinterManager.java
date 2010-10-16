package gui.menu.print;

import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.text.AttributedString;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.swing.text.JTextComponent;

/**
 * 
 */
public class PrinterManager {

	/**
	 * 
	 */
	private PrinterJob _printerJob;
	/**
	 * 
	 */
	private PageFormat _format;
	/**
	 * 
	 */
	private JTextComponent _textPrinted;
	/**
	 * 
	 */
	private AttributedString _style;
	/**
	 * 
	 */
	private boolean _showPage;
	/**
	 * 
	 */
	private boolean _showDate;
	/**
	 * 
	 */
	private PrinterText _printerText;

	/**
	 * 
	 * @param component
	 * @param pag
	 * @param date
	 */
	public PrinterManager(JTextComponent component, boolean pag, boolean date) {
		_textPrinted = component;
		_showPage = pag;
		_showDate = date;
	}

	/**
	 * 
	 * @return
	 */
	public AttributedString getStyle() {
		return _style;
	}

	/**
	 * 
	 * @param style
	 */
	public void setStyle(AttributedString style) {
		_style = style;
	}

	/**
	 * 
	 */
	public void configurePage() {

		_style = new AttributedString(_textPrinted.getText());
		_format = null;
		// Get the PrinterJob object
		_printerJob = PrinterJob.getPrinterJob();
		new HashPrintRequestAttributeSet();
		// Get the default page format, then allow the user to modify it
		_format = _printerJob.pageDialog(_printerJob.defaultPage());
		// Tell the PrinterJob what to print
		_printerText = new PrinterText(_textPrinted, _format, _showPage, _showDate);

		_printerJob.setPrintable(_printerText, _format);
	}

	/**
	 * 
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
	 * 
	 * @param date
	 */
	public void setDate(boolean date) {
		_showDate = date;
		_printerText.setDate(_showDate);
	}
	
	/**
	 * 
	 * @param pag
	 */
	public void setPage(boolean pag) {
		_showPage = pag;
		_printerText.setPage(_showPage);
	}

	/**
	 * 
	 * @return
	 */
	public Object getFormat() {
		return _format;
	}
}
