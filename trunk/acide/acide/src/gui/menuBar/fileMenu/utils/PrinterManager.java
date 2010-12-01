package gui.menuBar.fileMenu.utils;

import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.text.AttributedString;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.swing.text.JTextComponent;

import operations.log.AcideLog;

/************************************************************************																
 * Printer manager of ACIDE - A Configurable IDE											
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
 ***********************************************************************/
public class PrinterManager {

	/**
	 * Printer job
	 */
	private PrinterJob _printerJob;
	/**
	 * Page format
	 */
	private PageFormat _pageFormat;
	/**
	 * Printed text component
	 */
	private JTextComponent _printedTextComponent;
	/**
	 * Style of the document
	 */
	private AttributedString _style;
	/**
	 * Show page flag
	 */
	private boolean _showPage;
	/**
	 * Show date flag
	 */
	private boolean _showDate;
	/**
	 * Printer text
	 */
	private PrinterText _printerText;

	/**
	 * Class constructor
	 * 
	 * @param component text component
	 * @param showPage show page flag
	 * @param showDate show date flag
	 */
	public PrinterManager(JTextComponent component, boolean showPage, boolean showDate) {
		_printedTextComponent = component;
		_showPage = showPage;
		_showDate = showDate;
	}

	/**
	 * Returns the page style
	 * 
	 * @return the page style
	 */
	public AttributedString getStyle() {
		return _style;
	}

	/**
	 * Sets a new value to the style
	 * 
	 * @param style new value to set
	 */
	public void setStyle(AttributedString style) {
		_style = style;
	}

	/**
	 * Configures the page to print it
	 */
	public void configurePage() {

		_style = new AttributedString(_printedTextComponent.getText());
		_pageFormat = null;
		
		// Gets the PrinterJob object
		_printerJob = PrinterJob.getPrinterJob();
		new HashPrintRequestAttributeSet();
		
		// Gets the default page format, then allows the user to modify it
		_pageFormat = _printerJob.pageDialog(_printerJob.defaultPage());
		
		// Tells the PrinterJob what to print
		_printerText = new PrinterText(_printedTextComponent, _pageFormat, _showPage, _showDate);

		_printerJob.setPrintable(_printerText, _pageFormat);
	}

	/**
	 * Prints the page
	 */
	public void print() {
		
		try {	
			_printerJob.print();
		} catch (PrinterException exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Sets a new value to the show date flag
	 * 
	 * @param showDate new value to set
	 */
	public void setDate(boolean showDate) {
		_showDate = showDate;
		_printerText.setDate(_showDate);
	}
	
	/**
	 * Sets a new value to the show page flag
	 * 
	 * @param showPage new value to set
	 */
	public void setShowPage(boolean showPage) {
		_showPage = showPage;
		_printerText.setShowPage(_showPage);
	}

	/**
	 * Returns the page format
	 * 
	 * @return the page format
	 */
	public Object getPageFormat() {
		return _pageFormat;
	}
}
