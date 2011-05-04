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

import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.text.AttributedString;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.swing.text.JTextComponent;

import acide.gui.mainWindow.AcideMainWindow;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE printer manager.
 * 
 * @version 0.8
 */
public class AcidePrinterManager {

	/**
	 * ACIDE - A Configurable IDE printer manager unique class instance.
	 */
	private static AcidePrinterManager _instance;
	/**
	 * ACIDE - A Configurable IDE printer manager printer job.
	 */
	private PrinterJob _printerJob;
	/**
	 * ACIDE - A Configurable IDE printer manager page format.
	 */
	private PageFormat _pageFormat;
	/**
	 * ACIDE - A Configurable IDE printer manager printed text component.
	 */
	private JTextComponent _printedTextComponent;
	/**
	 * ACIDE - A Configurable IDE printer manager style of the document.
	 */
	private AttributedString _style;
	/**
	 * ACIDE - A Configurable IDE printer manager show page flag.
	 */
	private boolean _showPage;
	/**
	 * ACIDE - A Configurable IDE printer manager show date flag.
	 */
	private boolean _showDate;
	/**
	 * ACIDE - A Configurable IDE printer manager printer text.
	 */
	private AcidePrinterText _printerText;

	/**
	 * Returns the ACIDE - A Configurable IDE printer manager unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE printer manager unique class
	 *         instance.
	 */
	public static AcidePrinterManager getInstance() {
		if (_instance == null)
			_instance = new AcidePrinterManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE printer manager.
	 */
	public AcidePrinterManager() {

		// Sets the printed text component
		_printedTextComponent = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getActiveTextEditionArea();

		// Sets the show page
		_showPage = false;

		// Sets the show date
		_showDate = false;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE printer manager page style.
	 * 
	 * @return the ACIDE - A Configurable IDE printer manager page style.
	 */
	public AttributedString getStyle() {
		return _style;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE printer manager style.
	 * 
	 * @param style
	 *            new value to set.
	 */
	public void setStyle(AttributedString style) {
		_style = style;
	}

	/**
	 * Configures the page to print it.
	 */
	public void configurePage() {

		// Creates the style
		_style = new AttributedString(_printedTextComponent.getText());
		_pageFormat = null;

		// Gets the PrinterJob object
		_printerJob = PrinterJob.getPrinterJob();
		new HashPrintRequestAttributeSet();

		// Gets the default page format, then allows the user to modify it
		_pageFormat = _printerJob.pageDialog(_printerJob.defaultPage());

		// Tells the PrinterJob what to print
		_printerText = new AcidePrinterText(_printedTextComponent, _pageFormat,
				_showPage, _showDate);

		// Sets the printer job as printable
		_printerJob.setPrintable(_printerText, _pageFormat);
	}

	/**
	 * Prints the page.
	 */
	public void print() {

		try {

			// Prints the job
			_printerJob.print();
		} catch (PrinterException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE printer manager show
	 * date flag.
	 * 
	 * @param showDate
	 *            new value to set.
	 */
	public void setDate(boolean showDate) {
		_showDate = showDate;
		_printerText.setDate(_showDate);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE printer manager show
	 * page flag.
	 * 
	 * @param showPage
	 *            new value to set.
	 */
	public void setShowPage(boolean showPage) {
		_showPage = showPage;
		_printerText.setShowPage(_showPage);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE printer manager page format.
	 * 
	 * @return the ACIDE - A Configurable IDE printer manager page format.
	 */
	public Object getPageFormat() {
		return _pageFormat;
	}
}
