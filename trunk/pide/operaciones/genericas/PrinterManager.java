package operaciones.genericas;

import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.Map;

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.swing.text.JTextComponent;

public class PrinterManager {
	private PrinterJob printerJob;

	private PageFormat format;

	private JTextComponent text_print;

	private AttributedString style;

	private HashPrintRequestAttributeSet aset;

	private boolean pag;
	
	private boolean date;
	
	private PrinterText pt;
	
	public PrinterManager(JTextComponent component,boolean pag,boolean date) {
		text_print = component;
	    this.pag=pag;
	    this.date=date;
	}

	public AttributedString getStyle() {
		return style;
	}

	public void setStyle(AttributedString style) {
		this.style = style;
	}

	public void configPage() {
		// TODO Auto-generated method stub
		style = new AttributedString(text_print.getText());
		format=null;
		// Get the PrinterJob object
        printerJob = PrinterJob.getPrinterJob();
		aset = new HashPrintRequestAttributeSet();
        // Get the default page format, then allow the user to modify it
		format = printerJob.pageDialog(printerJob.defaultPage());
		// Tell the PrinterJob what to print
		pt=new PrinterText(text_print, format,pag,date);
		
		printerJob.setPrintable(pt,format);
	}

	public void imprimir() {
			try {
				printerJob.print();

			} catch (PrinterException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				System.out.println("Printing error: " + e);
			}
	}


	public void setDate(boolean date) {
		this.date = date;
	  pt.setDate(this.date);
	}

	public void setPag(boolean pag) {
		this.pag = pag;
	    pt.setPage(this.pag);
	}

	public Object getFormat() {
		// TODO Auto-generated method stub
		return format;
	}

}
