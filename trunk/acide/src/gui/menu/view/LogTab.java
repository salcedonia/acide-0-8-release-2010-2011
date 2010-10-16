package gui.menu.view;

import es.text.TextFile;
import gui.MainWindow;
import javax.swing.JFrame;

/**
 * 
 */
public class LogTab extends JFrame {
	
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private String _logFilePath = ".//log/logfile.txt";
	
	/**
	 * Constructor of the class.
	 */
	public LogTab() {
		
		MainWindow.getInstance().getEditorBuilder().newTab("Log", "Log", getTextLog(), false, 0);
	}
	
	/**
	 * 
	 * @return
	 */
	public String getTextLog() {
		
		TextFile f = new TextFile();
		String s = null;
		s = f.load(_logFilePath);
		return s;
	}
}
