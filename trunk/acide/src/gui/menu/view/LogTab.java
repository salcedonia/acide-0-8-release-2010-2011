package gui.menu.view;

import es.text.TextFile;
import gui.MainWindow;
import javax.swing.JFrame;

/**
 * Log tab to display in the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class LogTab extends JFrame {
	
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Log file path.
	 */
	private String _logFilePath = ".//log/logfile.txt";
	
	/**
	 * Constructor of the class.
	 */
	public LogTab() {
		
		MainWindow.getInstance().getEditorBuilder().newTab("Log", "Log", getTextLog(), false, 0);
	}
	
	/**
	 * Returns the text log.
	 * 
	 * @return The text log.
	 */
	public String getTextLog() {
		
		TextFile textFile = new TextFile();
		String fileContent = null;
		fileContent = textFile.load(_logFilePath);
		return fileContent;
	}
}
