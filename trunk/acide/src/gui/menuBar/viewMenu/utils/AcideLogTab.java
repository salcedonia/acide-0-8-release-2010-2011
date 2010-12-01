package gui.menuBar.viewMenu.utils;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

/************************************************************************																
 * Log tab to display in the editor of ACIDE - A Configurable IDE.											
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
public class AcideLogTab{
	
	/**
	 * Log file path.
	 */
	private static final String LOG_FILE_PATH = ".//log/logfile.txt";
	/**
	 * Log file content.
	 */
	private String _logFileContent = "";
	
	/**
	 * Creates a log tab.
	 */
	public AcideLogTab() {
		
		// Retrieves the log file content
		TextFile textFile = new TextFile();
		_logFileContent =textFile.load(LOG_FILE_PATH);
		
		if(_logFileContent != null)
			MainWindow.getInstance().getFileEditorManager().newTab("Log", "Log", _logFileContent, false, 0);
	}
	
	/**
	 * Returns the text log.
	 * 
	 * @return the text log.
	 */
	public String getLogFileContent() {
		return _logFileContent;
	}
	
	/**
	 * Sets a new value to the log file content.
	 * 
	 * @param logFileContent new value to set.
	 */
	public void setLogFileContent(String logFileContent){
		_logFileContent = logFileContent;
	}
}
