package operations.output;
import gui.outputPanel.AcideOutputPanel;

import java.io.*;

import operations.log.AcideLog;

/************************************************************************																
 * Handles the output process thread for the output and for the error.
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
 * @see Thread																												
 ***********************************************************************/
class OutputProcessThread extends Thread {
	
	/**
	 * Input stream.
	 */
	private InputStream _inputStream;
	/**
	 * Output shell of the application.
	 */
	private AcideOutputPanel _output;

	/**
	 * Class constructor.
	 * 
	 * @param inputStream Input stream.
	 * @param output Output of the main window of the application.
	 * @see InputStream
	 * @see AcideOutputPanel
	 */
	public OutputProcessThread(InputStream inputStream, AcideOutputPanel output) {
		_inputStream = inputStream;
		_output = output;
	}

	/**
	 * Main method of the class
	 */
	public synchronized void run() {
		
		try {
			
			InputStreamReader inputStreamReader = new InputStreamReader(_inputStream);
			BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
			StringBuffer stringBuffer = new StringBuffer();
			
			int character = 0;
			
			while ((character = bufferedReader.read()) != -1) {
				
				if (character != 13)
					stringBuffer.append((char) character);
				
				if (!bufferedReader.ready()) {
					
					_output.addText(stringBuffer.toString());
					stringBuffer = new StringBuffer();
				}
			}
			
			if (stringBuffer.length() != 0)
				_output.addText(stringBuffer.toString());
			
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
