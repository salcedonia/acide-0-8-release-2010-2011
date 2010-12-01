package operations.output;
import gui.output.OutputPanel;

import java.io.*;

import operations.log.Log;

/************************************************************************																
 * Handles the output process thread for the output and for the error										
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
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan Jos� Ortiz S�nchez										
 *         </ul>														
 *         <ul>															
 *         Delf�n Rup�rez Ca�as											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Mart�n L�zaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo G�mez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8		
 * @see Thread																												
 ***********************************************************************/
class OutputProcessThread extends Thread {
	
	/**
	 * Input stream
	 */
	private InputStream _inputStream;
	/**
	 * Output shell of the application
	 */
	private OutputPanel _output;

	/**
	 * Class constructor
	 * 
	 * @param inputStream Input stream
	 * @param output Output of the main window of the application
	 * @see InputStream
	 * @see OutputPanel
	 */
	public OutputProcessThread(InputStream inputStream, OutputPanel output) {
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
			
			int i = 0;
			
			while ((i = bufferedReader.read()) != -1) {
				
				if (i != 13)
					stringBuffer.append((char) i);
				
				if (!bufferedReader.ready()) {
					
					_output.addText(stringBuffer.toString());
					stringBuffer = new StringBuffer();
				}
			}
			
			if (stringBuffer.length() != 0)
				_output.addText(stringBuffer.toString());
			
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}