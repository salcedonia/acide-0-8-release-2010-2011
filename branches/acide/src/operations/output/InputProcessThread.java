package operations.output;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;

import operations.log.Log;

/************************************************************************																
 * Handles the process input thread to read the commands in the output										
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
public class InputProcessThread extends Thread {

	/**
	 * Buffered writer
	 */
	private BufferedWriter _writer;
	/**
	 * Input strea
	 */
	private InputStream _input;

	/**
	 * Class constructor
	 * 
	 * @param writer
	 *            new buffered reader
	 * @param input
	 *            new input stream
	 */
	public InputProcessThread(BufferedWriter writer, InputStream input) {
		_writer = writer;
		_input = input;
	}

	/**
	 * Main method of the thread
	 */
	public synchronized void run() {
		
		try {
			_writer.flush();
			BufferedReader bufferedReader = new BufferedReader(
					new InputStreamReader(_input));

			int i = 0;

			// PUT THIS SLEEP FOR THE REPAINTING IN THE MAIN WINDOW
			//sleep(2000);
			
			// USING THIS AVOIDS THE POSIBILITY TO ENTER
			// IN A BLOCK
			if (_input.available() > 0){
				
				while ((i = bufferedReader.read()) != -1) {
					_writer.write((char) i);
					_writer.flush();
				}
			}
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}