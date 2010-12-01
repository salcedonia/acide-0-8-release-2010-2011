package operations.output;

import es.configuration.output.OutputConfiguration;
import gui.mainWindow.MainWindow;
import gui.outputPanel.AcideOutputPanel;

import java.io.*;

import operations.log.AcideLog;

/************************************************************************																
 * Thread that executes the external shell and captures the input and output
 * streams into different threads for the output of the application being
 * thread-safe.									
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
public class OutputThread extends Thread {

	/**
	 * Buffered writer.
	 */
	private static BufferedWriter _writer = null;
	/**
	 * Process to execute.
	 */
	public Process _process = null;

	/**
	 * Class constructor.
	 */
	public OutputThread() {

	}

	/**
	 * Main method of the thread.
	 */
	public synchronized void run() {

		// Use "/bin/sh" in Linux.
		String shellPath = null;
		String shellDirectory = null;
		try {
			
			shellPath = OutputConfiguration.getInstance().getShellPath();
			shellDirectory = OutputConfiguration.getInstance()
					.getShellDirectory();
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		if (!shellPath.equals("") && !shellDirectory.equals("")
				&& !shellDirectory.equals("null") && !shellPath.equals("null")) {

			try {

				File path = new File(shellDirectory);
				_process = Runtime.getRuntime().exec(shellPath, null, path);
				
			} catch (Exception exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
			
			// CREATES THE OUTPUT STREAM WRITER
			_writer = new BufferedWriter(new OutputStreamWriter(
					_process.getOutputStream()));

			// CREATE THE INPUT STREAM
			InputProcessThread inputThread = new InputProcessThread(
					_writer, System.in);

			// CREATE THE ERROR STREAM
			OutputProcessThread errorGobbler = new OutputProcessThread(
					_process.getErrorStream(), MainWindow.getInstance()
							.getOutput());

			// CREATE THE OUTPUT STREAM
			OutputProcessThread outputGobbler = new OutputProcessThread(
					_process.getInputStream(), MainWindow.getInstance()
							.getOutput());

			// STARTS THE THREADS
			errorGobbler.start();
			outputGobbler.start();
			inputThread.start();

			// WAIT UNTIL THIS THREAD FINISHES HIS JOB
			try {
				_process.waitFor();
			} catch (InterruptedException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * Returns the buffered writer.
	 * 
	 * @return the buffered writer.
	 */
	public BufferedWriter getWriter() {
		return _writer;
	}

	/**
	 * Returns the process to execute.
	 * 
	 * @return the process to execute.
	 */
	public Process getProcess() {
		return _process;
	}

	/**
	 * Executes a command.
	 * 
	 * @param shell
	 *            shell in which the command is going to be executed.
	 * @param shellPath
	 *            shell path.
	 * @param command
	 *            command to execute.
	 * @param exit
	 *            exit command.
	 * @param output
	 *            output in which the result of the execution will be displayed.
	 */
	public void executeCommand(String shell, String shellPath, String command,
			String exit, AcideOutputPanel output) {

		Process process = null;
		String pathOutput = shellPath;

		try {

			File filePath = new File(pathOutput);
			process = Runtime.getRuntime().exec(shell, null, filePath);
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// CREATES THE WRITER
		BufferedWriter writer = new BufferedWriter(
				new OutputStreamWriter(process.getOutputStream()));

		// CREATES THE ERROR GOBBLER
		OutputProcessThread errorGobbler = new OutputProcessThread(
				process.getErrorStream(), output);

		// CREATES THE OUTPUT GOBBLER
		OutputProcessThread outputGobbler = new OutputProcessThread(
				process.getInputStream(), output);

		// STARTS THE THREADS
		errorGobbler.start();
		outputGobbler.start();

		try {
			writer.write(command + '\n');
			writer.flush();
			writer.write(exit + '\n');
			writer.flush();
		} catch (IOException exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
