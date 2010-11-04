package operations.output;
import gui.output.Output;

import java.io.*;

/**
 * Handles the stream gobbler.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class StreamGobbler extends Thread {
	
	/**
	 * Input stream.
	 */
	private InputStream _inputStream;
	/**
	 * Output shell of the application.
	 */
	private Output _output;

	/**
	 * Constructor of the class.
	 * 
	 * @param inputStream
	 * @param output
	 */
	StreamGobbler(InputStream inputStream, Output output) {
		_inputStream = inputStream;
		_output = output;
	}

	/**
	 * Main method of the class.
	 */
	public synchronized void run() {
		try {
			
			InputStreamReader isr = new InputStreamReader(_inputStream);
			BufferedReader br = new BufferedReader(isr);
			StringBuffer sb = new StringBuffer();
			int i = 0;
			while ((i = br.read()) != -1) {
				if (i != 13)
					sb.append((char) i);
				if (br.ready() == false) {
					_output.addText(sb.toString());
					sb = new StringBuffer();
				}
			}
			if (sb.length() != 0)
				_output.addText(sb.toString());
		} catch (Exception ioe) {
			ioe.printStackTrace();
		}
	}
}
