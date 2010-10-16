package operations.output;
import gui.output.Output;

import java.io.*;

/**
 * 
 */
class StreamGobbler extends Thread {
	
	/**
	 * 
	 */
	private InputStream _inputStream;
	/**
	 * 
	 */
	OutputStream _outputStream;
	/**
	 * 
	 */
	Output _output;

	/**
	 * Constructor of the class.
	 * 
	 * @param inputStream
	 * @param outputStream
	 * @param output
	 */
	StreamGobbler(InputStream inputStream, OutputStream outputStream, Output output) {
		_inputStream = inputStream;
		_outputStream = outputStream;
		_output = output;
	}

	/**
	 * 
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
