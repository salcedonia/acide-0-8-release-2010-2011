package operations.output;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * Handles the process input thread.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ProcessInputThread extends Thread {

	/**
	 * Buffered writer.
	 */
	private BufferedWriter _writer;
	/**
	 * Input stream.
	 */
	private InputStream _input;

	/**
	 * Constructor of the class.
	 * 
	 * @param writer New buffered reader.
	 * @param input New input stream.
	 */
	public ProcessInputThread(BufferedWriter writer, InputStream input) {
		_writer = writer;
		_input = input;
	}

	/**
	 * Main method of the thread.
	 */
	public synchronized void run() {
		try {
			_writer.flush();
			BufferedReader br = new BufferedReader(
					new InputStreamReader(_input));
			int i = 0;
			while ((i = br.read()) != -1) {
				_writer.write((char) i);
				_writer.flush();
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}