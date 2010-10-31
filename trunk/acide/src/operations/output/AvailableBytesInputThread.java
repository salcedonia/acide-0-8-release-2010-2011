package operations.output;

import java.io.IOException;
import java.io.InputStream;

/**
 * Handles the available bytes for the input thread.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AvailableBytesInputThread extends Thread {

	/**
	 * Input stream.
	 */
    private InputStream _input;
	
    /**
     * Constructor of the class.
     * 
     * @param input
     */
	public AvailableBytesInputThread(InputStream input) {
		_input = input;
	}

	/**
	 * Main method of the thread.
	 */
	public synchronized void run() {
		try {
			int inputAvailable = _input.available();
			System.out.println("Available bits in Input " + inputAvailable);
			while (true){
				if (inputAvailable != _input.available()){
					inputAvailable = _input.available();
					System.out.println("Available bits in Input " + inputAvailable);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
