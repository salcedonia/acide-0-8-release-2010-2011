package operations.output;

import java.io.IOException;
import java.io.InputStream;

/**
 * 
 */
public class BytesAvailableInputThread extends Thread {

	/**
	 * 
	 */
    private InputStream _input;
	
    /**
     * Constructor of the class.
     * 
     * @param input
     */
	public BytesAvailableInputThread(InputStream input) {
		_input = input;
	}

	/**
	 * 
	 */
	public synchronized void run() {
		try {
			int i = _input.available();
			System.out.println("Bits disponibles en Input " + i);
			while (true){
				if (i != _input.available()){
					i = _input.available();
					System.out.println("Bits disponibles en Input " + i);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
