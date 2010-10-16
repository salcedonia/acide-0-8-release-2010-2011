package operations.output;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * 
 */
public class ProcessInputThread extends Thread {
   
	/**
	 * 
	 */
    private BufferedWriter _writer;
    /**
     * 
     */
    private InputStream _input;
    
    /**
     * 
     * @param writer
     * @param input
     */
    public ProcessInputThread(BufferedWriter writer, InputStream input) {
        _writer = writer;
        _input = input;
    }
    
    /**
     * 
     */
    public synchronized void run() {
        try {
            _writer.flush();
            BufferedReader br = new BufferedReader(new InputStreamReader(_input));
            int i = 0; 
		    while ((i = br.read()) != -1){ 
		    		_writer.write((char)i);
		    		_writer.flush();
		    	}	
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}