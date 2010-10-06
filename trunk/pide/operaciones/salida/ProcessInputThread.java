package operaciones.salida;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;

public class ProcessInputThread extends Thread {
    
    private BufferedWriter writer;
    private InputStream input;
    
    public ProcessInputThread(BufferedWriter writer, InputStream input) {
        this.writer = writer;
        this.input = input;
    }
    
    public synchronized void run() {
        try {
            writer.flush( );
            BufferedReader br = new BufferedReader(new InputStreamReader(input));
            int i = 0; 
		    while ((i = br.read()) != -1){ 
		    		writer.write((char)i);
		    		writer.flush();
		    	}	
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
    
}