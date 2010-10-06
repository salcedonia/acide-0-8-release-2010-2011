package operaciones.salida;

import java.io.IOException;
import java.io.InputStream;

public class Disponible extends Thread {

    private InputStream input;
	
	public Disponible(InputStream input) {
		this.input = input;
	}

	public synchronized void run() {
		try {
			int i = input.available();
			System.out.println("Bits disponibles en Input " + i);
			while (true){
				if (i != input.available()){
					i = input.available();
					System.out.println("Bits disponibles en Input " + i);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
