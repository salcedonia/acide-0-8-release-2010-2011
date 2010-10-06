package operaciones.salida;
import gui.Ventana;
import gui.salida.Salida;

import java.io.*;

class StreamGobbler extends Thread {
	InputStream is;
	OutputStream os;
	Salida salida;

	StreamGobbler(InputStream is, OutputStream os, Salida salida) {
		this.is = is;
		this.os = os;
		this.salida = salida;
	}

	public synchronized void run() {
		try {
			Ventana v = Ventana.getInstance();
			InputStreamReader isr = new InputStreamReader(is);
			BufferedReader br = new BufferedReader(isr);
			StringBuffer sb = new StringBuffer();
			int i = 0; 
		    while ((i = br.read()) != -1){ 
				if (i!=13) sb.append((char) i);
		    	if (br.ready() == false){
		    		salida.añadeTexto(sb.toString());
		    		sb = new StringBuffer();
		    	}
		    }	
		    if (sb.length()!= 0) salida.añadeTexto(sb.toString());
		} catch (Exception ioe) {
			ioe.printStackTrace();
		}
	}
}

