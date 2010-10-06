package gui.log;

import es.texto.Fichero;
import gui.Ventana;
import gui.editor.CreadorEditor;
import javax.swing.JFrame;

/**
 * Clase que crea la pestaña de log en el editor de la aplicacion para su visualizacion
 */
public class LogTab extends JFrame {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo de la clase que guarda el path donde se encuentra el archivo de log 
	 */
	private String logPath = ".//log/logfile.txt";
	
	public LogTab() {
		String texto = actualizar();
		Ventana v = Ventana.getInstance();
		CreadorEditor ce = v.getCreadorEditor();
		ce.nuevaPestaña("Log","Log",texto,false,0);
	}
	
	/**
	 * @return Texto que contiene el archivo que indica logPath
	 */
	public String actualizar() {
		Fichero f = new Fichero();
		String s = null;
		s = f.cargar(logPath);
		return s;
	}
	
}
