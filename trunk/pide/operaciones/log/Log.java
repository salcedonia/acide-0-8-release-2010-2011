package operaciones.log;

import java.io.IOException;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/**
 * Clase que sirve de interfaz con el resto de la aplicación y las utilidades
 * del paquete org.apache.log4j. Patrón Fachada
 */
public class Log {

	/**
	 * Atributo log: servirá para crear un único logger y poder configurar sus
	 * características en el constructor.
	 */
	private static Logger log;

	/**
	 * Crea un solo atributo log y lo inicializa con las características
	 * adecuadas.
	 */
	public static void startLog() {
		if (log == null) {
			log = Logger.getLogger("LogAcide");
			log.setLevel(Level.ALL);
			try {
				PatternLayout p = new PatternLayout("%d{date} [%t] %-5p %c - %m\n");
				RollingFileAppender roll = new RollingFileAppender(p,".//log/logfile.txt");
				roll.setAppend(false);
				roll.setMaxBackupIndex(2);
				roll.setMaxFileSize("100KB");
				roll.rollOver();
				log.addAppender(roll);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Esta clase devuelve el único atributo de la clase para poder estar
	 * manejando siempre el mismo Logger durante la aplicación.
	 * 
	 * @return El atributo log de la clase que es de tipo Logger.
	 */
	public static Logger getLog() {
		return log;
	}

	/**
	 * Esta clase sirve de fachada para el paquete org.apache.log4j y se encarga
	 * de devolver un log nuevo con el nombre que le indicamos en los
	 * parámetros.
	 * 
	 * @param name:
	 *            Nombre del nuevo Logger que queremos obtener.
	 * @return Un objeto de tipo Logger con el nombre especificado en los
	 *         parámetros.
	 */
	public static Logger getLog(String name) {
		return Logger.getLogger(name);
	}

}
