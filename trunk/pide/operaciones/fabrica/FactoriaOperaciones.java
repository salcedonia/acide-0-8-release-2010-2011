package operaciones.fabrica;

import javax.swing.text.JTextComponent;

import operaciones.configuracion.DefaultConfiguration;
import operaciones.configuracion.LoadDefaultProject;
import operaciones.configuracion.ProyectoConfig;
import operaciones.genericas.PrinterManager;
import operaciones.genericas.Search;

/**
 * Clase que crea objetos del paquete operacion por medio de factoria abstracta
 * singleton
 * 
 * 
 */
public class FactoriaOperaciones {

	private static FactoriaOperaciones instancia;

	// Crea una unica instancia de FactoriaES
	public static FactoriaOperaciones getInstance() {
		if (instancia == null)
			instancia = new FactoriaOperaciones();
		return instancia;
	}

	/**
	 * Genera clase Buscar
	 * 
	 * @return
	 */
	public Search generaBuscar() {
		return new Search();
	}

	/**
	 * Genera clase ProyectoConfig
	 * 
	 * @return
	 */
	public ProyectoConfig generaProyectoConfig() {
		return new ProyectoConfig();
	}

	/**
	 * Build Printer
	 * 
	 * @return
	 */
	public PrinterManager buildPrinterManager(JTextComponent component,
			boolean page, boolean date) {
		return new PrinterManager(component, page, date);
	}

	public LoadDefaultProject BuildLoadDefaultProject() {
		return new LoadDefaultProject();
	}

	public DefaultConfiguration buildDefaultConfiguration() {
		return new DefaultConfiguration();
	}
}
