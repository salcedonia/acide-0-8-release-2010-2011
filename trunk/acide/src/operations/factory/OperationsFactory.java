package operations.factory;

import gui.menu.edit.Search;
import gui.menu.print.PrinterManager;

import javax.swing.text.JTextComponent;

import operations.configuration.DefaultConfiguration;
import operations.configuration.LoadDefaultProject;
import operations.configuration.ProjectConfiguration;

/**
 *
 */
public class OperationsFactory {

	/**
	 * 
	 */
	private static OperationsFactory _instance;

	/**
	 * 
	 * @return
	 */
	public static OperationsFactory getInstance() {
		if (_instance == null)
			_instance = new OperationsFactory();
		return _instance;
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public Search buildSearch() {
		return new Search();
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public ProjectConfiguration buildProjectConfiguration() {
		return new ProjectConfiguration();
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

	/**
	 * 
	 * @return
	 */
	public LoadDefaultProject buildLoadDefaultProject() {
		return new LoadDefaultProject();
	}

	/**
	 * 
	 * @return
	 */
	public DefaultConfiguration buildDefaultConfiguration() {
		return new DefaultConfiguration();
	}
}
