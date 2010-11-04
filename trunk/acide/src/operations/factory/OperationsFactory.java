package operations.factory;

import es.configuration.project.ProjectConfiguration;
import es.configuration.project.defaultConfiguration.DefaultProjectLoader;
import gui.menu.edit.Search;
import gui.menu.print.PrinterManager;

import javax.swing.text.JTextComponent;


/**
 * Builds the most relevant operations of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OperationsFactory {

	/**
	 * Instance of the class.
	 */
	private static OperationsFactory _instance;

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static OperationsFactory getInstance() {
		if (_instance == null)
			_instance = new OperationsFactory();
		return _instance;
	}

	/**
	 * Build the search of the application.
	 * 
	 * @return The search of the application.
	 */
	public Search buildSearch() {
		return new Search();
	}

	/**
	 * Build the project configuration.
	 * 
	 * @return The project configuration.
	 */
	public ProjectConfiguration buildProjectConfiguration() {
		return new ProjectConfiguration();
	}

	/**
	 * Build the printer of the application.
	 * 
	 * @return The printer of the application.
	 */
	public PrinterManager buildPrinterManager(JTextComponent component,
			boolean page, boolean date) {
		return new PrinterManager(component, page, date);
	}

	/**
	 * Build the default project loader of the application.
	 * 
	 * @return The default project loader of the application.
	 */
	public DefaultProjectLoader buildDefaultProjectLoader() {
		return new DefaultProjectLoader();
	}
}
