package operations.factory;

import es.configuration.project.ProjectConfiguration;
import es.configuration.project.defaultConfiguration.ProjectLauncher;
import gui.menu.edit.utils.Search;
import gui.menu.file.utils.PrinterManager;

import javax.swing.text.JTextComponent;

/************************************************************************
 * Builds the most relevant operations of ACIDE - A Configurable IDE
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 ***********************************************************************/
public class OperationsFactory {

	/**
	 * Class instance
	 */
	private static OperationsFactory _instance;

	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 * @see OperationsFactory
	 */
	public static OperationsFactory getInstance() {
		if (_instance == null)
			_instance = new OperationsFactory();
		return _instance;
	}

	/**
	 * Builds the search class of ACIDE - A Configurable IDE
	 * 
	 * @return The search class of ACIDE - A Configurable IDE
	 * @see Search
	 */
	public Search buildSearch() {
		return new Search();
	}

	/**
	 * Builds the project configuration
	 * 
	 * @return the project configuration
	 * @see ProjectConfiguration
	 */
	public ProjectConfiguration buildProjectConfiguration() {
		return new ProjectConfiguration();
	}

	/**
	 * Builds the printer manager of ACIDE - A Configurable IDE
	 * 
	 * @param component
	 *            text component to print
	 * @param page
	 *            indicates if the page number has to be showed on the printed
	 *            page or not
	 * @param date
	 *            indicates if the date has to be showed on the printed page or
	 *            not
	 * @return the printer manager of ACIDE - A Configurable IDE
	 * @see PrinterManager
	 */
	public PrinterManager buildPrinterManager(JTextComponent component,
			boolean page, boolean date) {
		return new PrinterManager(component, page, date);
	}

	/**
	 * Builds the project launcher of ACIDE - A Configurable IDE
	 * 
	 * @return the project launcher of ACIDE - A Configurable IDE
	 * @see ProjectLauncher
	 */
	public static ProjectLauncher buildProjectLauncher() {
		return new ProjectLauncher();
	}
}
