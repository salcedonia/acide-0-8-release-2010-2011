package operations.factory;

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.project.workbench.AcideWorkbenchConfigurationLoader;
import gui.menuBar.editMenu.utils.SearchEngine;
import gui.menuBar.fileMenu.utils.PrinterManager;

import javax.swing.text.JTextComponent;

/************************************************************************
 * Builds the most relevant operations of ACIDE - A Configurable IDE.
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
public class AcideOperationsFactory {

	/**
	 * Operations factory unique class instance.
	 */
	private static AcideOperationsFactory _instance;

	/**
	 * Returns the unique class instance.
	 * 
	 * @return the unique class instance.
	 * @see AcideOperationsFactory
	 */
	public static AcideOperationsFactory getInstance() {
		if (_instance == null)
			_instance = new AcideOperationsFactory();
		return _instance;
	}

	/**
	 * Builds the search class of ACIDE - A Configurable IDE.
	 * 
	 * @return The search class of ACIDE - A Configurable IDE.
	 * @see SearchEngine
	 */
	public SearchEngine buildSearch() {
		return new SearchEngine();
	}

	/**
	 * Builds the project configuration.
	 * 
	 * @return the project configuration.
	 * @see AcideProjectConfiguration
	 */
	public AcideProjectConfiguration buildProjectConfiguration() {
		return new AcideProjectConfiguration();
	}

	/**
	 * Builds the printer manager of ACIDE - A Configurable IDE.
	 * 
	 * @param component
	 *            text component to print.
	 * @param page
	 *            indicates if the page number has to be showed on the printed
	 *            page or not.
	 * @param date
	 *            indicates if the date has to be showed on the printed page or
	 *            not.
	 * @return the printer manager of ACIDE - A Configurable IDE.
	 * @see PrinterManager
	 */
	public PrinterManager buildPrinterManager(JTextComponent component,
			boolean page, boolean date) {
		return new PrinterManager(component, page, date);
	}

	/**
	 * Builds the project launcher of ACIDE - A Configurable IDE.
	 * 
	 * @return the project launcher of ACIDE - A Configurable IDE.
	 * @see AcideWorkbenchConfigurationLoader
	 */
	public static AcideWorkbenchConfigurationLoader buildAcideWorkbenchConfigurationLoader() {
		return new AcideWorkbenchConfigurationLoader();
	}
}
