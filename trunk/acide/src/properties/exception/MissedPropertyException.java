package properties.exception;

import java.util.ResourceBundle;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

/**
 * Missed property exception.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class MissedPropertyException extends Exception {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Parameter that raised the exception.
	 */
	private String _parameter;
	/**
	 * Labels to display in the selected language.
	 */
	private static ResourceBundle _labels = Language.getInstance().getLabels();
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	
	/**
	 * Constructor of the class.
	 * 
	 * @param parameter Parameter that raised the exception.
	 */
	public MissedPropertyException(String parameter) {
		super(_labels.getString("s426") + parameter + "'");
		_logger.info(_labels.getString("s427") + parameter + "'");
		_parameter = parameter;
	}

	/**
	 * Returns the parameter that raised the exception.
	 * 
	 * @return The parameter that raised the exception.
	 */
	public String getParameter() {
		return _parameter;
	}
}

