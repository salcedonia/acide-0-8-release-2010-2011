package properties.exception;

import java.util.ResourceBundle;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

/**
 * 
 */
public class MissedPropertyException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private String _parameter;
	/**
	 * 
	 */
	private static ResourceBundle _labels = Language.getInstance().getLabels();
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 * @param parameter
	 */
	public MissedPropertyException(String parameter) {
		super(_labels.getString("s426") + parameter + "'");
		_logger.info(_labels.getString("s427") + parameter + "'");
		_parameter = parameter;
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public String getNombreParametro() {
		return _parameter;
	}
}

