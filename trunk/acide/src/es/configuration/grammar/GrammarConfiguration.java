package es.configuration.grammar;

/**
 * Handle the grammar configuration of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class GrammarConfiguration {

	/**
	 * Grammar path.
	 */
	public static String _path;

	/**
	 * Constructor of the class.
	 */
	public GrammarConfiguration() {

	}

	/**
	 * Constructor of the class.
	 * 
	 * @param path Absolute path where the file is.
	 */
	public GrammarConfiguration(String path) {	
		_path = path;
	}

	/**
	 * Returns the grammar path.
	 * 
	 * @return The grammar path.
	 */
	public static String getPath() {
		return _path;
	}

	/**
	 * Set a new value to the grammar path.
	 * 
	 * @param path New value to set.
	 */
	public static void setPath(String path) {	
		_path = path;
	}
}
