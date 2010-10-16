package operations.configuration;

/**
 * 
 */
public class GrammarConfiguration {

	/**
	 * 
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
	 * 
	 * @return
	 */
	public static String getPath() {
		return _path;
	}

	/**
	 * 
	 * @param path
	 */
	public static void setPath(String path) {
		
		_path = path;
	}
}
