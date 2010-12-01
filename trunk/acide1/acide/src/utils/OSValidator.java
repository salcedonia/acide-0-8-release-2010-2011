package utils;

/**
 * Get the OS in which is running the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OSValidator{
	
	/**
	 * Instance of the class.
	 */
	private static OSValidator _instance;
	
	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public OSValidator getInstance(){
		
		if (_instance == null)
			_instance = new OSValidator();
		return _instance;
	}
	 
	/**
	 * Check if the current OS is WINDOWS.
	 * 
	 * @return True if the current OS is WINDOWS and false in other case.
	 */
	public static boolean isWindows(){
 
		String os = System.getProperty("os.name").toLowerCase();
	    return (os.indexOf( "win" ) >= 0); 
	}
 
	 
	/**
	 * Check if the current OS is MAC OS.
	 * 
	 * @return True if the current OS is MAC OS and false in other case.
	 */
	public static boolean isMac(){
 
		String os = System.getProperty("os.name").toLowerCase();
	    return (os.indexOf( "mac" ) >= 0); 
	}
 
	 
	/**
	 * Check if the current OS is UNIX or LINUX.
	 * 
	 * @return True if the current OS is UNIX or LINUX and false in other case.
	 */
	public static boolean isUnixOrLinux(){
 
		String os = System.getProperty("os.name").toLowerCase();
	    return (os.indexOf( "nix") >=0 || os.indexOf( "nux") >=0);
	}
}