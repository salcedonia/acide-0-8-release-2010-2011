package operations.configuration;

/**
 * Associates a file extension to an executable program on Windows
 * NT/2000/XP/2003/Vista.
 */
public class WindowsOSFileAssociation {

//	********************************************************************************
//	**								ORIGINAL CODE								  **
//	********************************************************************************
//	static final String USAGE = "\n\nUsage: java FileAssoc {fileType} {extension} {executable}\n"
//			+ "   Eg. java FileAssoc NotepadAaa .aaa \"C:\\WINDOWS\\notepad.exe\"\n\n";
//
//	String fileType;
//
//	String extension;
//
//	String executable;
//
//	public static void main(String[] args) throws Exception {
//		new FileAssoc().exec(args);
//	}
//
//	void exec(String[] args) throws Exception {
//		readParameters(args);
//		executeCommand("cmd /c assoc " + extension + "=" + fileType);
//		executeCommand("cmd /c ftype " + fileType + "=\"" + executable
//				+ "\" \"%1\" %*");
//	}
//
//	void readParameters(String[] args) throws Exception {
//		if (args.length != 3) throw new Exception(USAGE);
//		fileType = args[0];
//		extension = args[1];
//		executable = args[2];
//	}
//
//	void executeCommand(String command) throws Exception {
//		Process p = Runtime.getRuntime().exec(command);
//		p.waitFor();
//	}
	
	/**
	 * 
	 */
	public static void associateToWindowsReg(String fileType, String extension, String executable) throws Exception {
		Process p1 = Runtime.getRuntime().exec("cmd /c assoc " + extension + "=" + fileType);
		p1.waitFor();		
		Process p2 = Runtime.getRuntime().exec("cmd /c ftype " + fileType + "=\"" + executable + "\" \"%1\" %*");
		p2.waitFor();
	}
}
