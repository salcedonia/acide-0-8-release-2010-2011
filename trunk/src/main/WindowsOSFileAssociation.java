package main;

/**
 * Associates a file extension to an executable program on Windows
 * NT/2000/XP/2003/Vista.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class WindowsOSFileAssociation {
	
	/**
	 * Associates a file to a Windows Register.
	 */
	public static void associateToWindowsReg(String fileType, String extension, String executable) throws Exception {
		Process p1 = Runtime.getRuntime().exec("cmd /c assoc " + extension + "=" + fileType);
		p1.waitFor();		
		Process p2 = Runtime.getRuntime().exec("cmd /c ftype " + fileType + "=\"" + executable + "\" \"%1\" %*");
		p2.waitFor();
	}
}
