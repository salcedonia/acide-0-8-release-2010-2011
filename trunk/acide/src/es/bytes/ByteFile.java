package es.bytes;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import properties.PropertiesManager;

import language.Language;

/**
 * 
 */
public class ByteFile {

	/**
	 * 
	 * @param fromFileName
	 * @param toFileName
	 * @throws IOException
	 */
	public static void copy(String fromFileName, String toFileName)
			throws IOException {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = language.getLabels();
		
		File fromFile = new File(fromFileName);
		File toFile = new File(toFileName);
		FileInputStream from = null;
		FileOutputStream to = null;
		
		try {
			from = new FileInputStream(fromFile);
			to = new FileOutputStream(toFile);
			byte[] buffer = new byte[4096];
			int bytesRead;
			while ((bytesRead = from.read(buffer)) != -1)
				to.write(buffer, 0, bytesRead); // write
		} finally {
			if (from != null)
				try {
					from.close();
				} catch (IOException e) {
					JOptionPane
							.showMessageDialog(null, labels.getString("s265")
									+ fromFileName, labels.getString("s266"),
									JOptionPane.ERROR_MESSAGE);
				}
			if (to != null)
				try {
					to.close();
				} catch (IOException e) {
					JOptionPane.showMessageDialog(null,
							labels.getString("s267") + toFileName,
							labels.getString("268"), JOptionPane.ERROR_MESSAGE);
				}
		}
	}
}
