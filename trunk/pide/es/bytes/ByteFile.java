package es.bytes;

import idioma.Idioma;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import principal.almacenPropiedades;

public class ByteFile {
	
	public static void copy(String fromFileName, String toFileName) throws IOException {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		File fromFile = new File(fromFileName);
		File toFile = new File(toFileName);
		/*if (!fromFile.exists()) 
			throw new IOException("FileCopy: " + "no such source file: " + fromFileName);
		if (!fromFile.isFile())
			throw new IOException("FileCopy: " + "can't copy directory: " + fromFileName);
		if (!fromFile.canRead())
			throw new IOException("FileCopy: " + "source file is unreadable: " + fromFileName);
		if (toFile.isDirectory())
			toFile = new File(toFile, fromFile.getName());
		if (toFile.exists()) {
			if (!toFile.canWrite())
				throw new IOException("FileCopy: " + "destination file is unwriteable: " + toFileName);
			System.out.print("Overwrite existing file " + toFile.getName() + "? (Y/N): ");
			System.out.flush();
			BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
			String response = in.readLine();
			if (!response.equals("Y") && !response.equals("y"))
				throw new IOException("FileCopy: " + "existing file was not overwritten.");
		} 
		else {
			String parent = toFile.getParent();
			if (parent == null)
				parent = System.getProperty("user.dir");
			File dir = new File(parent);
			if (!dir.exists())
				throw new IOException("FileCopy: " + "destination directory doesn't exist: " + parent);
			if (dir.isFile())
				throw new IOException("FileCopy: " + "destination is not a directory: " + parent);
			if (!dir.canWrite())
				throw new IOException("FileCopy: " + "destination directory is unwriteable: " + parent);
		}*/
		FileInputStream from = null;
		FileOutputStream to = null;
		try {
			from = new FileInputStream(fromFile);
			to = new FileOutputStream(toFile);
			byte[] buffer = new byte[4096];
			int bytesRead;
			while ((bytesRead = from.read(buffer)) != -1)
				to.write(buffer, 0, bytesRead); // write
		} 
		finally {
			if (from != null)
				try {
					from.close();
				} 
				catch (IOException e) {
					JOptionPane.showMessageDialog(null,labels.getString("s265") + fromFileName,labels.getString("s266"),JOptionPane.ERROR_MESSAGE);
				}
			if (to != null)
				try {
					to.close();
				} 
				catch (IOException e) {
					JOptionPane.showMessageDialog(null,labels.getString("s267") + toFileName,labels.getString("268"),JOptionPane.ERROR_MESSAGE);
				}
		}
	}
}
