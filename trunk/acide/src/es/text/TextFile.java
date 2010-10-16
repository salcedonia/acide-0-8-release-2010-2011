package es.text;

import gui.MainWindow;

import javax.swing.*;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import java.io.*;
import java.util.ResourceBundle;

/**
 * 
 */
public class TextFile {

	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JFileChooser _fileChooser;

	/**
	 * Constructor of the class.
	 */
	public TextFile() {

		_fileChooser = new JFileChooser();
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public String read() {

		ResourceBundle labels = Language.getInstance().getLabels();
		String fileName = null;
		String text = null;
		File file = null;

		try {
			text = PropertiesManager.getProperty("DefaultPath");
			file = new File(text);
			_fileChooser.setDialogTitle(labels.getString("s9"));
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception e) {
			e.printStackTrace();
		}

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = _fileChooser.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s300") + fileName);
			System.out.println(fileName);
			PropertiesManager.setProperty("DefaultPath", fileName);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			_logger.info(labels.getString("s302"));
		}
		return fileName;

	}

	/**
	 * 
	 * 
	 * @return
	 */
	public String readPath() {
		
		ResourceBundle labels = Language.getInstance().getLabels();
		
		String path = " ";
		String text = null;
		File file = null;
		
		try {
			text = PropertiesManager.getProperty("DefaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception e) {
			e.printStackTrace();
		}
		_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {
			path = _fileChooser.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s303") + path);
			PropertiesManager.setProperty("DefaultPath", path);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			_logger.info(labels.getString("s304"));
		}
		return path;
	}

	/**
	 * 
	 * @param filter
	 * @return
	 */
	public String read(TextFileFilter filter) {
		
		ResourceBundle labels = Language.getInstance().getLabels();
		JFileChooser sel = new JFileChooser();
		String fileName = " ";
		String text = null;
		File file = null;
		try {
			text = PropertiesManager.getProperty("DefaultPath");
			file = new File(text);
			sel.setFileFilter(filter);
			sel.setCurrentDirectory(file.getParentFile());
		} catch (Exception e) {
			e.printStackTrace();
		}

		int value = sel.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {
			
			fileName = sel.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s305") + fileName);
			PropertiesManager.setProperty("DefaultPath", fileName);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			sel.cancelSelection();
			_logger.info(labels.getString("s306"));
		}
		return fileName;
	}

	/**
	 * 
	 * @return 
	 */
	public String write() {
		ResourceBundle labels = Language.getInstance().getLabels();
		String fileName = " ";
		String text = null;
		File file = null;
		MainWindow mainWindow = MainWindow.getInstance();
		try {
			text = PropertiesManager.getProperty("DefaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file);
		} catch (Exception e) {
			e.printStackTrace();
		}
		boolean a = true;
		int value = _fileChooser.showSaveDialog(_fileChooser);
		File f = _fileChooser.getSelectedFile();
		if (value == JFileChooser.APPROVE_OPTION) {
			if (f.exists()
					&& _fileChooser.getDialogType() == JFileChooser.SAVE_DIALOG) {
				int result = JOptionPane.showConfirmDialog(null,
						labels.getString("s954"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);
				if (result == JOptionPane.YES_OPTION) {
					a = true;
				}
				if (result == JOptionPane.NO_OPTION) {
					a = false;
					
					if (mainWindow.getMenu().isNPF()) {
						mainWindow.getEditorBuilder()
								.getPane()
								.remove(mainWindow.getEditorBuilder()
										.getSelectedEditorIndex());
						mainWindow.getEditorBuilder().getPane().validate();
					}
				}

			} else
				a = true;
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			_logger.info(labels.getString("s308"));
			a = false;

			if (mainWindow.getMenu().isNPF()) {
				mainWindow.getEditorBuilder().getPane()
						.remove(mainWindow.getEditorBuilder().getSelectedEditorIndex());
				mainWindow.getEditorBuilder().getPane().validate();
			}
		}
		if (a) {
			fileName = _fileChooser.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s307") + fileName);
			PropertiesManager.setProperty("DefaultPath", fileName);
		}

		return fileName;
	}

	/**
	 * 
	 * 
	 * @param fileName
	 *           
	 * @return 
	 * @throws IOException
	 */
	public String load(String fileName) {

		ResourceBundle labels = Language.getInstance().getLabels();
		try {
			BufferedReader reader;
			reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(fileName)));
			StringBuffer b = new StringBuffer("");
			String cad;

			while (true) {
				cad = reader.readLine();
				if (cad == null)
					break;
				b.append(cad + "\n");
			}
			reader.close();
			System.gc();
			return b.toString();

		} catch (IOException e) {
			e.printStackTrace();
			_logger.error(labels.getString("s309") + fileName);
			return null;
		}

	}

	/**
	 * 
	 * @param file
	 * @param string
	 *         
	 * @return
	 */
	public boolean save(String file, String string){
		
		ResourceBundle labels = Language.getInstance().getLabels();
		
		try {
			PrintWriter salida = new PrintWriter(new BufferedWriter(
					new FileWriter(file)));
			salida.print(string);
			salida.close();
			_logger.info(labels.getString("s310") + file);
			return true;
		}
		catch (java.io.IOException e) {
			_logger.error(labels.getString("s311") + file);
			return false;
		}
	}

	/**
	 * 
	 * @return
	 */
	public JFileChooser getFileChooser() {
		return _fileChooser;
	}
}
