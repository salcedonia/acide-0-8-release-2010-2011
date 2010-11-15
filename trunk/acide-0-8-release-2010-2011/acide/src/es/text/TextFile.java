package es.text;

import gui.mainWindow.MainWindow;

import javax.swing.*;

import operations.log.Log;

import properties.PropertiesManager;

import java.io.*;
import java.util.ResourceBundle;

import language.Language;

/************************************************************************																
 * Handles the text files of ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan Jos� Ortiz S�nchez										
 *         </ul>														
 *         <ul>															
 *         Delf�n Rup�rez Ca�as											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Mart�n L�zaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo G�mez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8																														
 ***********************************************************************/
public class TextFile {

	/**
	 * File chooser
	 */
	private JFileChooser _fileChooser;

	/**
	 * Class constructor
	 */
	public TextFile() {
		_fileChooser = new JFileChooser();
	}

	/**
	 * Returns the text file name 
	 * 
	 * @return the text file name
	 */
	public String read() {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		String fileName = null;
		String text = null;
		File file = null;

		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setDialogTitle(labels.getString("s9"));
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = _fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			Log.getLog().info(labels.getString("s300") + fileName);
			
			PropertiesManager.setProperty("defaultPath", fileName);

		} else if (value == JFileChooser.CANCEL_OPTION) {
			
			_fileChooser.cancelSelection();
			
			// Updates the log
			Log.getLog().info(labels.getString("s302"));
		}
		return fileName;

	}

	/**
	 * Returns the text file path selected in a file chooser
	 * 
	 * @return the text file path selected in a file chooser
	 */
	public String readPath() {

		// Gets the language TO DISPLAY
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		String path = " ";
		String text = null;
		File file = null;

		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {
			path = _fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			Log.getLog().info(labels.getString("s303") + path);
			
			PropertiesManager.setProperty("defaultPath", path);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			
			_fileChooser.cancelSelection();
			
			// Updates the log
			Log.getLog().info(labels.getString("s304"));
		}
		return path;
	}

	/**
	 * Returns the text file name
	 * 
	 * @param filter
	 *            filter for the text files
	 * 
	 * @return the text file name
	 */
	public String read(TextFileFilter filter) {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		JFileChooser fileChooser = new JFileChooser();
		String fileName = " ";
		String text = null;
		File file = null;
		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		int value = fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			Log.getLog().info(labels.getString("s305") + fileName);
			
			PropertiesManager.setProperty("defaultPath", fileName);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			
			fileChooser.cancelSelection();
			
			// Updates the log
			Log.getLog().info(labels.getString("s306"));
		}
		return fileName;
	}

	/**
	 * Writes on a text file
	 * 
	 * @return the absolute file path
	 */
	public String write() {

		// Gets the language TO DISPLAY
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		String absoluteFilePath = " ";
		String text = null;
		File file = null;
		
		MainWindow mainWindow = MainWindow.getInstance();
		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file);
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		boolean isApproved = true;

		int value = _fileChooser.showSaveDialog(_fileChooser);
		File selectedFile = _fileChooser.getSelectedFile();

		// ASK THE USER IF WANTS TO SAVE IT
		if (value == JFileChooser.APPROVE_OPTION) {
			
			// IF EXISTS
			if (selectedFile.exists()
					&& _fileChooser.getDialogType() == JFileChooser.SAVE_DIALOG) {
				int result = JOptionPane.showConfirmDialog(null,
						labels.getString("s954"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);
				if (result == JOptionPane.YES_OPTION) {
					isApproved = true;
				}
				if (result == JOptionPane.NO_OPTION) {
					isApproved = false;

					if (mainWindow.getMenu().isNPF()) {
						mainWindow
								.getEditorManager()
								.getPane()
								.remove(mainWindow.getEditorManager()
										.getSelectedEditorIndex());
						mainWindow.getEditorManager().getPane().validate();
					}
				}

			} else
				isApproved = true;
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			
			// Updates the log
			Log.getLog().info(labels.getString("s308"));
			
			isApproved = false;

			if (mainWindow.getMenu().isNPF()) {
				mainWindow
						.getEditorManager()
						.getPane()
						.remove(mainWindow.getEditorManager()
								.getSelectedEditorIndex());
				mainWindow.getEditorManager().getPane().validate();
			}
		}
		if (isApproved) {
			
			absoluteFilePath = _fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			Log.getLog().info(labels.getString("s307") + absoluteFilePath);
			
			PropertiesManager.setProperty("defaultPath", absoluteFilePath);
		}

		return absoluteFilePath;
	}

	/**
	 * Loads the text file content into a string
	 * 
	 * @param fileName
	 *            file name
	 * @return the file content
	 * @throws IOException
	 */
	public String load(String fileName) {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
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

		} catch (IOException exception) {
			
			// Updates the log
			Log.getLog().error(labels.getString("s309") + fileName);
			exception.printStackTrace();
			
			return null;
		}

	}

	/**
	 * Saves the file content given as a parameter into a file given as a
	 * parameter as well
	 * 
	 * @param file
	 *            file to save in
	 * @param fileContent
	 *            file content to store into the file
	 * 
	 * @return true if the operation was succeed and false in other case
	 */
	public boolean save(String file, String fileContent) {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		try {
			PrintWriter salida = new PrintWriter(new BufferedWriter(
					new FileWriter(file)));
			salida.print(fileContent);
			salida.close();
			
			// Updates the log
			Log.getLog().info(labels.getString("s310") + file);
			
			return true;
		} catch (IOException exception) {
			
			// Updates the log
			Log.getLog().error(labels.getString("s311") + file);
			exception.printStackTrace();
				
			return false;
		}
	}

	/**
	 * Returns the file chooser
	 * 
	 * @return the file chooser
	 */
	public JFileChooser getFileChooser() {
		return _fileChooser;
	}
}
