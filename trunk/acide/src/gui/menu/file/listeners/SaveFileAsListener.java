package gui.menu.file.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Save file as menu item listener											
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
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see ActionListener																													
 ***********************************************************************/
public class SaveFileAsListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

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

		TextFile textFile = IOFactory.getInstance().buildFile();
		String f = " ";

		// If there are opened files
		if (MainWindow.getInstance().getEditorManager().getNumEditors() != 0) {

			f = textFile.write();

			// If the file is not empty
			if (!f.equals(" ")) {

				boolean result = textFile.save(f, MainWindow.getInstance()
						.getEditorManager().getSelectedEditor().getText());

				// If it could save it
				if (result) {
					
					// Updates the log
					Log.getLog().info(labels.getString("s93") + f
							+ labels.getString("s94"));
					
					// Sets the green button
					MainWindow.getInstance().getEditorManager()
							.setGreenButton();
					
					// Gets the file name
					int index = f.lastIndexOf("\\");
					if(index == -1)
						index = f.lastIndexOf("/");
					index++;
					String file = f.substring(index, f.length());
					
					// Sets the title
					MainWindow.getInstance().getEditorManager().getPane()
							.setTitleAt(
									MainWindow.getInstance()
											.getEditorManager().getPane()
											.getSelectedIndex(), file);
					
					// Sets the file path
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setAbsolutePath(f);
					
					// Sets the tool tip text
					MainWindow.getInstance().getEditorManager().getPane()
							.setToolTipText(f);

					// Saves the original file
					File explorerFile = new File(MainWindow.getInstance()
							.getEditorManager().getSelectedEditor()
							.getAbsolutePath());
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setLastChange(
									explorerFile.lastModified());
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setLastSize(
									explorerFile.length());

				} else
					// Updates the log
					Log.getLog().info(labels.getString("s95") + f);
			} else
				// Updates the log
				Log.getLog().info(labels.getString("s92"));
		} else
			// Updates the log
			Log.getLog().info(labels.getString("s89"));
	}
}
