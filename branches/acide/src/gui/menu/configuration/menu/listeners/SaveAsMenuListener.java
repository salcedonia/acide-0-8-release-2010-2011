package gui.menu.configuration.menu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;
import es.bytes.ByteFile;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;
import gui.menu.configuration.menu.gui.MenuConfigurationWindow;

/************************************************************************																
 * Save as menu menu item listener											
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
public class SaveAsMenuListener implements ActionListener {

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
		final ResourceBundle labels = language.getLabels();

		try {
			String currentMenu = PropertiesManager
					.getProperty("currentMenuConfiguration");
			JFileChooser fileChooser = new JFileChooser();
			TextFileFilter filter = new TextFileFilter(
					labels.getString("s126"));
			filter.addExtension("menuCfg");
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(new File("./configuration/menu/"));
			
			String fileName = "";
			int chosenOption = fileChooser.showSaveDialog(fileChooser);
			if (chosenOption == JFileChooser.APPROVE_OPTION) {
				fileName = fileChooser.getSelectedFile()
						.getAbsolutePath();
				if (!fileName.endsWith(".menuCfg"))
					fileName += ".menuCfg";
				ByteFile.copy(currentMenu, fileName);
				PropertiesManager.setProperty("currentMenuConfiguration",
						fileName);
				MainWindow.getInstance().getMenu().getConfiguration().getMenu().getSaveMenu().setEnabled(false);
				MenuConfigurationWindow.setChangesSaved(true);

				// Updates the log
				Log.getLog().info(
						labels.getString("s528") + fileName
								+ labels.getString("s529"));

			} else if (chosenOption == JFileChooser.CANCEL_OPTION) {

				fileChooser.cancelSelection();

				// Updates the log
				Log.getLog().info(labels.getString("s527"));
			}
		} catch (Exception exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s291"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
		}
	}
}
