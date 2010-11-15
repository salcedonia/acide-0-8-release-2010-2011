package gui.menu.configuration.menu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;
import es.bytes.ByteFile;
import gui.mainWindow.MainWindow;
import gui.menu.configuration.menu.gui.MenuConfigurationWindow;

/************************************************************************																
 * Save menu menu item listener											
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
public class SaveMenuListener implements ActionListener {

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
			String previous = PropertiesManager
					.getProperty("previousMenuConfiguration");
			String current = PropertiesManager
					.getProperty("currentMenuConfiguration");
			ByteFile.copy(current, previous);
			PropertiesManager.setProperty("currentMenuConfiguration",
					previous);
			MainWindow.getInstance().getMenu().getConfiguration().getMenu().getSaveMenu().setEnabled(false);
			MenuConfigurationWindow.setChangesSaved(true);
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s293"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			Log.getLog().error(exception.getMessage());
		}
	}
}
