package gui.menuBar.configurationMenu.toolBarMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.bytes.ByteFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.ToolBarConfigurationWindow;

/************************************************************************																
 * Save tool bar listener.											
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
 * @see ActionListener																														
 ***********************************************************************/
public class SaveToolBarMenuItemListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		
		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		try {
			String previous = ResourceManager
					.getInstance().getProperty("previousToolBarConfiguration");
			String current = ResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");
			ByteFile.copy(current, previous);
			
			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", previous);
			
			MainWindow.getInstance().getMenu().getConfiguration().getToolBar().getSaveToolBar().setEnabled(false);
			ToolBarConfigurationWindow.setAreChangesSaved(true);
		} catch (Exception exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s299"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}