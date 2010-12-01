package gui.menuBar.configurationMenu.menuMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.bytes.ByteFile;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.menuMenu.gui.MenuConfigurationWindow;

/************************************************************************																
 * Save as menu menu item listener.											
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
public class SaveAsMenuMenuItemListener implements ActionListener {

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
			String currentMenu = ResourceManager
					.getInstance().getProperty("currentMenuConfiguration");
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
				
				// Updates the RESOURCES MANAGER
				ResourceManager.getInstance().setProperty("currentMenuConfiguration",
						fileName);
				
				MainWindow.getInstance().getMenu().getConfiguration().getMenu().getSaveMenu().setEnabled(false);
				MenuConfigurationWindow.setChangesAreSaved(true);

				// Updates the log
				AcideLog.getLog().info(
						labels.getString("s528") + fileName
								+ labels.getString("s529"));

			} else if (chosenOption == JFileChooser.CANCEL_OPTION) {

				fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(labels.getString("s527"));
			}
		} catch (Exception exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s291"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
