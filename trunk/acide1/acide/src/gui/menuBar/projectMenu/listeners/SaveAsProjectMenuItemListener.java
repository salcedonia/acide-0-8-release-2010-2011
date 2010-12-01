package gui.menuBar.projectMenu.listeners;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Save as menu item listener.
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
public class SaveAsProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

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
			ResourceBundle labels = language.getLabels();
			
			TextFile f = AcideIOFactory.getInstance().buildFile();

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Selects the project extension
				String[] ExtPide = new String[] { "acidePrj" };
				f.getFileChooser().addChoosableFileFilter(
						new ExtensionFilter(ExtPide, labels
								.getString("s328")));

				String file = f.write();

				// Sets the language
				MainWindow.getInstance().getProjectConfiguration()
						.setLanguage(
								ResourceManager.getInstance().getProperty("language"));

				// Sets the menu
				MainWindow.getInstance().getProjectConfiguration().setMenu(
						ResourceManager
								.getInstance().getProperty("currentMenuConfiguration"));

				// Sets the tool bar
				MainWindow
						.getInstance()
						.getProjectConfiguration()
						.setToolBar(
								ResourceManager
										.getInstance().getProperty("currentToolBarConfiguration"));

				// Add the extension if the name does not contain it
				if (!file.contains(".acidePrj"))
					file = file + ".acidePrj";

				// Sets the path
				MainWindow.getInstance().getProjectConfiguration().setPath(
						file);

				// Saves the file
				String cad = MainWindow.getInstance()
						.getProjectConfiguration().save();
				f.save(MainWindow.getInstance().getProjectConfiguration()
						.getProjectPath(), cad);

				// Is the first time that the project has been saved
				MainWindow.getInstance().getProjectConfiguration()
						.setFirstSave(true);
				
				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty("defaultAcideProject", file);
				ResourceManager.getInstance().setProperty("defaultPath", file);

				// The project has not been modified yet
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(false);
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
