package gui.menu.project.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Save project menu item listener											
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
public class SaveProjectListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		TextFile textFile = IOFactory.getInstance().buildFile();

		try {

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// IF THIS IS NOT THE FIRST TIME THAT THE PROJECT IS SAVED
				// THEN SAVE IT AS
				if (!MainWindow.getInstance().getProjectConfiguration()
						.isFirstSave()) {

					MainWindow.getInstance().getMenu().getProject()
							.getSaveAsProject().setEnabled(true);
					MainWindow.getInstance().getMenu().getProject()
							.getSaveAsProject().doClick();
				} else {

					// Sets the language
					MainWindow.getInstance().getProjectConfiguration()
							.setLanguage(
									PropertiesManager
											.getProperty("language"));

					// Sets the menu
					MainWindow
							.getInstance()
							.getProjectConfiguration()
							.setMenu(
									PropertiesManager
											.getProperty("currentMenuConfiguration"));

					// Sets the tool bar
					MainWindow
							.getInstance()
							.getProjectConfiguration()
							.setToolBar(
									PropertiesManager
											.getProperty("currentToolBarConfiguration"));

					// Sets the grammar
					MainWindow.getInstance().getProjectConfiguration()
							.setSyntacticConfiguration(
									PropertiesManager
											.getProperty("currentGrammar"));

					// Sets the lexicon
					MainWindow.getInstance().getProjectConfiguration()
							.setLexicalConfiguration(
									PropertiesManager
											.getProperty("languagePath"));

					// Sets the shell configuration
					MainWindow.getInstance().getProjectConfiguration()
					.setOutputConfiguration(
							PropertiesManager
									.getProperty("outputConfiguration"));

					// Saves the configuration into the file
					String fileContent = MainWindow.getInstance()
							.getProjectConfiguration().save();
					textFile.save(MainWindow.getInstance()
							.getProjectConfiguration().getProjectPath(),
							fileContent);

					// The project has not been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(false);
				}
			}
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
