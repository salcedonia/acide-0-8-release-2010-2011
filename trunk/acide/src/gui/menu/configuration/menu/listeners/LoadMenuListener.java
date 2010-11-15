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
import es.configuration.menu.MenuConfiguration;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;
import gui.menu.configuration.menu.gui.MenuConfigurationWindow;

/************************************************************************																
 * Load menu menu item listener											
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
public class LoadMenuListener implements ActionListener {

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

		JFileChooser fileChooser = new JFileChooser();
		TextFileFilter filter = new TextFileFilter(labels.getString("s287"));
		filter.addExtension("menuCfg");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/menu/"));

		int chosenOption = fileChooser.showOpenDialog(null);
		if (chosenOption == JFileChooser.APPROVE_OPTION) {
			
			String menuFile = fileChooser.getSelectedFile().getAbsolutePath();
			boolean[] values = null;
			try {
				
				values = MenuConfiguration
						.loadMenuConfigurationFile(menuFile);
				PropertiesManager.setProperty("currentMenuConfiguration",
						menuFile);
				MenuConfiguration.setAll(values);
				
				MainWindow.getInstance().getMenu().buildMenu();
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				MainWindow.getInstance().getMenu().getConfiguration().getMenu().getSaveMenu().setEnabled(false);

				// Updates the log
				Log.getLog().info(labels.getString("s289"));

				MenuConfigurationWindow.setChangesSaved(true);

				// NOT DEFAULT PROJECT
				if (!MainWindow.getInstance().getProjectConfiguration()
						.isDefaultProject())
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null,
						labels.getString("s288") + " " + menuFile,
						labels.getString("289"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				Log.getLog().error(
						labels.getString("s288") + " " + menuFile);
			}
		} else
		// Cancel option
		if (chosenOption == JFileChooser.CANCEL_OPTION) {
			// Updates the log
			Log.getLog().info(labels.getString("s290"));
		}
	}
}