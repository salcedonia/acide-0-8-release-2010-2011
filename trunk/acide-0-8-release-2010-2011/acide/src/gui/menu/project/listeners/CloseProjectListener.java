package gui.menu.project.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Close project menu item listener											
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
public class CloseProjectListener implements ActionListener {

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
		
		boolean cancelSelected = false;

		// If the project has been modified
		if (MainWindow.getInstance().getProjectConfiguration().isModified()) {
			
			// Do you want to save it?
			int chosenOption = JOptionPane.showConfirmDialog(null, labels
					.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);
			
			// If cancel
			if (chosenOption == JOptionPane.CANCEL_OPTION) {
				cancelSelected = true;
			}
			
			// If yes
			if (chosenOption == JOptionPane.OK_OPTION) {
				MainWindow.getInstance().getMenu().getProject().getSaveProject()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getProject().getSaveProject()
						.doClick();
			}
		}
		if (!cancelSelected) {
			
			MainWindow.getInstance().getExplorer().getRoot().removeAllChildren();
			MainWindow.getInstance().getExplorer().getTreeModel().reload();
			MainWindow.getInstance().getExplorer().getPopupMenu().getAddFile()
					.setEnabled(false);
			MainWindow.getInstance().getExplorer().getPopupMenu().getSaveProject()
					.setEnabled(false);
			MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
					.setEnabled(false);
			MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
					.setEnabled(false);
			MainWindow.getInstance().getProjectConfiguration().saveMainWindowParameters();
			MainWindow.getInstance().setTitle(labels.getString("s425") + " - <empty>");
			MainWindow.getInstance().getProjectConfiguration().removeFiles();
			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();
			
			// Saves the default configuration
			PropertiesManager.setProperty("defaultAcideProject",
					"./configuration/project/default.acidePrj");
			MainWindow.getInstance().getProjectConfiguration().setName("");
			MainWindow.getInstance().getProjectConfiguration().setIsModified(false);
			MainWindow.getInstance().getMenu().getFile().getCloseAllFiles().setEnabled(
					true);
			MainWindow.getInstance().getMenu().getFile().getCloseAllFiles().doClick();
			MainWindow.getInstance().getMenu().disableProjectMenu();
			MainWindow.getInstance().getStatusBar().setMessage("");
		}
	}
}
