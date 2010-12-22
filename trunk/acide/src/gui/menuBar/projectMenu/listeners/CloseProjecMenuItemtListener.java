package gui.menuBar.projectMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Close project menu item listener.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public class CloseProjecMenuItemtListener implements ActionListener {

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
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		boolean isCancelOptionSelected = false;

		// If the project has been modified
		if (MainWindow.getInstance().getProjectConfiguration().isModified()) {

			// Do you want to save it?
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (chosenOption == JOptionPane.OK_OPTION) {

				// Enables the save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);

				// Save the project
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();
			} else if (chosenOption != JOptionPane.NO_OPTION)
				isCancelOptionSelected = true;
		}

		if (!isCancelOptionSelected) {

			// Removes all the nodes in the explorer tree
			MainWindow.getInstance().getExplorerPanel().getRoot()
					.removeAllChildren();

			// Reloads the explorer tree model
			MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

			// Disables the add file menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getAddFile().setEnabled(false);

			// Disables the save project menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getSaveProject().setEnabled(false);

			// Disables the remove file menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(false);

			// Disables the delete file menu item in the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFile().setEnabled(false);

			// Save the main window parameters
			MainWindow.getInstance().getProjectConfiguration()
					.saveMainWindowParameters();

			// Sets the default title to the project
			MainWindow.getInstance().setTitle(
					labels.getString("s425") + " - <empty>");

			// Removes all the files related to the project
			MainWindow.getInstance().getProjectConfiguration().removeFiles();

			// Updates the MAIN WINDOW
			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();

			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty("defaultAcideProject",
					"./configuration/project/default.acidePrj");

			// Sets the project name as empty
			MainWindow.getInstance().getProjectConfiguration().setName("");

			// The project has not been modified yet
			MainWindow.getInstance().getProjectConfiguration()
					.setIsModified(false);

			// Enables the close all files menu item
			MainWindow.getInstance().getMenu().getFile().getCloseAllFiles()
					.setEnabled(true);

			// Close all files in the project
			MainWindow.getInstance().getMenu().getFile().getCloseAllFiles()
					.doClick();

			// Disables the project menu
			MainWindow.getInstance().getMenu().disableProjectMenu();

			// Disables the open all files menu item
			MainWindow.getInstance().getMenu().getFile().getOpenAllFiles().setEnabled(false);
			
			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage("");
		}
	}
}
