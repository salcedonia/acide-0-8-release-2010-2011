package gui.menuBar.fileMenu.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Save file menu item listener.
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
public class SaveFileMenuItemListener implements ActionListener {

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

		TextFile textFile = AcideIOFactory.getInstance().buildFile();
		String filePath = " ";

		// If there are opened files
		if (MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels() != 0) {

			// If it is not the NEW FILE
			if (!MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath()
					.equals(labels.getString("s79"))) {

				// Save the file
				boolean result = textFile.save(MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getAbsolutePath(), MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getTextEditionAreaContent());

				// If it could save it
				if (result) {

					// Updates the log
					AcideLog.getLog().info(
							labels.getString("s93") + filePath
									+ labels.getString("s94"));

					// Sets the green button
					MainWindow.getInstance().getFileEditorManager()
							.setGreenButton();

					// Saves the original file
					File explorerFile = new File(MainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath());

					// Sets the last change
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setLastChange(explorerFile.lastModified());

					// Sets the new length
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setLastSize(explorerFile.length());

					// Updates the file disk copy
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setFileDiskCopy(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getTextEditionAreaContent());
				} else {

					// Updates the log
					AcideLog.getLog().info(labels.getString("s95") + filePath);
				}
			} else {

				// Enables the save file as menu item
				MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
						.setEnabled(true);

				// Does the save file as menu item action
				MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
						.doClick();
			}
		} else
			// Updates the log
			AcideLog.getLog().info(labels.getString("s89"));
	}
}
