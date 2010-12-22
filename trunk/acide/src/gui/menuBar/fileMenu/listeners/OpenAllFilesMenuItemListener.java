package gui.menuBar.fileMenu.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import language.AcideLanguage;

import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Open file menu item listener.
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
public class OpenAllFilesMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
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
		
		for (int index = 0; index < MainWindow.getInstance()
				.getProjectConfiguration().getNumFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(MainWindow.getInstance()
					.getProjectConfiguration().getFileAt(index).getPath());

			// If the file is not a directory and exists
			if (!MainWindow.getInstance().getProjectConfiguration()
					.getFileAt(index).isDirectory()
					&& file.exists()) {

				TextFile textFile = AcideIOFactory.getInstance().buildFile();
				String text = null;
				text = textFile.load(MainWindow.getInstance()
						.getProjectConfiguration().getFileAt(index).getPath());

				String fileName = null;
				String filePath = MainWindow.getInstance()
						.getProjectConfiguration().getFileAt(index).getPath();

				// Gets the file name
				if (filePath != null) {

					int lastIndexOfSlash = filePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePath.lastIndexOf("/");
					fileName = filePath.substring(lastIndexOfSlash + 1,
							filePath.length());
				}

				// Updates the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setMessage(
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(index).getPath());

				// Check if it is a MAIN or COMPILABLE FILE
				int fileType = 0;

				// COMPILABLE
				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(index).isCompilableFile()) {

					fileType = 2;

					// Adds the <COMPILABLE> tag in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index).getPath()
											+ " <COMPILABLE>");
				}

				// MAIN
				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(index).isMainFile()) {

					fileType = 1;

					// Adds the <MAIN> tag in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index).getPath()
											+ " <MAIN>");
				}

				// Opens a new tab in the editor
				MainWindow.getInstance().getFileEditorManager()
						.newTab(fileName, filePath, text, true, fileType);

				// Checks if it is marked as a MAIN or COMPILABLE FILE
				for (int i = 0; i < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); i++) {

					if (MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(i)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(index)
									.getPath())) {

						// IS COMPILABLE FILE?
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(index).isCompilableFile())
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(i)
									.setCompilerFile(true);

						// IS MAIN FILE?
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(index).isMainFile())
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(i).setMainFile(true);
					}
				}

				// Enables the file menu
				MainWindow.getInstance().getMenu().enableFileMenu();

				// Enables the edit menu
				MainWindow.getInstance().getMenu().enableEditMenu();

				// Updates the undo manager
				AcideUndoRedoManager.getInstance().update();

				// The project configuration has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(false);
			} else {

				// If the file does not exist
				if (!file.exists()) {

					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s970")
									+ MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index).getPath()
									+ labels.getString("s971"), "Error",
							JOptionPane.ERROR_MESSAGE);

					// Removes the file from the project
					MainWindow.getInstance().getProjectConfiguration()
							.removeFileAt(index);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				}
			}
		}

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// Sets the selected editor
				if (MainWindow.getInstance().getProjectConfiguration()
						.getSelectedEditorIndex() != -1) {

					// Sets the selected file editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getSelectedEditorIndex());

					// Sets the focus in the edition area
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().requestFocusInWindow();

					// Selects the tree node
					MainWindow.getInstance().getExplorerPanel()
							.selectTreeNodeFromFileEditor();

					// Updates the status bar with the selected editor
					MainWindow.getInstance().getStatusBar()
							.updatesStatusBarFromFileEditor();
				}
			}
		});
	}

}
