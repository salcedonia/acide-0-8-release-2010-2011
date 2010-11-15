package gui.menu.file.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.undo.UndoableEdit;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Open file menu item listener											
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
public class OpenFileListener implements ActionListener {

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

		TextFile textFile = IOFactory.getInstance().buildFile();
		String filePath = " ";
		filePath = textFile.read();

		// If the file exists
		if (filePath != null) {

			boolean isOpened = false;

			// Checks if the file is already opened
			int fileIndex = -1;
			for (int position = 0; position < MainWindow.getInstance().getEditorManager()
					.getNumEditors(); position++) {
				if (MainWindow.getInstance().getEditorManager()
						.getEditorAt(position).getAbsolutePath().equals(filePath)) {
					isOpened = true;
					fileIndex = position;
				}
			}

			// If it is not opened
			if (!isOpened) {

				String text = null;
				text = textFile.load(filePath);

				// If the text is not empty
				if (text != null) {

					// Searches for the file into the project configuration file list
					int fileProjectIndex = -1;
					for (int pos = 0; pos < MainWindow.getInstance()
							.getProjectConfiguration()
							.getNumFilesFromList(); pos++) {
						if (MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(pos)
								.getPath().equals(filePath))
							fileProjectIndex = pos;
					}

					// If belongs to the project
					if (fileProjectIndex > -1) {

						int type = 0;

						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(fileProjectIndex)
										.getPath());

						// Is COMPILABLE FILE?
						if (MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(
										fileProjectIndex)
								.isCompilableFile()) {
							type = 2;

							// Updates the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow
													.getInstance()
													.getProjectConfiguration()
													.getFileAt(
															fileProjectIndex)
													.getPath()
													+ " <COMPILABLE>");
						}

						// Is MAIN FILE?
						if (MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(
										fileProjectIndex).isMainFile()) {
							type = 1;

							// Updates the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow
													.getInstance()
													.getProjectConfiguration()
													.getFileAt(
															fileProjectIndex)
													.getPath()
													+ " <MAIN>");
						}

						// Opens a new tab
						MainWindow.getInstance().getEditorManager().newTab(
								filePath, filePath, text, true, type);
					} else {

						// If it does not belong to the project

						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setMessage(
								filePath);

						// Opens a new tab with the content
						MainWindow.getInstance().getEditorManager().newTab(
								filePath, filePath, text, true, 0);
					}

					// Updates the log
					Log.getLog().info(labels.getString("s84") + filePath);
					Log.getLog().info(labels.getString("s85") + filePath
							+ labels.getString("s86"));

					// UNDO REDO
					MainWindow.getInstance().getMenu().enableFileMenu();
					MainWindow.getInstance().getMenu().enableEditMenu();
					DefaultStyledDocument document = MainWindow.getInstance()
							.getEditorManager().getSelectedEditor()
							.getSyntaxDocument();

					document.addUndoableEditListener(new UndoableEditListener() {

						/*
						 * (non-Javadoc)
						 * 
						 * @seejavax.swing.event.UndoableEditListener#
						 * undoableEditHappened
						 * (javax.swing.event.UndoableEditEvent)
						 */
						@Override
						public void undoableEditHappened(
								UndoableEditEvent undoableEditEvent) {

							UndoableEdit edit = undoableEditEvent.getEdit();

							if (edit instanceof DefaultDocumentEvent
									&& ((DefaultDocumentEvent) edit)
											.getType() == DefaultDocumentEvent.EventType.CHANGE) {
								return;
							} else {
								MainWindow.getInstance().getMenu()
										.getEdit().getUndoManager()
										.addEdit(undoableEditEvent.getEdit());
							}
						}
					});

					// Sets the caret in the first position of the editor
					MainWindow.getInstance().getEditorManager().getSelectedEditor().getEditor().setCaretPosition(0);

					// Sets the new file state to opened
					for (int filePosition = 0; filePosition < MainWindow
							.getInstance().getProjectConfiguration()
							.getFileListSize(); filePosition++) {
						if (MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(
										filePosition).getPath().equals(
										filePath)) {
							MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(
											filePosition).setIsOpened(true);
						}
					}

					// Not default project
					if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
						
						// The project has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);

				} else {

					// EMPTY FILE
					
					// Updates the log
					Log.getLog().info(labels.getString("s88"));
				}

			} else {

				// Puts the focus in the opened editor
				MainWindow.getInstance().getEditorManager()
						.setSelectedEditorAt(fileIndex);
			}
		} else

			// FILE DOESN'T EXISTS
			
			// Updates the log
			Log.getLog().info(labels.getString("s83"));
	}
}