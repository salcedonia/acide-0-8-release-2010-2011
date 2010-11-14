package gui.editor.editorManager.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.editor.editorManager.EditorManager;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.tree.TreePath;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Editor builder mouse click listener										
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
 * @see MouseAdapter																													
 ***********************************************************************/
public class EditorManagerMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		EditorManager editorManager = MainWindow.getInstance().getEditorManager();
		
		// If there are opened editors
		if (editorManager.getPane().getComponentCount() != 0) {

			String selectedEditorPath = editorManager.getSelectedEditor()
					.getAbsolutePath();

			// If has path
			if (selectedEditorPath != null) {

				File f = new File(selectedEditorPath);

				if ((f.lastModified() != editorManager.getSelectedEditor()
						.getLastChange())
						|| (f.length() != editorManager.getSelectedEditor().getLastSize())) {

					// Gets the language
					Language language = Language.getInstance();

					try {
						language.getLanguage(PropertiesManager
								.getProperty("language"));
					} catch (Exception exception) {
						
						// Updates the log
						Log.getLog().error(exception.getMessage());
						exception.printStackTrace();
					}

					// Gets the labels
					ResourceBundle labels = language.getLabels();

					int chosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s65"));

					if (chosenOption == 0) {

						TextFile newTextFile = new TextFile();
						editorManager.getSelectedEditor().loadText(
								newTextFile.load(selectedEditorPath));
						editorManager.getSelectedEditor().setLastChange(f.lastModified());
						editorManager.getSelectedEditor().setLastSize(f.length());
					} else {
						editorManager.getSelectedEditor().setLastChange(f.lastModified());
						editorManager.getSelectedEditor().setLastSize(f.length());
					}
				}
			}
		}

		if (editorManager.getSelectedEditor() != null) {

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().getAbsolutePath());

			for (int i = 0; i < MainWindow.getInstance()
					.getProjectConfiguration().getNumFilesFromList(); i++) {

				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(i).getPath().equals(
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditor()
										.getAbsolutePath()))

					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).isCompilableFile())

						if (MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(i)
								.isMainFile())

							// MAIN FILE
							MainWindow.getInstance().getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getEditorManager()
													.getSelectedEditor()
													.getAbsolutePath()
													+ " <MAIN>");
						else

							// COMPILABLE FILE
							MainWindow.getInstance().getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getEditorManager()
													.getSelectedEditor()
													.getAbsolutePath()
													+ " <COMPILABLE>");
					else

						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditor()
										.getAbsolutePath());
			}

			// Default configuration
			if (MainWindow.getInstance()
					.getProjectConfiguration().isDefaultProject()) {

				// Checks the type
				if (MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().isCompilerFile())
					
					if (MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().isMainFile())

						// MAIN FILE
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditor()
										.getAbsolutePath()
										+ " <MAIN>");
					else

						// COMPILABLE FILE
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditor()
										.getAbsolutePath()
										+ " <COMPILABLE>");
				else
					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							MainWindow.getInstance().getEditorManager()
									.getSelectedEditor().getAbsolutePath());
			}
		}

		// Puts the selected file in the editor if there
		// is not an opened project

		// Not default project
		if (!MainWindow.getInstance()
				.getProjectConfiguration().isDefaultProject()) {

			// Editor selected?
			if (editorManager.getSelectedEditor() != null) {

				ExplorerFile f = new ExplorerFile();
				int y = -1;

				// Searches for the file in the explorer tree
				for (int j = 0; j < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); j++) {

					// Does the file belong to the project?
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(j).getPath().equals(
									MainWindow.getInstance()
											.getEditorManager()
											.getSelectedEditor()
											.getAbsolutePath())) {

						f = MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(j);
						
						for (int m = 0; m < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList() + 1; m++) {

							if (MainWindow.getInstance().getExplorer()
									.getTree().getPathForRow(m)
									.getLastPathComponent().toString()
									.equals(f.getLastPathComponent())) {

								y = m;
							}
						}
					}
				}

				TreePath currentSelection = MainWindow.getInstance()
						.getExplorer().getTree().getPathForRow(y);
				MainWindow.getInstance().getExplorer().getTree()
						.setSelectionPath(currentSelection);
			}
		}

		// Updates the button icons
		if (MainWindow.getInstance().getEditorManager().getNumEditors() > 0) {

			int index = MainWindow.getInstance().getEditorManager()
					.getSelectedEditorIndex();

			ImageIcon icon = (ImageIcon) editorManager.getPane().getIconAt(index);
			MainWindow.getInstance().getEditorManager().getPane().setIcon(
					icon);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {

		EditorManager editorManager = MainWindow.getInstance().getEditorManager();
		
		// If there are opened editors
		if (MainWindow.getInstance().getEditorManager().getNumEditors() > 0) {

			int index = MainWindow.getInstance().getEditorManager()
					.getSelectedEditorIndex();
			
			ImageIcon icon = (ImageIcon) editorManager.getPane().getIconAt(index);
			MainWindow.getInstance().getEditorManager().getPane().setIcon(
					icon);
		}
	}
}
