package gui.fileEditor.fileEditorManager.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.tree.TreePath;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * ACIDE - A Configurable IDE file editor manager mouse click listener.
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
 * @see MouseAdapter
 ***********************************************************************/
public class AcideFileEditorManagerMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		mouseEventListener(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		mouseEventListener(mouseEvent);
	}

	/**
	 * Mouse event listener method to handle the mouse events.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void mouseEventListener(MouseEvent mouseEvent) {

		final AcideFileEditorManager editorManager = MainWindow.getInstance()
				.getFileEditorManager();

		// If there are opened editors
		if (editorManager.getTabbedPane().getComponentCount() != 0) {

			String selectedEditorPath = editorManager.getSelectedFileEditorPanel()
					.getAbsolutePath();

			// Sets the focus on the selected editor
			SwingUtilities.invokeLater(new Runnable() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see java.lang.Runnable#run()
				 */
				@Override
				public void run() {

					if (editorManager.getSelectedFileEditorPanelIndex() != -1) {

						// Gets the active text pane
						JTextPane activeTextPane = editorManager
						.getFileEditorPanelAt(
								editorManager.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea();
						
						// Sets the caret position at its position
						activeTextPane.setCaretPosition(
										activeTextPane.getCaretPosition());

						// Sets the caret visible
						activeTextPane.getCaret().setVisible(true);
						
						// Sets the focus on the active text component
						activeTextPane.requestFocusInWindow();
					}
				}
			});

			// If has path
			if (selectedEditorPath != null) {

				File f = new File(selectedEditorPath);

				if ((f.lastModified() != editorManager.getSelectedFileEditorPanel()
						.getLastChange())
						|| (f.length() != editorManager.getSelectedFileEditorPanel()
								.getLastSize())) {

					// Gets the language
					AcideLanguage language = AcideLanguage.getInstance();

					try {
						language.getLanguage(ResourceManager.getInstance()
								.getProperty("language"));
					} catch (Exception exception) {

						// Updates the log
						AcideLog.getLog().error(exception.getMessage());
						exception.printStackTrace();
					}

					// Gets the labels
					ResourceBundle labels = language.getLabels();

					int chosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s65"));

					if (chosenOption == 0) {

						TextFile newTextFile = new TextFile();
						editorManager.getSelectedFileEditorPanel().loadText(
								newTextFile.load(selectedEditorPath));
						editorManager.getSelectedFileEditorPanel().setLastChange(
								f.lastModified());
						editorManager.getSelectedFileEditorPanel().setLastSize(
								f.length());
					} else {
						editorManager.getSelectedFileEditorPanel().setLastChange(
								f.lastModified());
						editorManager.getSelectedFileEditorPanel().setLastSize(
								f.length());
					}
				}
			}
		}

		if (editorManager.getSelectedFileEditorPanel() != null) {

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath());

			for (int i = 0; i < MainWindow.getInstance()
					.getProjectConfiguration().getNumFilesFromList(); i++) {

				if (MainWindow
						.getInstance()
						.getProjectConfiguration()
						.getFileAt(i)
						.getPath()
						.equals(MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath()))

					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).isCompilableFile())

						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).isMainFile())

							// MAIN FILE
							MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getAbsolutePath()
													+ " <MAIN>");
						else

							// COMPILABLE FILE
							MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getAbsolutePath()
													+ " <COMPILABLE>");
					else

						// Updates the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath());
			}

			// Default configuration
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Checks the type
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilerFile())

					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isMainFile())

						// MAIN FILE
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath()
												+ " <MAIN>");
					else

						// COMPILABLE FILE
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath()
												+ " <COMPILABLE>");
				else
					// Updates the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());
			}
		}

		// Puts the selected file in the editor if there
		// is not an opened project

		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isDefaultProject()) {

			// Editor selected?
			if (editorManager.getSelectedFileEditorPanel() != null) {

				ExplorerFile f = new ExplorerFile();
				int index = -1;

				// Searches for the file in the explorer tree
				for (int position = 0; position < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); position++) {

					// Does the file belong to the project?
					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(position)
							.getPath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager().getSelectedFileEditorPanel()
									.getAbsolutePath())) {

						f = MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(position);

						for (int m = 0; m < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList() + 1; m++) {

							if (MainWindow.getInstance().getExplorer()
									.getTree().getPathForRow(m)
									.getLastPathComponent().toString()
									.equals(f.getLastPathComponent())) {

								index = m;
							}
						}
					}
				}

				TreePath currentSelection = MainWindow.getInstance()
						.getExplorer().getTree().getPathForRow(index);
				MainWindow.getInstance().getExplorer().getTree()
						.setSelectionPath(currentSelection);
			}
		}

		// Updates the button icons
		if (MainWindow.getInstance().getFileEditorManager().getNumFileEditorPanels() > 0) {

			int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			ImageIcon icon = (ImageIcon) editorManager.getTabbedPane()
					.getIconAt(selectedEditorIndex);
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.setIcon(icon);
		}
	}
}
