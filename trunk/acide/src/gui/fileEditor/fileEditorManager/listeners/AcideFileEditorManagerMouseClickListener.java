package gui.fileEditor.fileEditorManager.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;

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

		// If there are opened editors
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getComponentCount() != 0) {

			String selectedEditorPath = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
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

					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex() != -1) {

						// Gets the active text pane
						JTextPane activeTextPane = MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										MainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex())
								.getActiveTextEditionArea();

						// Sets the caret position at its position
						activeTextPane.setCaretPosition(activeTextPane
								.getCaretPosition());

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

				if ((f.lastModified() != MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getLastChange())
						|| (f.length() != MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getLastSize())) {

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

					// Ask to the user for saving 
					int chosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s65"));

					// OK OPTION
					if (chosenOption == JOptionPane.OK_OPTION) {

						TextFile newTextFile = new TextFile();
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.loadText(newTextFile.load(selectedEditorPath));
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(f.lastModified());
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(f.length());
					} else {
						
						// NO OPTION
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(f.lastModified());
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(f.length());
					}
				}
			}
		}

		// Updates the status bar with the type of file selected in the file
		// editor
		MainWindow.getInstance().getStatusBar()
				.updatesStatusBarFromFileEditor();

		// Selects the node in the tree that matches with the clicked file
		// editor
		MainWindow.getInstance().getExplorerPanel().selectTreeNodeFromFileEditor();

		// Updates the button icons
		MainWindow.getInstance().getFileEditorManager().updatesButtonIcons();
	}
}
