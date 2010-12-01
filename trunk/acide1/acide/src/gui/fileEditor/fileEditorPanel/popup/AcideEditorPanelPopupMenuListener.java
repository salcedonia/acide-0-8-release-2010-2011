package gui.fileEditor.fileEditorPanel.popup;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/************************************************************************
 * Editor panel popup menu listener.
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
public class AcideEditorPanelPopupMenuListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/**
	 * Shows the popup menu.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void maybeShowPopup(MouseEvent mouseEvent) {

		if (mouseEvent.isPopupTrigger()) {

			AcideFileEditorPanel selectedEditor = MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel();
			
			selectedEditor.getPopupMenu().getCopy().setEnabled(false);
			selectedEditor.getPopupMenu().getCut().setEnabled(false);
			selectedEditor.getPopupMenu().getPaste().setEnabled(false);
			selectedEditor.getPopupMenu().getAddFile().setEnabled(false);
			selectedEditor.getPopupMenu().getRemoveFile().setEnabled(false);
			selectedEditor.getPopupMenu().getSetCompilable().setEnabled(false);
			selectedEditor.getPopupMenu().getUnsetCompilable().setEnabled(false);
			selectedEditor.getPopupMenu().getSetMain().setEnabled(false);
			selectedEditor.getPopupMenu().getUnsetMain().setEnabled(false);

			// Check the systeme clipboard
			if (Toolkit.getDefaultToolkit().getSystemClipboard()
					.getContents(null) != null)
				// Enables the paste menu item in the popup menu
				selectedEditor.getPopupMenu().getPaste().setEnabled(true);

			// Checks the selected editor
			if (selectedEditor.getActiveTextEditionArea().getSelectedText() != null) {

				// Enable the options
				selectedEditor.getPopupMenu().getCopy().setEnabled(true);
				selectedEditor.getPopupMenu().getCut().setEnabled(true);
			}

			// Is it default project?
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Disables the add and remove file menu item in the popup menu
				selectedEditor.getPopupMenu().getAddFile().setEnabled(false);
				selectedEditor.getPopupMenu().getRemoveFile().setEnabled(false);

				// Checks the types
				if (!selectedEditor.isMainFile())
					selectedEditor.getPopupMenu().getSetMain().setEnabled(true);
				if (selectedEditor.isMainFile())
					selectedEditor.getPopupMenu().getUnsetMain().setEnabled(true);
				if (!selectedEditor.isCompilerFile()
						|| (selectedEditor.isCompilerFile() && selectedEditor.isMainFile()))
					selectedEditor.getPopupMenu().getSetCompilable().setEnabled(true);
				if (selectedEditor.isCompilerFile() && !selectedEditor.isMainFile())
					selectedEditor.getPopupMenu().getUnsetCompilable().setEnabled(true);
			} else {

				// Searches for the file in the project list
				String file = MainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getAbsolutePath();

				boolean exists = false;

				for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList(); i++) {
					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i)
							.getPath().equals(file)) {
						exists = true;
					}
				}
				if (exists) {
					selectedEditor.getPopupMenu().getRemoveFile().setEnabled(true);
					selectedEditor.getPopupMenu().getAddFile().setEnabled(false);

					if (!selectedEditor.isMainFile())
						selectedEditor.getPopupMenu().getSetMain().setEnabled(true);
					if (selectedEditor.isMainFile())
						selectedEditor.getPopupMenu().getUnsetMain().setEnabled(true);
					if (!selectedEditor.isCompilerFile()
							|| (selectedEditor.isCompilerFile() && selectedEditor.isMainFile()))
						selectedEditor.getPopupMenu().getSetCompilable().setEnabled(true);
					if (selectedEditor.isCompilerFile() && !selectedEditor.isMainFile())
						selectedEditor.getPopupMenu().getUnsetCompilable()
								.setEnabled(true);

				} else {
					selectedEditor.getPopupMenu().getRemoveFile().setEnabled(false);
					selectedEditor.getPopupMenu().getAddFile().setEnabled(true);
				}
			}

			// Shows the popup menu
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getPopupMenu()
					.show(mouseEvent.getComponent(), mouseEvent.getX(),
							mouseEvent.getY());
		}
	}
}
