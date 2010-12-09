package gui.fileEditor.fileEditorPanel.listeners;

import gui.mainWindow.MainWindow;

import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.undo.UndoManager;

/************************************************************************
 * Editor panel document listener.
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
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan Jos� Ortiz S�nchez
 *         </ul>
 *         <ul>
 *         Delf�n Rup�rez Ca�as
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Mart�n L�zaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo G�mez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see DocumentListener
 ***********************************************************************/
public class AcideFileEditorPanelDocumentListener implements DocumentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void insertUpdate(DocumentEvent documentEvent) {

		// Gets the selected editor index
		final int selectedEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the selected editor undo manager
		final UndoManager undoManager = MainWindow.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(selectedEditorIndex).getUndoManager();

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				if (undoManager.canUndo()) {

					// Sets the red icon to the close button
					MainWindow.getInstance().getFileEditorManager()
							.getTestPlaf()
							.getCloseButtonAt(selectedEditorIndex)
							.setRedCloseButton();

					// Enables the save as menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(true);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				} else {
					// Sets the green icon to the close button
					MainWindow.getInstance().getFileEditorManager()
							.getTestPlaf()
							.getCloseButtonAt(selectedEditorIndex)
							.setGreenCloseButton();

					// Enables the save as menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(false);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(false);
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent documentEvent) {

		// Gets the selected editor index
		final int selectedEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the selected editor undo manager
		final UndoManager undoManager = MainWindow.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(selectedEditorIndex).getUndoManager();

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				if (undoManager.canUndo()) {

					// Sets the red icon to the close button
					MainWindow.getInstance().getFileEditorManager()
							.getTestPlaf()
							.getCloseButtonAt(selectedEditorIndex)
							.setRedCloseButton();

					// Enables the save as menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(true);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				} else {
					// Sets the green icon to the close button
					MainWindow.getInstance().getFileEditorManager()
							.getTestPlaf()
							.getCloseButtonAt(selectedEditorIndex)
							.setGreenCloseButton();

					// Enables the save as menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(false);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(false);
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {

		// Gets the selected editor index
		final int selectedEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the selected editor undo manager
		final UndoManager undoManager = MainWindow.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(selectedEditorIndex).getUndoManager();

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				if (undoManager.canUndo()) {

					// Sets the red icon to the close button
					MainWindow.getInstance().getFileEditorManager()
							.getTestPlaf()
							.getCloseButtonAt(selectedEditorIndex)
							.setRedCloseButton();

					// Enables the save as menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(true);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				} else {
					// Sets the green icon to the close button
					MainWindow.getInstance().getFileEditorManager()
							.getTestPlaf()
							.getCloseButtonAt(selectedEditorIndex)
							.setGreenCloseButton();

					// Enables the save as menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(false);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(false);
				}
			}
		});
	}
}
