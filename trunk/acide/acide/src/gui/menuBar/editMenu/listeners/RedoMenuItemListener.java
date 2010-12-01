package gui.menuBar.editMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.undo.CannotRedoException;
import javax.swing.undo.UndoManager;

import operations.log.AcideLog;

/************************************************************************
 * Redo menu item listener.
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
 * @see ActionListener
 ***********************************************************************/
public class RedoMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// Gets the selected editor index
			int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// If there are opened editors
			if (selectedEditorIndex != -1) {
				
				// Gets the selected editor undo manager
				UndoManager selectedEditorUndoManager = MainWindow
						.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex).getUndoManager();

				if (selectedEditorUndoManager.canRedo())
					selectedEditorUndoManager.redo();

				// Updates the redo menu item option
				MainWindow.getInstance().getMenu().getEdit().getUndo()
						.setEnabled(selectedEditorUndoManager.canRedo());
			}
		} catch (CannotRedoException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
