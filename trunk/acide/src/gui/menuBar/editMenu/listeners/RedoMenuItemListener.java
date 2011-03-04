package gui.menuBar.editMenu.listeners;

import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.undo.CannotRedoException;

import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE edit menu redo menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
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

			// Gets the selected file editor panel index
			int selectedFileEditorPanelIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// If there are opened editors
			if (selectedFileEditorPanelIndex != -1) {
				
				if (AcideUndoRedoManager.getInstance().canRedo())
					AcideUndoRedoManager.getInstance().redo();

				// Updates the redo menu item option
				MainWindow.getInstance().getMenu().getEdit().getUndo()
						.setEnabled(AcideUndoRedoManager.getInstance().canRedo());
			}
		} catch (CannotRedoException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
