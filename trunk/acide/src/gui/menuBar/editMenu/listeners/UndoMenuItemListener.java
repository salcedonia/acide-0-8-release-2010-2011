package gui.menuBar.editMenu.listeners;

import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.undo.CannotUndoException;

import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE edit menu undo menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class UndoMenuItemListener implements ActionListener {

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

				if (AcideUndoRedoManager.getInstance().canUndo())
					AcideUndoRedoManager.getInstance().undo();
				
				// Updates the undo menu item option
				MainWindow.getInstance().getMenu().getEdit().getUndo()
						.setEnabled(AcideUndoRedoManager.getInstance().canUndo());
			}
		} catch (CannotUndoException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}