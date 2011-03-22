package acide.gui.menuBar.editMenu.listeners;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.undo.CannotRedoException;

import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE edit menu redo menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideRedoMenuItemListener implements ActionListener {

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
			int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// If there are opened editors
			if (selectedFileEditorPanelIndex != -1) {
				
				if (AcideUndoManager.getInstance().canRedo())
					AcideUndoManager.getInstance().redo();

				// Updates the redo menu item option
				AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem()
						.setEnabled(AcideUndoManager.getInstance().canRedo());
			}
		} catch (CannotRedoException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
