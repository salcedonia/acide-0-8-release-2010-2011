package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * ACIDE - A Configurable IDE file editor text edition area mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideFileEditorTextEditionAreaMouseClickListener extends
		MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/**
	 * Dispatches the mouse event.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void dispatchEvent(MouseEvent mouseEvent) {

		// Requests the focus in window for the active editor text pane
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getTextEditionPanelList()
				.get(MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getActiveEditorIndex())
				.getTextPane().requestFocusInWindow();
	}
}
