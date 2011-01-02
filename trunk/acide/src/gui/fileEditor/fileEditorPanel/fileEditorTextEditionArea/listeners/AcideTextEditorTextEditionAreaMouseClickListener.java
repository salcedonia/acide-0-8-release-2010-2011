package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Editor panel mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideTextEditorTextEditionAreaMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getTextEditionPanelList()
				.get(MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getActiveEditorIndex())
				.getTextPane().requestFocusInWindow();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
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
