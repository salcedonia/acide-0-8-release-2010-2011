package operations.listeners;

import gui.MainWindow;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Listener for the mouse events in the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AcideMouseListener extends MouseAdapter{

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	public void mouseClicked(MouseEvent arg0) {
		
		// SEARCH
		SearchGUI.getInstance().setCurrentPosition(-2);
		SearchGUI.getInstance().setCycle(false);
		SearchGUI.getInstance().setEnd(false);
		SearchGUI.getInstance().getSearch().setTemporalPosition(-2);
		SearchGUI.getInstance().getSearch().setCycle(false);
		SearchGUI.getInstance().setCycle(false);
		SearchGUI.getInstance().setSelectedText(null);
		SearchGUI.setFirst(true);
		
		// REPLACE
		ReplaceGUI.getInstance().setCurrentPosition(-2);
		ReplaceGUI.getInstance().setCycle(false);
		ReplaceGUI.getInstance().setEnd(false);
		ReplaceGUI.getInstance().getSearch().setTemporalPosition(-2);
		ReplaceGUI.getInstance().getSearch().setCycle(false);
		ReplaceGUI.getInstance().setCycle(false);
		ReplaceGUI.getInstance().setSelectedText(null);
		ReplaceGUI.setIsFirstSearch(true);
		ReplaceGUI.setIsFirstReplacement(true);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	public void mouseReleased(MouseEvent arg0) {

		String selectedText = null;
		int numEditor;
		numEditor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();
		selectedText = MainWindow.getInstance().getEditorBuilder().getEditorAt(numEditor).getEditor()
				.getSelectedText();
		
		if (selectedText != null) {
			SearchGUI.getInstance().getSelectedRadioButton().setSelected(true);
			SearchGUI.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			SearchGUI.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			SearchGUI.getInstance().getAllRadioButton().setEnabled(true);
		}
		if (selectedText != null) {
			ReplaceGUI.getInstance().getSelectedRadioButton().setSelected(true);
			ReplaceGUI.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			ReplaceGUI.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			ReplaceGUI.getInstance().getAllRadioButton().setEnabled(true);
		}
		
		// SEARCH
		SearchGUI.getInstance().setCurrentPosition(-2);
		SearchGUI.getInstance().setCycle(false);
		SearchGUI.getInstance().setEnd(false);
		SearchGUI.getInstance().getSearch().setTemporalPosition(-2);
		SearchGUI.getInstance().getSearch().setCycle(false);
		SearchGUI.getInstance().setCycle(false);
		SearchGUI.setFirst(true);
		SearchGUI.getInstance().setSelectedText(null);
		
		// REPLACE
		ReplaceGUI.getInstance().setCurrentPosition(-2);
		ReplaceGUI.getInstance().setCycle(false);
		ReplaceGUI.getInstance().setEnd(false);
		ReplaceGUI.getInstance().getSearch().setTemporalPosition(-2);
		ReplaceGUI.getInstance().getSearch().setCycle(false);
		ReplaceGUI.getInstance().setCycle(false);
		ReplaceGUI.getInstance().setSelectedText(null);
		ReplaceGUI.setIsFirstSearch(true);
		ReplaceGUI.setIsFirstReplacement(true);
	}
}
