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
		SearchGUI searchGUI = SearchGUI.getInstance();
		searchGUI.setCurrentPosition(-2);
		searchGUI.setCycle(false);
		searchGUI.setEnd(false);
		searchGUI.getSearch().setTemporalPosition(-2);
		searchGUI.getSearch().setCycle(false);
		searchGUI.setCycle(false);
		searchGUI.setSelectedText(null);
		SearchGUI.setFirst(true);
		
		// REPLACE
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		replaceGUI.setCurrentPosition(-2);
		replaceGUI.setCycle(false);
		replaceGUI.setEnd(false);
		replaceGUI.getSearch().setTemporalPosition(-2);
		replaceGUI.getSearch().setCycle(false);
		replaceGUI.setCycle(false);
		replaceGUI.setSelectedText(null);
		ReplaceGUI.setIsFirstSearch(true);
		ReplaceGUI.setIsFirstReplacement(true);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	public void mouseReleased(MouseEvent arg0) {

		SearchGUI searchGUI = SearchGUI.getInstance();
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		MainWindow mainWindow = MainWindow.getInstance();
		
		String selectedText = null;
		int numEditor;
		numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
		selectedText = mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
				.getSelectedText();
		
		if (selectedText != null) {
			searchGUI.getSelectedRadioButton().setSelected(true);
			searchGUI.getAllRadioButton().setEnabled(false);
		} else {
			searchGUI.getCurrentDocumentRadioButton().setSelected(true);
			searchGUI.getAllRadioButton().setEnabled(true);
		}
		if (selectedText != null) {
			replaceGUI.getSelectedRadioButton().setSelected(true);
			replaceGUI.getAllRadioButton().setEnabled(false);
		} else {
			replaceGUI.getCurrentDocumentRadioButton().setSelected(true);
			replaceGUI.getAllRadioButton().setEnabled(true);
		}
		
		// SEARCH
		searchGUI.setCurrentPosition(-2);
		searchGUI.setCycle(false);
		searchGUI.setEnd(false);
		searchGUI.getSearch().setTemporalPosition(-2);
		searchGUI.getSearch().setCycle(false);
		searchGUI.setCycle(false);
		SearchGUI.setFirst(true);
		searchGUI.setSelectedText(null);
		
		// REPLACE
		replaceGUI.setCurrentPosition(-2);
		replaceGUI.setCycle(false);
		replaceGUI.setEnd(false);
		replaceGUI.getSearch().setTemporalPosition(-2);
		replaceGUI.getSearch().setCycle(false);
		replaceGUI.setCycle(false);
		replaceGUI.setSelectedText(null);
		ReplaceGUI.setIsFirstSearch(true);
		ReplaceGUI.setIsFirstReplacement(true);
	}
}
