package operations.listeners;

import gui.MainWindow;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * 
 */
public class AcideMouseListener extends MouseAdapter{

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	public void mouseClicked(MouseEvent arg0) {
		SearchGUI bu = SearchGUI.getInstance();
		ReplaceGUI re = ReplaceGUI.getInstance();
		// Search
		bu.setCurrentPosition(-2);
		bu.setCycle(false);
		bu.setEnd(false);
		bu.getSearch().setTemporalPosition(-2);
		bu.getSearch().setCycle(false);
		bu.setCycle(false);
		bu.setSelectedText(null);
		SearchGUI.setFirst(true);
		// Replace
		re.setCurrentPosition(-2);
		re.setCycle(false);
		re.setEnd(false);
		re.getB().setTemporalPosition(-2);
		re.getB().setCycle(false);
		re.setCycle(false);
		re.setSelectedText(null);
		ReplaceGUI.setFirst(true);
		ReplaceGUI.setFirstReplacement(true);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@SuppressWarnings("static-access")
	public void mouseReleased(MouseEvent arg0) {

		SearchGUI searchGUI = SearchGUI.getInstance();
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		MainWindow mainWindow = MainWindow.getInstance();
		String t = null;
		int numEditor;
		numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
		t = mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
				.getSelectedText();
		if (t != null) {
			searchGUI.getRdBtnSelected().setSelected(true);
			searchGUI.getRdBtnAll().setEnabled(false);
		} else {
			searchGUI.getRdBtnCurrentDocument().setSelected(true);
			searchGUI.getRdBtnAll().setEnabled(true);
		}
		if (t != null) {
			replaceGUI.getSelected().setSelected(true);
			replaceGUI.getRadioTodo().setEnabled(false);
		} else {
			replaceGUI.getCurrentDocument().setSelected(true);
			replaceGUI.getRadioTodo().setEnabled(true);
		}
		// Search
		searchGUI.setCurrentPosition(-2);
		searchGUI.setCycle(false);
		searchGUI.setEnd(false);
		searchGUI.getSearch().setTemporalPosition(-2);
		searchGUI.getSearch().setCycle(false);
		searchGUI.setCycle(false);
		SearchGUI.setFirst(true);
		searchGUI.setSelectedText(null);
		// Replace
		replaceGUI.setCurrentPosition(-2);
		replaceGUI.setCycle(false);
		replaceGUI.setEnd(false);
		replaceGUI.getB().setTemporalPosition(-2);
		replaceGUI.getB().setCycle(false);
		replaceGUI.setCycle(false);
		replaceGUI.setSelectedText(null);
		ReplaceGUI.setFirst(true);
		ReplaceGUI.setFirstReplacement(true);
	}
}
