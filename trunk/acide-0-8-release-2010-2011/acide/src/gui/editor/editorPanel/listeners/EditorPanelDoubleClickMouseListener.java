package gui.editor.editorPanel.listeners;

import es.explorer.ExplorerFile;
import gui.editor.editorManager.utils.logic.MatchingBraces;
import gui.editor.editorPanel.EditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.text.BadLocationException;
import javax.swing.tree.TreePath;

import operations.log.Log;

/************************************************************************																
 * Editor panel double click listener										
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see MouseAdapter																													
 ***********************************************************************/
public class EditorPanelDoubleClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		EditorPanel selectedEditor = MainWindow.getInstance()
		.getEditorManager().getSelectedEditor();
		
		// Double click
		if (mouseEvent.getClickCount() > 1) {
			
			try {
				
				// Selects the word which is over the caret position in
				// the active editor
				int start = selectedEditor.getEditor().getCaretPosition();
				int end = MatchingBraces.findMatchingBracket(selectedEditor.getEditor()
						.getDocument(), start - 1);
				
				if (end > -1) {
					if (end > start)
						selectedEditor.selectText(start - 1, end - start + 2);
					if (end < start)
						selectedEditor.selectText(end, start - end);
				}
			} catch (BadLocationException exception) {
				
				// Updates the log
				Log.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		}

		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

			ExplorerFile explorerFile = new ExplorerFile();
			int y = -1;
			for (int j = 0; j < MainWindow.getInstance()
					.getProjectConfiguration().getNumFilesFromList(); j++) {

				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(j).getPath().equals(
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditor()
										.getAbsolutePath())) {

					explorerFile = MainWindow.getInstance()
							.getProjectConfiguration().getFileAt(j);

					for (int m = 0; m < MainWindow.getInstance()
							.getProjectConfiguration()
							.getNumFilesFromList() + 1; m++) {

						if (MainWindow
								.getInstance()
								.getExplorer()
								.getTree()
								.getPathForRow(m)
								.getLastPathComponent()
								.toString()
								.equals(explorerFile.getLastPathComponent())) {

							y = m;
						}
					}
				}
			}

			// Selects the file in the explorer tree
			TreePath currentSelection = MainWindow.getInstance()
					.getExplorer().getTree().getPathForRow(y);
			MainWindow.getInstance().getExplorer().getTree()
					.setSelectionPath(currentSelection);
		}
	}
}
