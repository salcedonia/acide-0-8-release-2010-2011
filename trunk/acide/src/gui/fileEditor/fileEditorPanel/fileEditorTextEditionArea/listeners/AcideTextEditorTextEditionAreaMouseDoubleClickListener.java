package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import es.explorer.ExplorerFile;
import gui.fileEditor.fileEditorManager.utils.logic.MatchingBraces;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.text.BadLocationException;
import javax.swing.tree.TreePath;

import operations.log.AcideLog;

/************************************************************************																
 * Editor panel double click listener.
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
public class AcideTextEditorTextEditionAreaMouseDoubleClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		AcideFileEditorPanel selectedEditor = MainWindow.getInstance()
		.getFileEditorManager().getSelectedFileEditorPanel();
		
		// Double click
		if (mouseEvent.getClickCount() > 1) {
			
			try {
				
				// Selects the word which is over the caret position in
				// the active editor
				int start = selectedEditor.getActiveTextEditionArea().getCaretPosition();
				int end = MatchingBraces.findMatchingBracket(selectedEditor.getActiveTextEditionArea()
						.getDocument(), start - 1);
				
				if (end > -1) {
					if (end > start)
						selectedEditor.selectText(start - 1, end - start + 2);
					if (end < start)
						selectedEditor.selectText(end, start - end);
				}
			} catch (BadLocationException exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		}

		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

			ExplorerFile explorerFile = new ExplorerFile();
			int index = -1;
			for (int position = 0; position < MainWindow.getInstance()
					.getProjectConfiguration().getNumFilesFromList(); position++) {

				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(position).getPath().equals(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath())) {

					explorerFile = MainWindow.getInstance()
							.getProjectConfiguration().getFileAt(position);

					for (int positionProject = 0; positionProject < MainWindow.getInstance()
							.getProjectConfiguration()
							.getNumFilesFromList() + 1; positionProject++) {

						if (MainWindow
								.getInstance()
								.getExplorer()
								.getTree()
								.getPathForRow(positionProject)
								.getLastPathComponent()
								.toString()
								.equals(explorerFile.getLastPathComponent())) {

							index = positionProject;
						}
					}
				}
			}

			// Selects the file in the explorer tree
			TreePath currentSelection = MainWindow.getInstance()
					.getExplorer().getTree().getPathForRow(index);
			MainWindow.getInstance().getExplorer().getTree()
					.setSelectionPath(currentSelection);
		}
	}
}
