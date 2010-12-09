package gui.listeners;

import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.gui.replace.ReplaceWindow;
import gui.menuBar.editMenu.gui.search.SearchWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/************************************************************************																
 * ACIDE - A Configurable IDE mouse listener.
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
public class AcideMouseListener extends MouseAdapter{

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		
		// SEARCH
		SearchWindow.getInstance().setCurrentPosition(-2);
		SearchWindow.getInstance().setIsCycle(false);
		SearchWindow.getInstance().setIsEnd(false);
		SearchWindow.getInstance().getSearch().setTemporalPosition(-2);
		SearchWindow.getInstance().getSearch().setIsCycle(false);
		SearchWindow.getInstance().setIsCycle(false);
		SearchWindow.getInstance().setSelectedText(null);
		SearchWindow.setIsFirst(true);
		
		// REPLACE
		ReplaceWindow.getInstance().setCurrentPosition(-2);
		ReplaceWindow.getInstance().setIsCycle(false);
		ReplaceWindow.getInstance().setIsEnd(false);
		ReplaceWindow.getInstance().getSearch().setTemporalPosition(-2);
		ReplaceWindow.getInstance().getSearch().setIsCycle(false);
		ReplaceWindow.getInstance().setIsCycle(false);
		ReplaceWindow.getInstance().setSelectedText(null);
		ReplaceWindow.setIsFirstSearch(true);
		ReplaceWindow.setIsFirstReplacement(true);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {

		String selectedText = null;
		int numEditor;
		numEditor = MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanelIndex();
		selectedText = MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor).getActiveTextEditionArea()
				.getSelectedText();
		
		if (selectedText != null) {
			SearchWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
			SearchWindow.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			SearchWindow.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			SearchWindow.getInstance().getAllRadioButton().setEnabled(true);
		}
		if (selectedText != null) {
			ReplaceWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
			ReplaceWindow.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			ReplaceWindow.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			ReplaceWindow.getInstance().getAllRadioButton().setEnabled(true);
		}
		
		// SEARCH
		SearchWindow.getInstance().setCurrentPosition(-2);
		SearchWindow.getInstance().setIsCycle(false);
		SearchWindow.getInstance().setIsEnd(false);
		SearchWindow.getInstance().getSearch().setTemporalPosition(-2);
		SearchWindow.getInstance().getSearch().setIsCycle(false);
		SearchWindow.getInstance().setIsCycle(false);
		SearchWindow.setIsFirst(true);
		SearchWindow.getInstance().setSelectedText(null);
		
		// REPLACE
		ReplaceWindow.getInstance().setCurrentPosition(-2);
		ReplaceWindow.getInstance().setIsCycle(false);
		ReplaceWindow.getInstance().setIsEnd(false);
		ReplaceWindow.getInstance().getSearch().setTemporalPosition(-2);
		ReplaceWindow.getInstance().getSearch().setIsCycle(false);
		ReplaceWindow.getInstance().setIsCycle(false);
		ReplaceWindow.getInstance().setSelectedText(null);
		ReplaceWindow.setIsFirstSearch(true);
		ReplaceWindow.setIsFirstReplacement(true);
	}
}
