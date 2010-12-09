package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/************************************************************************																
 * Editor panel keyboard listener.										
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
 * @see KeyAdapter																														
 ***********************************************************************/
public class AcideTextEditorTextEditionAreaKeyboardListener extends KeyAdapter {
	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent keyEvent) {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {
		
		AcideFileEditorPanel selectedEditor = MainWindow.getInstance()
		.getFileEditorManager().getSelectedFileEditorPanel();
		
		if (selectedEditor.getTextEditionPanelList().get(0)
				.getBraceMatcher() != -1) {
			selectedEditor.getSyntaxDocument().removeBrace(selectedEditor.getTextEditionPanelList().get(0)
					.getBraceMatcher());
			selectedEditor.getTextEditionPanelList().get(0)
			.setBraceMatcher(-1);
		}
		
		if (selectedEditor.getTextEditionPanelList().get(1)
				.getBraceMatcher() != -1) {
			selectedEditor.getSyntaxDocument().removeBrace(selectedEditor.getTextEditionPanelList().get(1)
					.getBraceMatcher());
			selectedEditor.getTextEditionPanelList().get(1)
			.setBraceMatcher(-1);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent keyEvent) {

		AcideFileEditorPanel selectedEditor = MainWindow.getInstance()
		.getFileEditorManager().getSelectedFileEditorPanel();
		
		if (selectedEditor.getTextEditionPanelList().get(0)
				.getBraceMatcher() != -1) {
			selectedEditor.getSyntaxDocument().removeBrace(selectedEditor.getTextEditionPanelList().get(0)
					.getBraceMatcher());
			selectedEditor.getTextEditionPanelList().get(0)
			.setBraceMatcher(-1);
		}
		
		if (selectedEditor.getTextEditionPanelList().get(1)
				.getBraceMatcher() != -1) {
			selectedEditor.getSyntaxDocument().removeBrace(selectedEditor.getTextEditionPanelList().get(1)
					.getBraceMatcher());
			selectedEditor.getTextEditionPanelList().get(1)
			.setBraceMatcher(-1);
		}
	};
}
