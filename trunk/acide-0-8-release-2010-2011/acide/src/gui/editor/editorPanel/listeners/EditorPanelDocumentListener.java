package gui.editor.editorPanel.listeners;

import gui.mainWindow.MainWindow;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/************************************************************************																
 * Editor panel document listener										
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
 * @see DocumentListener																												
 ***********************************************************************/
public class EditorPanelDocumentListener implements DocumentListener {
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.DocumentListener#insertUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void insertUpdate(DocumentEvent documentEvent) {
		
		// Sets the red icon
		MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditorIndex()).setRedButton();
		
		// Enables the save as menu item 
		MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
				.setEnabled(true);
		
		// The project configuration has been modified
		MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent documentEvent) {
		
		// Sets the red icon
		MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditorIndex()).setRedButton();
		
		// Enables the save as menu item 
		MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
				.setEnabled(true);
		
		// The project configuration has been modified
		MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {
		
		// Sets the red icon
		MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditorIndex()).setRedButton();
		
		// Enables the save as menu item 
		MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
				.setEnabled(true);
		
		// The project configuration has been modified
		MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
	}
}
