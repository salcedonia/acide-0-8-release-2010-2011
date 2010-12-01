package gui.outputPanel.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

/************************************************************************																
 * ACIDE - A Configurable IDE output panel focus listener.
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
 * @see FocusListener																													
 ***********************************************************************/
public class AcideOutputPanelFocusListener implements FocusListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.FocusListener#focusGained(java.awt.event. FocusEvent)
	 */
	@Override
	public void focusGained(FocusEvent focusEvent) {

		int selectedEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		if (selectedEditorIndex != -1)
			// Hides the caret in the opened editor
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getCaret().setVisible(false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent )
	 */
	@Override
	public void focusLost(FocusEvent focusEvent) {

		int selectedEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		if (selectedEditorIndex != -1)
			// Shows the caret in the opened editor
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getCaret().setVisible(true);
	}
}
