package gui.outputPanel.listeners;

import gui.mainWindow.MainWindow;
import gui.outputPanel.AcideOutputPanel;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/************************************************************************																
 * Output panel popup menu listener.										
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
public class AcideOutputPanelPopupMenuListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/**
	 * Shows the popup menu.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void maybeShowPopup(MouseEvent mouseEvent) {

		AcideOutputPanel outputPanel = MainWindow.getInstance().getOutput();
		
		if (mouseEvent.isPopupTrigger()) {

			// If there is not selected text
			if (outputPanel.getTextComponent().getSelectedText() == null) {

				// Disables the copy menu item
				outputPanel.getPopupMenu().getCopy().setEnabled(false);

				// Disables the cut menu item
				outputPanel.getPopupMenu().getCut().setEnabled(false);
			} else {

				// Enables the copy menu item
				outputPanel.getPopupMenu().getCopy().setEnabled(true);

				// If the caret is after the prompt
				if (outputPanel.getTextComponent().getSelectionStart() < outputPanel.getPromptCaretPosition())
					outputPanel.getPopupMenu().getCut().setEnabled(false);
				else
					outputPanel.getPopupMenu().getCut().setEnabled(true);
			}

			// If the caret position is before the prompt
			if (outputPanel.getTextComponent().getSelectionStart() < outputPanel.getPromptCaretPosition())
				outputPanel.getPopupMenu().getPaste().setEnabled(false);
			else
				outputPanel.getPopupMenu().getPaste().setEnabled(true);

			// Shows the popup menu
			outputPanel.getPopupMenu().show(mouseEvent.getComponent(), mouseEvent.getX(), mouseEvent.getY());
		}
	}
}
