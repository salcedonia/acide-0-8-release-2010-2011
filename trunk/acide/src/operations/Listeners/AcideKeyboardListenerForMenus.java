package operations.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/************************************************************************																
 * ACIDE keyboard listener for menus.											
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
public class AcideKeyboardListenerForMenus extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getFile().getSaveAllFiles().setEnabled(true);

		MainWindow.getInstance().getMenu().getEdit().getUndo().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getRedo().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getCopy().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getPaste().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getCut().setEnabled(true);

		MainWindow.getInstance().getMenu().getProject().getSaveProject().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getRemoveFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getDeleteFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getRemoveFolder().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getSetMain().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getUnsetMain().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getSetCompilable().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getUnsetCompilable().setEnabled(true);
	}
}
