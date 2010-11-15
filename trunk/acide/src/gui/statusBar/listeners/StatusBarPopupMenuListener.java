package gui.statusBar.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/************************************************************************
 * Status bar popup menu listener
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan Jos� Ortiz S�nchez
 *         </ul>
 *         <ul>
 *         Delf�n Rup�rez Ca�as
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Mart�n L�zaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo G�mez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see MouseAdapter
 ***********************************************************************/
public class StatusBarPopupMenuListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent )
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		if (mouseEvent.isPopupTrigger())
			MainWindow
					.getInstance()
					.getStatusBar()
					.getPopupMenu()
					.show(mouseEvent.getComponent(), mouseEvent.getX(),
							mouseEvent.getY());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent
	 * )
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
		if (mouseEvent.isPopupTrigger())
			MainWindow
					.getInstance()
					.getStatusBar()
					.getPopupMenu()
					.show(mouseEvent.getComponent(), mouseEvent.getX(),
							mouseEvent.getY());
	}
}
