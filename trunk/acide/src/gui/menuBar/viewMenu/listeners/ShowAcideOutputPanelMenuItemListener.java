package gui.menuBar.viewMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/************************************************************************																
 * Show output panel menu item listener.
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
 * @see ActionListener																												
 ***********************************************************************/
public class ShowAcideOutputPanelMenuItemListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		
		if (MainWindow.getInstance().getMenu().getView().getShowOutputPanel().isSelected()) {
			MainWindow.getInstance().getHorizontalSplitPane().setDividerLocation(MainWindow.getInstance().getMenu().getView().getShellSize());
			MainWindow.getInstance().getHorizontalSplitPane().getBottomComponent()
					.setVisible(true);
		} else {
			MainWindow.getInstance().getMenu().getView().setShellSize(MainWindow.getInstance().getHorizontalSplitPane().getDividerLocation());
			MainWindow.getInstance().getHorizontalSplitPane().setDividerLocation(0);
			MainWindow.getInstance().getHorizontalSplitPane().getBottomComponent()
					.setVisible(false);
		}
		
		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
			
			// The project has been modified
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
	}
}
