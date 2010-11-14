package gui.menu.view.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/************************************************************************																
 * Show output panel menu item listener											
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
 * @see ActionListener																												
 ***********************************************************************/
public class ShowOutputPanelListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		
		if (MainWindow.getInstance().getMenu().getView().getShowOutputPanel().isSelected()) {
			MainWindow.getInstance().getSplitPaneHorizontal().setDividerLocation(MainWindow.getInstance().getMenu().getView().getShellSize());
			MainWindow.getInstance().getSplitPaneHorizontal().getBottomComponent()
					.setVisible(true);
		} else {
			MainWindow.getInstance().getMenu().getView().setShellSize(MainWindow.getInstance().getSplitPaneHorizontal().getDividerLocation());
			MainWindow.getInstance().getSplitPaneHorizontal().setDividerLocation(0);
			MainWindow.getInstance().getSplitPaneHorizontal().getBottomComponent()
					.setVisible(false);
		}
		
		// NOT DEFAULT PROJECT
		if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
			
			// THE PROJECT HAS BEEN MODIFIED
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
	}
}
