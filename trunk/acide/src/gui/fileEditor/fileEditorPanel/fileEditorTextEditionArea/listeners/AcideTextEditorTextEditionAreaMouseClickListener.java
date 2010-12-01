package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/************************************************************************
 * Editor panel mouse click listener.
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
public class AcideTextEditorTextEditionAreaMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getTextEditionPanelList()
				.get(MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getActiveEditorIndex())
				.getTextPane().requestFocusInWindow();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		MainWindow
		.getInstance()
		.getFileEditorManager()
		.getSelectedFileEditorPanel()
		.getTextEditionPanelList()
		.get(MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveEditorIndex())
		.getTextPane().requestFocusInWindow();
	}
}
