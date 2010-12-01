package gui.menuBar.editMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/************************************************************************
 * Select all menu item listener.
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
 * @see ActionListener
 ***********************************************************************/
public class SelectAllMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Sets the caret in the first position of the editor
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
				.setCaretPosition(0);

		// Get the text length
		int length = MainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
				.getText().length();

		// Sets the selection from the first the last
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
				.setSelectionEnd(length);
	}
}
