package gui.menuBar.fileMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/************************************************************************
 * Save all files item listener.
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
 * @see ActionListener
 ***********************************************************************/
public class SaveAllFilesMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the number of editors
		int numEditors = MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels();

		// From the first to the last file editor
		for (int i = 0; i < numEditors; i++) {

			// Sets the selected file editor panel at the current index
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(i);

			// Does the save or save as action
			MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
		}

		// Restores the original selected file editor panel
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);
	}
}