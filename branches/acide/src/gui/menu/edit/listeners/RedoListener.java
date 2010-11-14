package gui.menu.edit.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.undo.CannotRedoException;

import operations.log.Log;

/************************************************************************
 * Redo menu item listener
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
public class RedoListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			if (MainWindow.getInstance().getMenu().getEdit().getUndoManager()
					.canRedo())
				MainWindow.getInstance().getMenu().getEdit().getUndoManager()
						.redo();
			
			// Updates the redo menu item option
			MainWindow
					.getInstance()
					.getMenu()
					.getEdit()
					.getUndo()
					.setEnabled(
							MainWindow.getInstance().getMenu().getEdit()
									.getUndoManager().canRedo());
			
		} catch (CannotRedoException exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
