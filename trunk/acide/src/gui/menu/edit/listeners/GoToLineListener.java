package gui.menu.edit.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************
 * Go to line menu item listener
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
public class GoToLineListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Ask the user for the line number
		String line = (String) JOptionPane.showInputDialog(null,
				labels.getString("s448"), labels.getString("s447"),
				JOptionPane.YES_NO_CANCEL_OPTION, null, null, null);

		// TODO: COMPROBAR QUE EL NòMERO DE LêNEA SEA UN NòMERO
		if ((line != null)) {
			try {

				int selectedEditor = MainWindow.getInstance()
						.getEditorManager().getSelectedEditorIndex();

				if (selectedEditor >= 0)
					MainWindow.getInstance().getEditorManager()
							.getEditorAt(selectedEditor)
							.goToLine(Integer.parseInt(line));
			} catch (Exception exception) {
				
				// Updates the log
				Log.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}
}
