package gui.menu.file.listeners;

import gui.editor.editorManager.EditorManager;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.undo.UndoableEdit;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * New file menu item listener											
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
public class NewFileListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
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

		MainWindow.getInstance().getMenu().enableFileMenu();
		MainWindow.getInstance().getMenu().enableEditMenu();
		EditorManager editorBuilder = MainWindow.getInstance()
				.getEditorManager();
		editorBuilder.newTab(labels.getString("s79"), labels
				.getString("s79"), "", true, 0);
		
		// Updates the log
		Log.getLog().info(labels.getString("s80"));

		// UNDO REDO
		MainWindow.getInstance().getMenu().enableFileMenu();
		MainWindow.getInstance().getMenu().enableEditMenu();
		DefaultStyledDocument doc = MainWindow.getInstance()
				.getEditorManager().getSelectedEditor().getSyntaxDocument();
		doc.addUndoableEditListener(new UndoableEditListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * javax.swing.event.UndoableEditListener#undoableEditHappened
			 * (javax.swing.event.UndoableEditEvent)
			 */
			@Override
			public void undoableEditHappened(UndoableEditEvent undoableEditEvent) {

				UndoableEdit edit = undoableEditEvent.getEdit();

				if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
						.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
					MainWindow.getInstance().getMenu().getEdit()
							.getUndoManager().addEdit(undoableEditEvent.getEdit());
				}
			}
		});

		// Updates the status bar
		MainWindow.getInstance().getStatusBar().setMessage(
				labels.getString("s79"));
	}
}
