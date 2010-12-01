package gui.menuBar.fileMenu.listeners;

import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.SwingUtilities;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.undo.UndoableEdit;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * New file menu item listener.											
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
public class NewFileMenuItemListener implements ActionListener {

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
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		MainWindow.getInstance().getMenu().enableFileMenu();
		MainWindow.getInstance().getMenu().enableEditMenu();
		AcideFileEditorManager editorBuilder = MainWindow.getInstance()
				.getFileEditorManager();
		
		// Adds the new tab to the tabbed pane
		editorBuilder.newTab(labels.getString("s79"), labels
				.getString("s79"), "", true, 0);
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s80"));

		// UNDO REDO
		MainWindow.getInstance().getMenu().enableFileMenu();
		MainWindow.getInstance().getMenu().enableEditMenu();
		DefaultStyledDocument doc = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel().getSyntaxDocument();
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
					
					// Gets the selected editor index
					int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanelIndex();
					
					// Set the edit property over the selected editor undo manager
					MainWindow.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(selectedEditorIndex)
					.getUndoManager().addEdit(undoableEditEvent
							.getEdit());
				}
			}
		});

		// Updates the status bar
		MainWindow.getInstance().getStatusBar().setMessage(
				labels.getString("s79"));
		
		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				// Sets the focus on the text area
				MainWindow
					.getInstance()
					.getFileEditorManager().getFileEditorPanelAt(MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea().requestFocusInWindow();
			}
		});
	}
}
