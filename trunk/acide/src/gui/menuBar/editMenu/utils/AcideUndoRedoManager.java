package gui.menuBar.editMenu.utils;

import gui.mainWindow.MainWindow;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;

/************************************************************************
 * Undo redo manager of ACIDE - A Configurable IDE.
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
 * @see UndoManager
 ***********************************************************************/
public class AcideUndoRedoManager extends UndoManager {

	/**
	 * Acide undo redo manager class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Acide undo redo manager unique class instance.
	 */
	private static AcideUndoRedoManager _instance;
	
	/**
	 * Returns the Acide undo redo manager unique class instance.
	 * 
	 * @return the Acide undo redo manager unique class instance.
	 */
	public static AcideUndoRedoManager getInstance(){
		
		if(_instance == null)
			_instance = new AcideUndoRedoManager();
		return _instance;
	}
	
	/** 
	 * Creates a new Acide undo redo manager.
	 */
	public AcideUndoRedoManager() {
		super();
	}
	
	/**
	 * Adds a new undoable event to the undo redo manager of the new file
	 * which has been opened in the file editor panel.
	 */
	public void update(){
			
		// Gets the syntax document of the selected file editor panel
		DefaultStyledDocument document = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel().getSyntaxDocument();
		
		// Adds the undoable edit listener to support undoable events on it
		document.addUndoableEditListener(new UndoableEditListener() {
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
					
					// Sets the edit property over the general undo manager
					AcideUndoRedoManager.getInstance().addEdit(undoableEditEvent.getEdit());
				}
			}
		});
	}
}
