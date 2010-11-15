package gui.editor.editorPanel.listeners;

import gui.editor.editorManager.utils.logic.MatchingBraces;
import gui.editor.editorPanel.EditorPanel;
import gui.mainWindow.MainWindow;

import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import operations.log.Log;

/************************************************************************																
 * Editor panel caret listener										
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
 * @see CaretListener																													
 ***********************************************************************/
public class EditorPanelCaretListener implements CaretListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent
	 * )
	 */
	@Override
	public void caretUpdate(CaretEvent caretEvent) {

		EditorPanel selectedEditor = MainWindow.getInstance().getEditorManager().getSelectedEditor();
		
		if (selectedEditor.getEditor1().isFocusOwner())
			selectedEditor.setActiveEditor(1);

		if (selectedEditor.getEditor2().isFocusOwner())
			selectedEditor.setActiveEditor(2);

		Element root = selectedEditor.getSyntaxDocument().getDefaultRootElement();
		int dot = caretEvent.getDot();
		int line = root.getElementIndex(dot);
		int col = dot - root.getElement(line).getStartOffset();
		
		// Updates the status bar
		MainWindow.getInstance().getStatusBar().getMessageLineCol()
				.setText((line + 1) + ":" + (col + 1));

		try {
			if (selectedEditor.getBrace1() != -1) {
				selectedEditor.getSyntaxDocument().removeBrace(selectedEditor.getBrace1());
				selectedEditor.setBrace1(-1);
			}
			if (selectedEditor.getBrace2() != -1) {
				selectedEditor.getSyntaxDocument().removeBrace(selectedEditor.getBrace2());
				selectedEditor.setBrace2(-1);
			}
			
			int start = selectedEditor.getEditor().getCaretPosition();
			int end = MatchingBraces.findMatchingBracket(selectedEditor.getEditor()
					.getDocument(), start - 1);
			
			if (((start > 0) && (start <= selectedEditor.getSyntaxDocument().getLength()))
					&& ((end >= 0) && (end <= selectedEditor.getSyntaxDocument().getLength()))) {
				
				if (end > -1) {
					
					if (end > start) {
						selectedEditor.setBrace1(start - 1);
						selectedEditor.setBrace2(end);
						selectedEditor.getSyntaxDocument().setBrace(selectedEditor.getBrace1());
						selectedEditor.getSyntaxDocument().setBrace(selectedEditor.getBrace2());
					} else if (end < start) {
						selectedEditor.setBrace1(end);
						selectedEditor.setBrace2(start - 1);
						selectedEditor.getSyntaxDocument().setBrace(selectedEditor.getBrace1());
						selectedEditor.getSyntaxDocument().setBrace(selectedEditor.getBrace2());
					}
				}
			}
		} catch (BadLocationException exception) {

			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
