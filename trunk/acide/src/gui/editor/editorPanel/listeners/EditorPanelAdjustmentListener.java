package gui.editor.editorPanel.listeners;

import gui.editor.editorPanel.EditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.Adjustable;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

/************************************************************************
 * Editor panel adjustment listener
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
 * @see AdjustmentListener
 ***********************************************************************/
public class EditorPanelAdjustmentListener implements AdjustmentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt
	 * .event.AdjustmentEvent)
	 */
	@Override
	public void adjustmentValueChanged(AdjustmentEvent adjustmentEvent) {

		EditorPanel selectedEditor = MainWindow.getInstance()
				.getEditorManager().getSelectedEditor();
		Adjustable source = adjustmentEvent.getAdjustable();

		if (adjustmentEvent.getValueIsAdjusting()) {

			// VERTICAL VALUE 1
			selectedEditor.setVerticalValue1(selectedEditor.getScrollPane1()
					.getVerticalScrollBar().getValue());

			// HORIZONTAL VALUE 1
			selectedEditor.setHorizontalValue1(selectedEditor.getScrollPane1()
					.getHorizontalScrollBar().getValue());

			// VERTICAL VALUE 2
			selectedEditor.setVerticalValue2(selectedEditor.getScrollPane2()
					.getVerticalScrollBar().getValue());

			// HORIZONTAL VALUE 2
			selectedEditor.setHorizontalValue2(selectedEditor.getScrollPane2()
					.getHorizontalScrollBar().getValue());
			return;
		}
		int orientation = source.getOrientation();
		if (orientation == Adjustable.HORIZONTAL) {
		} else {
		}
		int type = adjustmentEvent.getAdjustmentType();
		switch (type) {
		case AdjustmentEvent.UNIT_INCREMENT:
			break;
		case AdjustmentEvent.UNIT_DECREMENT:
			break;
		case AdjustmentEvent.BLOCK_INCREMENT:
			break;
		case AdjustmentEvent.BLOCK_DECREMENT:
			break;
		case AdjustmentEvent.TRACK:
			break;
		}
		
		// EDITOR1 is focused
		if (selectedEditor.getEditor1().isFocusOwner()
				|| selectedEditor.getScrollPane1().getVerticalScrollBar().isFocusOwner()
				|| selectedEditor.getScrollPane1().getHorizontalScrollBar().isFocusOwner()) {
			
			// Updates the values
			selectedEditor.getScrollPane2().getVerticalScrollBar().setValue(selectedEditor.getVerticalValue2());
			selectedEditor.getScrollPane2().getHorizontalScrollBar().setValue(selectedEditor.getHorizontalValue2());
			selectedEditor.setVerticalValue1(selectedEditor.getScrollPane1().getVerticalScrollBar().getValue());
			selectedEditor.setHorizontalValue1(selectedEditor.getScrollPane1().getHorizontalScrollBar()
					.getValue());
		}
		
		// EDITOR2 is focused
		if (selectedEditor.getEditor2().isFocusOwner()
				|| selectedEditor.getScrollPane2().getVerticalScrollBar().isFocusOwner()
				|| selectedEditor.getScrollPane2().getHorizontalScrollBar().isFocusOwner()) {
			
			// Updates the values
			selectedEditor.getScrollPane1().getVerticalScrollBar().setValue(selectedEditor.getVerticalValue1());
			selectedEditor.getScrollPane1().getHorizontalScrollBar().setValue(selectedEditor.getHorizontalValue1());
			selectedEditor.setVerticalValue2(selectedEditor.getScrollPane2().getVerticalScrollBar().getValue());
			selectedEditor.setHorizontalValue2(selectedEditor.getScrollPane2().getHorizontalScrollBar()
					.getValue());
		}
	}
}
