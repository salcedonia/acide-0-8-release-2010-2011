package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.Adjustable;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

/************************************************************************
 * Editor panel adjustment listener.
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
public class AcideTextEditorTextEditionAreaAdjustmentListener implements AdjustmentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt
	 * .event.AdjustmentEvent)
	 */
	@Override
	public void adjustmentValueChanged(AdjustmentEvent adjustmentEvent) {

		AcideFileEditorPanel selectedEditor = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel();
		Adjustable source = adjustmentEvent.getAdjustable();

		if (adjustmentEvent.getValueIsAdjusting()) {

			// VERTICAL VALUE 1
			selectedEditor
					.getTextEditionPanelList()
					.get(0)
					.setVerticalValue(
							selectedEditor.getTextEditionPanelList().get(0)
									.getScrollPane().getVerticalScrollBar()
									.getValue());

			// HORIZONTAL VALUE 1
			selectedEditor
					.getTextEditionPanelList()
					.get(0)
					.setHorizontalValue(
							selectedEditor.getTextEditionPanelList().get(0)
									.getScrollPane().getHorizontalScrollBar()
									.getValue());

			// VERTICAL VALUE 2
			selectedEditor
					.getTextEditionPanelList()
					.get(1)
					.setVerticalValue(
							selectedEditor.getTextEditionPanelList().get(1)
									.getScrollPane().getVerticalScrollBar()
									.getValue());

			// HORIZONTAL VALUE 2
			selectedEditor
					.getTextEditionPanelList()
					.get(1)
					.setHorizontalValue(
							selectedEditor.getTextEditionPanelList().get(1)
									.getScrollPane().getHorizontalScrollBar()
									.getValue());
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
		if (selectedEditor.getTextEditionPanelList().get(0).getScrollPane().isFocusOwner()
				|| selectedEditor.getTextEditionPanelList().get(0)
				.getScrollPane().getVerticalScrollBar()
						.isFocusOwner()
				|| selectedEditor.getTextEditionPanelList().get(0)
				.getScrollPane().getHorizontalScrollBar()
						.isFocusOwner()) {

			// Updates the values
			selectedEditor.getTextEditionPanelList().get(1)
			.getScrollPane().getVerticalScrollBar()
					.setValue(selectedEditor.getTextEditionPanelList().get(1)
							.getVerticalValue());
			selectedEditor.getTextEditionPanelList().get(1)
			.getScrollPane().getHorizontalScrollBar()
					.setValue(selectedEditor.getTextEditionPanelList().get(1)
							.getHorizontalValue());
			selectedEditor.getTextEditionPanelList().get(0)
			.setVerticalValue(selectedEditor.getTextEditionPanelList().get(0)
					.getScrollPane()
					.getVerticalScrollBar().getValue());
			selectedEditor.getTextEditionPanelList().get(0)
			.setHorizontalValue(selectedEditor.getTextEditionPanelList().get(0)
					.getScrollPane()
					.getHorizontalScrollBar().getValue());
		}

		// EDITOR2 is focused
		if (selectedEditor.getTextEditionPanelList().get(1).getScrollPane().isFocusOwner()
				|| selectedEditor.getTextEditionPanelList().get(1)
				.getScrollPane().getVerticalScrollBar()
						.isFocusOwner()
				|| selectedEditor.getTextEditionPanelList().get(1)
				.getScrollPane().getHorizontalScrollBar()
					.isFocusOwner()) {

			// Updates the values
			selectedEditor.getTextEditionPanelList().get(0)
			.getScrollPane().getVerticalScrollBar()
					.setValue(selectedEditor.getTextEditionPanelList().get(0)
							.getVerticalValue());
			selectedEditor.getTextEditionPanelList().get(0)
			.getScrollPane().getHorizontalScrollBar()
					.setValue(selectedEditor.getTextEditionPanelList().get(0)
							.getHorizontalValue());
			selectedEditor.getTextEditionPanelList().get(1)
			.setVerticalValue(selectedEditor.getTextEditionPanelList().get(1)
					.getScrollPane()
					.getVerticalScrollBar().getValue());
			selectedEditor.getTextEditionPanelList().get(1)
			.setHorizontalValue(selectedEditor.getTextEditionPanelList().get(1)
					.getScrollPane()
					.getHorizontalScrollBar().getValue());
		}
	}
}
