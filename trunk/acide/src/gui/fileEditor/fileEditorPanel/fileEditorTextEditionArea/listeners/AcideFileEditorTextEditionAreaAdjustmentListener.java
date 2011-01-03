/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.Adjustable;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

import javax.swing.SwingUtilities;

/**
 * Editor panel adjustment listener.
 * 
 * @version 0.8
 * @see AdjustmentListener
 */
public class AcideFileEditorTextEditionAreaAdjustmentListener implements
		AdjustmentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt
	 * .event.AdjustmentEvent)
	 */
	@Override
	public void adjustmentValueChanged(final AdjustmentEvent adjustmentEvent) {

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				final AcideFileEditorPanel selectedEditor = MainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel();
				final Adjustable source = adjustmentEvent.getAdjustable();

				if (adjustmentEvent.getValueIsAdjusting()) {

					// VERTICAL VALUE 1
					selectedEditor
							.getTextEditionPanelList()
							.get(0)
							.setVerticalValue(
									selectedEditor.getTextEditionPanelList()
											.get(0).getScrollPane()
											.getVerticalScrollBar().getValue());

					// HORIZONTAL VALUE 1
					selectedEditor
							.getTextEditionPanelList()
							.get(0)
							.setHorizontalValue(
									selectedEditor.getTextEditionPanelList()
											.get(0).getScrollPane()
											.getHorizontalScrollBar()
											.getValue());

					// VERTICAL VALUE 2
					selectedEditor
							.getTextEditionPanelList()
							.get(1)
							.setVerticalValue(
									selectedEditor.getTextEditionPanelList()
											.get(1).getScrollPane()
											.getVerticalScrollBar().getValue());

					// HORIZONTAL VALUE 2
					selectedEditor
							.getTextEditionPanelList()
							.get(1)
							.setHorizontalValue(
									selectedEditor.getTextEditionPanelList()
											.get(1).getScrollPane()
											.getHorizontalScrollBar()
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

				if (selectedEditor != null) {
					// EDITOR1 is focused
					if (selectedEditor.getTextEditionPanelList().get(0)
							.getScrollPane().isFocusOwner()
							|| selectedEditor.getTextEditionPanelList().get(0)
									.getScrollPane().getVerticalScrollBar()
									.isFocusOwner()
							|| selectedEditor.getTextEditionPanelList().get(0)
									.getScrollPane().getHorizontalScrollBar()
									.isFocusOwner()) {

						// Updates the values
						selectedEditor
								.getTextEditionPanelList()
								.get(1)
								.getScrollPane()
								.getVerticalScrollBar()
								.setValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(1).getVerticalValue());
						selectedEditor
								.getTextEditionPanelList()
								.get(1)
								.getScrollPane()
								.getHorizontalScrollBar()
								.setValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(1).getHorizontalValue());
						selectedEditor
								.getTextEditionPanelList()
								.get(0)
								.setVerticalValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(0).getScrollPane()
												.getVerticalScrollBar()
												.getValue());
						selectedEditor
								.getTextEditionPanelList()
								.get(0)
								.setHorizontalValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(0).getScrollPane()
												.getHorizontalScrollBar()
												.getValue());
					}

					// EDITOR2 is focused
					if (selectedEditor.getTextEditionPanelList().get(1)
							.getScrollPane().isFocusOwner()
							|| selectedEditor.getTextEditionPanelList().get(1)
									.getScrollPane().getVerticalScrollBar()
									.isFocusOwner()
							|| selectedEditor.getTextEditionPanelList().get(1)
									.getScrollPane().getHorizontalScrollBar()
									.isFocusOwner()) {

						// Updates the values
						selectedEditor
								.getTextEditionPanelList()
								.get(0)
								.getScrollPane()
								.getVerticalScrollBar()
								.setValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(0).getVerticalValue());
						selectedEditor
								.getTextEditionPanelList()
								.get(0)
								.getScrollPane()
								.getHorizontalScrollBar()
								.setValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(0).getHorizontalValue());
						selectedEditor
								.getTextEditionPanelList()
								.get(1)
								.setVerticalValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(1).getScrollPane()
												.getVerticalScrollBar()
												.getValue());
						selectedEditor
								.getTextEditionPanelList()
								.get(1)
								.setHorizontalValue(
										selectedEditor
												.getTextEditionPanelList()
												.get(1).getScrollPane()
												.getHorizontalScrollBar()
												.getValue());
					}
				}
			}
		});
	}
}
