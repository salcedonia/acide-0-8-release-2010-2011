package gui.fileEditor.fileEditorPanel.listeners;

import gui.mainWindow.MainWindow;

import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/************************************************************************
 * Editor panel document listener.
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
 * @see DocumentListener
 ***********************************************************************/
public class AcideFileEditorPanelDocumentListener implements DocumentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void insertUpdate(DocumentEvent documentEvent) {
		updatesEditorAndProjectState(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent documentEvent) {
		updatesEditorAndProjectState(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {
		updatesEditorAndProjectState(documentEvent);
	}

	/**
	 * Updates the close button and the modification project state.
	 */
	public void updatesEditorAndProjectState(final DocumentEvent documentEvent) {

		// Gets the selected file editor panel index
		final int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanelIndex();

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				if (selectedFileEditorPanelIndex != -1) {
				
					// Gets the current content
					String fileContent = MainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedFileEditorPanelIndex)
							.getTextEditionAreaContent();

					// If has been changes in the file
					if (!MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedFileEditorPanelIndex)
							.isEqualToFileDiskCopy(fileContent)) {

						// Sets the red icon to the close button
						MainWindow.getInstance().getFileEditorManager()
								.getTestPlaf()
								.getCloseButtonAt(selectedFileEditorPanelIndex)
								.setRedCloseButton();

						// Enables the save as menu item
						MainWindow.getInstance().getMenu().getFile()
								.getSaveFileAs().setEnabled(true);

						// The project configuration has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);
					} else {

						// Sets the red icon to the close button
						MainWindow.getInstance().getFileEditorManager()
								.getTestPlaf()
								.getCloseButtonAt(selectedFileEditorPanelIndex)
								.setGreenCloseButton();

						// Enables the save as menu item
						MainWindow.getInstance().getMenu().getFile()
								.getSaveFileAs().setEnabled(false);

						// The project configuration has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(false);
					}
				}
			}
		});
	}
}
