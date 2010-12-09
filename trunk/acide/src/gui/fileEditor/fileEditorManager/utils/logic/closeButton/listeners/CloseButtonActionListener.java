package gui.fileEditor.fileEditorManager.utils.logic.closeButton.listeners;

import es.text.TextFile;
import gui.fileEditor.fileEditorManager.utils.logic.closeButton.CloseButton;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.util.ResourceBundle;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.plaf.UIResource;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Close button action listener.
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
 * @see UIResource
 * @see AbstractAction
 ***********************************************************************/
public class CloseButtonActionListener extends AbstractAction {

	/**
	 * Close button action listener class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Close button editor index.
	 */
	private int _index;

	/**
	 * Creates a new close button action listener.
	 * 
	 * @param index
	 *            close button editor index.
	 */
	public CloseButtonActionListener(int index) {
		super();
		_index = index;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event
	 * .ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		boolean isCancelOption = false;

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Is the file modified?
		if (MainWindow.getInstance().getFileEditorManager().isRedButton(_index)) {

			// Ask the user if he wants to save it
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s643"), labels.getString("s994"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (chosenOption == JOptionPane.OK_OPTION) {

				// Is it the new file?
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(_index).getAbsolutePath()
						.equals(labels.getString("s79"))) {

					TextFile textFile = AcideIOFactory.getInstance()
							.buildFile();
					String filePath = " ";
					filePath = textFile.write();

					if (!filePath.equals(" ")) {

						// Saves the file
						boolean savingResult = textFile.save(filePath,
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(_index)
										.getTextEditionAreaContent());

						// If it could save the file?
						if (savingResult) {

							// Sets the green button
							MainWindow.getInstance().getFileEditorManager()
									.setGreenButtonAt(_index);

							// Sets the path
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.setAbsolutePath(filePath);

							// Sets the tool type text
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.setToolTipText(filePath);

							// Gets the name
							int index = filePath.lastIndexOf("\\");
							if (index == -1)
								index = filePath.lastIndexOf("/");
							String name = filePath.substring(index + 1,
									filePath.length());
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index).setName(name);

							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setMessage("");
						}
					}
				} else {

					// Is not the new file

					TextFile textFile = AcideIOFactory.getInstance()
							.buildFile();

					// Saves the file
					boolean savingResult = textFile.save(MainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(_index).getAbsolutePath(),
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.getTextEditionAreaContent());

					// If it could save it
					if (savingResult)

						// Updates the closing button
						MainWindow.getInstance().getFileEditorManager()
								.setGreenButtonAt(_index);
				}

				// Sets opened to false to the project file
				for (int filPos = 0; filPos < MainWindow.getInstance()
						.getProjectConfiguration().getFileListSize(); filPos++) {

					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(filPos)
							.getPath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.getAbsolutePath())) {
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(filPos).setIsOpened(false);
					}
				}

				// If it is not the default project
				if (!MainWindow.getInstance().getProjectConfiguration()
						.isDefaultProject())

					// Sets the project to modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

				// Removes the tab
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(_index);

			} else if (chosenOption == JOptionPane.NO_OPTION) {

				// Removes the tab
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(_index);

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage("");
			}
			else
				if(chosenOption == JOptionPane.CANCEL_OPTION)
					isCancelOption = true;
		} else {

			// Is not modified

			// Sets opened to false to the project file
			for (int filePos = 0; filePos < MainWindow.getInstance()
					.getProjectConfiguration().getFileListSize(); filePos++) {

				if (MainWindow
						.getInstance()
						.getProjectConfiguration()
						.getFileAt(filePos)
						.getPath()
						.equals(MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(_index).getAbsolutePath())) {
					MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(filePos).setIsOpened(false);
				}
			}

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject())

				// Sets the project ot modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

			// Removes the tab
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(_index);

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage("");
		}

		// No more tabs?
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getTabCount() == 0) {

			// Disables the file menu
			MainWindow.getInstance().getMenu().disableFileMenu();

			// Disables the edit menu
			MainWindow.getInstance().getMenu().disableEditMenu();
		}

		if (!isCancelOption) {
			
			// Exchanges the closing buttons
			for (int index = _index; index < MainWindow.getInstance()
					.getFileEditorManager().getTestPlaf().getCloseButtons()
					.size() - 1; index++) {

				CloseButton closeButton = (CloseButton) MainWindow
						.getInstance().getFileEditorManager().getTestPlaf()
						.getCloseButtons().get(index);
				CloseButton nextCloseButton = (CloseButton) MainWindow
						.getInstance().getFileEditorManager().getTestPlaf()
						.getCloseButtons().get(index + 1);

				// Sets the button color
				if (nextCloseButton.isRedButton())
					closeButton.setRedCloseButton();
				else
					closeButton.setGreenCloseButton();

				// Sets the position
				MainWindow.getInstance().getFileEditorManager().getTestPlaf()
						.getCloseButtons().set(index, closeButton);
			}
		}
	}
}
