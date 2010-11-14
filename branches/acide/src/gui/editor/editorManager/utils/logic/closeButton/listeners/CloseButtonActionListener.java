package gui.editor.editorManager.utils.logic.closeButton.listeners;

import es.text.TextFile;
import gui.editor.editorManager.utils.logic.closeButton.CloseButton;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.util.ResourceBundle;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.plaf.UIResource;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************
 * Close button action
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
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Index of the button
	 */
	private int _index;

	/**
	 * Class constructor
	 * 
	 * @param index
	 *            index of the button
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
	public void actionPerformed(ActionEvent e) {

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
		final ResourceBundle labels = language.getLabels();

		// Is the file modified?
		if (MainWindow.getInstance().getEditorManager().isRedButton(_index)) {

			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s643"), labels.getString("s994"),
					JOptionPane.YES_NO_OPTION);

			if (chosenOption == JOptionPane.OK_OPTION) {

				// Is it the new file?
				if (MainWindow.getInstance().getEditorManager()
						.getEditorAt(_index).getAbsolutePath()
						.equals(labels.getString("s79"))) {

					TextFile textFile = IOFactory.getInstance().buildFile();
					String filePath = " ";
					filePath = textFile.write();

					if (!filePath.equals(" ")) {

						// Saves the file
						boolean savingResult = textFile.save(filePath,
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(_index).getText());

						// If it could save the file?
						if (savingResult) {

							// Sets the green button
							MainWindow.getInstance().getEditorManager()
									.setGreenButtonAt(_index);

							// Sets the path
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(_index)
									.setAbsolutePath(filePath);

							// Sets the tool type text
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(_index)
									.setToolTipText(filePath);

							// Gets the name
							int index = filePath.lastIndexOf("\\");
							if (index == -1)
								index = filePath.lastIndexOf("/");
							String name = filePath.substring(index + 1,
									filePath.length());
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(_index).setName(name);

							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setMessage("");
						}
					}
				} else {

					// Is not the new file

					TextFile textFile = IOFactory.getInstance().buildFile();

					// Saves the file
					boolean savingResult = textFile.save(
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(_index).getAbsolutePath(),
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(_index).getText());

					// If it could save it
					if (savingResult)

						// Updates the closing button
						MainWindow.getInstance().getEditorManager()
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
							.equals(MainWindow.getInstance().getEditorManager()
									.getEditorAt(_index).getAbsolutePath())) {
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
				MainWindow.getInstance().getEditorManager().getPane()
						.remove(_index);

			} else if (chosenOption == JOptionPane.NO_OPTION) {

				// Removes the tab
				MainWindow.getInstance().getEditorManager().getPane()
						.remove(_index);

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage("");
			}
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
						.equals(MainWindow.getInstance().getEditorManager()
								.getEditorAt(_index).getAbsolutePath())) {
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
			MainWindow.getInstance().getEditorManager().getPane()
					.remove(_index);

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage("");
		}

		// No more tabs?
		if (MainWindow.getInstance().getEditorManager().getPane().getTabCount() == 0) {

			// Disables the file menu
			MainWindow.getInstance().getMenu().disableFileMenu();

			// Disables the edit menu
			MainWindow.getInstance().getMenu().disableEditMenu();
		}

		// Exchanges the closing buttons
		for (int pos = _index; pos < MainWindow.getInstance()
				.getEditorManager().getTestPlaf().getCloseButtons().size() - 1; pos++) {

			CloseButton closeButton = (CloseButton) MainWindow.getInstance()
					.getEditorManager().getTestPlaf().getCloseButtons()
					.get(pos);
			CloseButton nextCloseButton = (CloseButton) MainWindow
					.getInstance().getEditorManager().getTestPlaf()
					.getCloseButtons().get(pos + 1);

			// Sets the button color
			if (nextCloseButton.isRedButton())
				closeButton.setRedButton();
			else
				closeButton.setGreenButton();

			// Sets the position
			MainWindow.getInstance().getEditorManager().getTestPlaf()
					.getCloseButtons().set(pos, closeButton);
		}
	}
}
