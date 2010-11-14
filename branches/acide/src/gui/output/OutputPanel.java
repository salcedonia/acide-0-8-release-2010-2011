package gui.output;

import es.configuration.output.OutputConfiguration;
import gui.mainWindow.MainWindow;
import gui.output.listeners.OutputPanelKeyboardListener;
import gui.output.listeners.OutputPanelPopupMenuListener;
import gui.output.popup.OutputPanelPopupMenu;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.IOException;
import java.util.ArrayList;
import java.util.ResourceBundle;

import language.Language;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;

import operations.listeners.*;
import operations.log.Log;
import operations.output.OutputThread;
import properties.PropertiesManager;

/************************************************************************																
 * Output panel of ACIDE - A Configurable IDE										
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
 * @see JPanel																													
 ***********************************************************************/
public class OutputPanel extends JPanel {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Text component of the output
	 */
	private JTextComponent _textComponent;
	/**
	 * Scroll pane for the output
	 */
	private JScrollPane _scrollPane;
	/**
	 * Text handler for the output
	 */
	private DefaultStyledDocument _defaultStyledDocument;
	/**
	 * Process thread for the output
	 */
	private OutputThread _processThread;
	/**
	 * Popup menu for the output
	 */
	private OutputPanelPopupMenu _popupMenu;
	/**
	 * Echo command to display
	 */
	private String _echoCommand;
	/**
	 * Prompt caret position
	 */
	private int _promptCaretPosition;
	/**
	 * Selection size
	 */
	private int _selectionSize;
	/**
	 * Index of the selected document
	 */
	private int _historicCurrentIndex;
	/**
	 * Maximum value for the index
	 */
	private int _historicMaximumIndex;
	/**
	 * Historic with the commands typed by the user
	 */
	private ArrayList<String> _historic;

	/**
	 * Class constructor
	 */
	public OutputPanel(boolean editable) {

		super();

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

		try {

			// Updates the log
			Log.getLog().info(labels.getString("s422"));

			_textComponent = buildOutput();
			_textComponent.setEditable(editable);
			_echoCommand = "";
			_promptCaretPosition = 0;
			_selectionSize = 0;

			// Listeners
			_textComponent.addKeyListener(new OutputPanelKeyboardListener());
			_textComponent.addKeyListener(new AcideKeyboardListenerForMenus());
			_textComponent.addCaretListener(new CaretListener() {
				/**
				 * Update the caret position, setting it always after the prompt
				 * except when it is making the selection for copying the
				 * content
				 * 
				 * @param caretEvent
				 *            caret event
				 * @see CaretEvent
				 */
				public void caretUpdate(CaretEvent caretEvent) {
					if (_textComponent.getCaretPosition() < _textComponent
							.getText().length() - _selectionSize) {
						_textComponent.setCaretPosition(_textComponent
								.getText().length());
					}
				}
			});

			_textComponent.setFont(new Font("Monospaced", Font.PLAIN, 12));
			_textComponent.setForeground(Color.BLACK);
			_textComponent.setCaretColor(Color.BLACK);
			_textComponent.setBackground(Color.WHITE);

			setLayout(new BorderLayout());

			_scrollPane = new JScrollPane(_textComponent);
			add(_scrollPane);

			// Updates the log
			Log.getLog().info(labels.getString("s423"));

		} catch (Exception exception) {

			// Updates the log
			Log.getLog().info(labels.getString("s424"));
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// HISTORIC
		_historic = new ArrayList<String>();
		_historicCurrentIndex = 0;
		_historicMaximumIndex = 0;

		// Text component listener
		_textComponent.addMouseListener(new OutputPanelPopupMenuListener());

		// INIT THE POPUP MENU
		buildPopupMenu();
	}

	/**
	 * Updates the output visualization
	 */
	public void updateVisualization() {

		int fontStyle = 0;

		if (OutputConfiguration.getInstance().getFontStyle()
				.matches("Font.PLAIN"))
			fontStyle = Font.PLAIN;
		if (OutputConfiguration.getInstance().getFontStyle()
				.matches("Font.BOLD"))
			fontStyle = Font.BOLD;
		if (OutputConfiguration.getInstance().getFontStyle()
				.matches("Font.ITALIC"))
			fontStyle = Font.ITALIC;
		if (OutputConfiguration.getInstance().getFontStyle()
				.matches("Font.BOLD+Font.ITALIC"))
			fontStyle = Font.BOLD + Font.ITALIC;

		// IMPORTANT: Waits a little time for repainting
		try {
			Thread.sleep(1000);
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		setFontName(new Font(OutputConfiguration.getInstance().getFontName(),
				fontStyle, OutputConfiguration.getInstance().getFontSize()));
		setForegroundColor(OutputConfiguration.getInstance()
				.getForegroundColor());
		setBackgroundColor(OutputConfiguration.getInstance()
				.getBackgroundColor());
		_textComponent.setCaretColor(OutputConfiguration.getInstance()
				.getForegroundColor());
	}

	/**
	 * Builds the output popup menu
	 */
	public void buildPopupMenu() {
		_popupMenu = new OutputPanelPopupMenu();
	}

	/**
	 * Builds the output text component
	 * 
	 * @return the output text component
	 * @see JTextComponent
	 */
	protected JTextComponent buildOutput() {

		_defaultStyledDocument = new DefaultStyledDocument();
		JTextPane textArea = new JTextPane(_defaultStyledDocument) {

			/**
			 * Class serial version UID
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
			@Override
			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;

				super.setSize(d);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see javax.swing.JEditorPane#getScrollableTracksViewportWidth()
			 */
			@Override
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};
		return textArea;
	}

	/**
	 * Adds a text given as a parameter to the output text
	 * 
	 * @param text
	 *            text to add
	 */
	public void addText(String text) {

		try {
			if (OutputConfiguration.getInstance().getEchoCommand()
					&& (_echoCommand.length() <= text.length())) {
				if (text.substring(0, _echoCommand.length()).equals(
						_echoCommand)) {
					text = text.substring(_echoCommand.length(), text.length());
				}
			}
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		String auxText = _textComponent.getText();
		auxText = auxText + text;
		_textComponent.setText(auxText);
		_promptCaretPosition = _textComponent.getText().length();
		_textComponent.setCaretPosition(_textComponent.getText().length());
	}

	/**
	 * Executes the exit command for the output
	 */
	public void executeExitCommand() {

		try {

			String exitCommand = OutputConfiguration.getInstance()
					.getExitCommand();
			_echoCommand = exitCommand;

			if (_processThread.getWriter() != null) {
				_processThread.getWriter().write(exitCommand + '\n');
				_processThread.getWriter().flush();
			}

		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Executes a command in the output shell given as a parameter
	 * 
	 * @param command
	 *            command to execute
	 */
	public void executeCommand(String command) {

		if (_processThread.getWriter() != null) {
			try {

				if (MainWindow.getInstance().getEditorManager().getNumEditors() > 0) {
					command = command.replace("$activeFile$", MainWindow
							.getInstance().getEditorManager()
							.getSelectedEditor().getAbsolutePath());
					command = command.replace("$activeFilePath$", MainWindow
							.getInstance().getEditorManager()
							.getSelectedEditor().getFilePath());
					command = command.replace("$activeFileExt$", MainWindow
							.getInstance().getEditorManager()
							.getSelectedEditor().getFileExtension());
					command = command.replace("$activeFileName$", MainWindow
							.getInstance().getEditorManager()
							.getSelectedEditor().getFileName());
				}

				// Default project
				if (MainWindow.getInstance().getProjectConfiguration()
						.isDefaultProject()) {

					// If it has a MAIN FILE
					if (MainWindow.getInstance().getEditorManager()
							.getMainEditor() != null) {

						command = command.replace("$mainFile$", MainWindow
								.getInstance().getEditorManager()
								.getMainEditor().getAbsolutePath());
						command = command.replace("$mainFilePath$", MainWindow
								.getInstance().getEditorManager()
								.getMainEditor().getFilePath());
						command = command.replace("$mainFileExt$", MainWindow
								.getInstance().getEditorManager()
								.getMainEditor().getFileExtension());
						command = command.replace("$mainFileName$", MainWindow
								.getInstance().getEditorManager()
								.getMainEditor().getFileName());
					}
				} else {

					// Not default project

					// Searches for the MAIN FILE
					int posMainEditor = -1;
					for (int pos = 0; pos < MainWindow.getInstance()
							.getProjectConfiguration().getNumFilesFromList(); pos++) {
						if (MainWindow.getInstance().getProjectConfiguration().getFileAt(pos)
								.isMainFile())
							posMainEditor = pos;
					}

					// If exists
					if (posMainEditor != -1) {
						command = command.replace(
								"$mainFile$",
								MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(posMainEditor).getPath());
						command = command
								.replace("$mainFilePath$",
										MainWindow.getInstance().getProjectConfiguration()
												.getFileAt(posMainEditor)
												.getFilePath());
						command = command.replace(
								"$mainFileExt$",
								MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(posMainEditor).getFileExt());
						command = command
								.replace("$mainFileName$",
										MainWindow.getInstance().getProjectConfiguration()
												.getFileAt(posMainEditor)
												.getFileName());
					}
				}

				// Executes the command
				sendCommandToOutput(command);

			} catch (Exception exception) {
				
				// Updates the log
				Log.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * Send the command to the output writer so it can execute the command
	 * 
	 * @param command
	 *            command to execute in the output writer
	 */
	public void sendCommandToOutput(String command) {

		try {
			_selectionSize = 0;
			_echoCommand = command;

			// Send the command to the output shell
			_processThread.getWriter().write(command + "\n");
			_processThread.getWriter().flush();

			// Updates the historic
			updateHistoric(command);
			
		} catch (IOException exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the output historic with the new executed command
	 * 
	 * @param command
	 *            Executed command
	 */
	public void updateHistoric(String command) {

		// Sets the care at the end of the command
		_textComponent.setCaretPosition(_promptCaretPosition);

		// Updates the historic
		if (_historic.contains(command)) {
			_historicMaximumIndex--;
			_historic.remove(command);
		}
		_historic.add(command);
		_historicMaximumIndex++;
		_historicCurrentIndex = _historicMaximumIndex;
	}

	/**
	 * Returns the text component
	 * 
	 * @return the text component
	 */
	public JTextComponent getTextComponent() {
		return _textComponent;
	}

	/**
	 * Returns the text of the output shell
	 * 
	 * @return the text of the output shell
	 */
	public String getText() {
		return _textComponent.getText();
	}

	/**
	 * Executes the output process thread
	 */
	public void execute() {
		_processThread = new OutputThread();
		_processThread.start();
	}

	/**
	 * Resets the output
	 */
	public void resetOutput() {
		_textComponent.setText("");
		execute();
	}

	/**
	 * Returns the prompt caret position
	 * 
	 * @return the prompt caret position
	 */
	public int getPromptCaretPosition() {
		return _promptCaretPosition;
	}

	/**
	 * Sets a new value to the font name
	 * 
	 * @param fontName
	 *            new value to set
	 */
	public void setFontName(Font font) {
		_textComponent.setFont(font);
		repaint();
	}

	/**
	 * Sets a new value to the foreground color
	 * 
	 * @param foregroundColor
	 *            new value to set
	 */
	public void setForegroundColor(Color foregroundColor) {
		_textComponent.setForeground(foregroundColor);
		_textComponent.setCaretColor(foregroundColor);
		repaint();
	}

	/**
	 * Sets a new value to the background color
	 * 
	 * @param backgroundColor
	 *            new value to set
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_textComponent.setBackground(backgroundColor);
		repaint();
	}

	/**
	 * Returns the selection size
	 * 
	 * @return the selection size
	 */
	public int getSelectionSize() {
		return _selectionSize;
	}
	
	/**
	 * Sets a new value to the selection size
	 * 
	 * @param selectionSize new value to set
	 */
	public void setSelectionSize(int selectionSize){
		_selectionSize = selectionSize;
	}
	
	/**
	 * Returns the default styled document
	 * 
	 * @return the default styled document
	 */
	public DefaultStyledDocument getDefaultStyledDocument(){
		return _defaultStyledDocument;
	}
	
	/**
	 * Sets a new value to the default styled document
	 * 
	 * @param defaultStyledDocument new value to set
	 */
	public void setDefaultStyledDocument(DefaultStyledDocument defaultStyledDocument){
		_defaultStyledDocument = defaultStyledDocument;
	}

	/**
	 * Returns the output process thread
	 * 
	 * @return the output process thread
	 */
	public OutputThread getProcessThread() {
		return _processThread;
	}
	
	/**
	 * Returns the output historic
	 * 
	 * @return the output historic
	 */
	public ArrayList<String> getHistoric(){
		return _historic;
	}
	
	/**
	 * Returns the historic current index
	 * 
	 * @return the historic current index
	 */
	public int getHistoricCurrentIndex(){
		return _historicCurrentIndex;
	}
	
	/**
	 * Sets a new value to the historic current index
	 * 
	 * @param historicCurrentIndex new value to set
	 */
	public void setHistoricCurrentIndex(int historicCurrentIndex){
		_historicCurrentIndex = historicCurrentIndex;
	}

	/**
	 * Returns the historic maximum index
	 * 
	 * @return the historic maximum index
	 */
	public int getHistoricMaximumIndex() {
		return _historicMaximumIndex;
	}
	
	/**
	 * Sets the historic maximum index
	 * 
	 * @param historicMaximumIndex the historic maximum index
	 */
	public void setHistoricMaximumIndex(int historicMaximumIndex){
		_historicMaximumIndex = historicMaximumIndex;
	}
	
	/**
	 * Returns the output panel popup menu
	 * 
	 * @return the output panel popup menu
	 */
	public OutputPanelPopupMenu getPopupMenu(){
		return _popupMenu;
	}
}
