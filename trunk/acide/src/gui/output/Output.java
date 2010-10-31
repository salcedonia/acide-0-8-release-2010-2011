package gui.output;

import gui.MainWindow;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.ResourceBundle;

import language.Language;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;

import operations.listeners.*;
import operations.log.Log;
import operations.output.ProcessThread;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * Output shell of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class Output extends JPanel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Text component of the output.
	 */
	private JTextComponent _textComponent;
	/**
	 * Scroll pane for the output.
	 */
	private JScrollPane _scrollPane;
	/**
	 * Text handler for the output.
	 */
	private DefaultStyledDocument _text;
	/**
	 * Thread process for the output.
	 */
	private ProcessThread _processThread;
	/**
	 * Popup menu for the output.
	 */
	private PopupMenuOutput _popupMenu;
	/**
	 * Echo command to display.
	 */
	private String _echoCommand;
	/**
	 * Maximum position for the caret.
	 */
	private int _maxCaretPosition;
	/**
	 * Selection size.
	 */
	private int _selectionSize;
	/**
	 * Index of the selected document.
	 */
	private int _index;
	/**
	 * Maximum value for the index.
	 */
	private int _maxIndex;
	/**
	 * Historic with the commands typed by the user.
	 */
	private ArrayList<String> _historic;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	

	/**
	 * Constructor of the class.
	 */
	public Output(boolean editable) {

		super();

		ResourceBundle labels = Language.getInstance().getLabels();

		try {
			_logger.info(labels.getString("s422"));

			_textComponent = buildOutput();
			_textComponent.setEditable(editable);
			_echoCommand = "";
			_maxCaretPosition = 0;
			_selectionSize = 0;
			_textComponent.addKeyListener(new OutputKeyboardListener());
			_textComponent.addKeyListener(new AcideKeyboardListenerForMenus());
			_textComponent.setFont(new Font("Monospaced", Font.PLAIN, 12));
			_processThread = new ProcessThread();

			setLayout(new BorderLayout());

			_scrollPane = new JScrollPane(_textComponent);
			add(_scrollPane);

			_logger.info(labels.getString("s423"));

			/*
			 * salida.addCaretListener(new CaretListener() { public void
			 * caretUpdate(CaretEvent e) { if
			 * (salida.getCaretPosition()<salida.getText().length()-longCom){
			 * salida.setCaretPosition(salida.getText().length()); } } });
			 */

		} catch (Exception e) {

			// FAIL TO CREATE THE OUTPUT
			_logger.info(labels.getString("s424"));
			e.printStackTrace();
		}
		_historic = new ArrayList<String>();
		_index = 0;
		_maxIndex = 0;
		_textComponent.addMouseListener(new OutputMouseListener());

		buildPopupMenu(labels);
	}

	/**
	 * Build the popup menu.
	 * 
	 * @param labels
	 *            Labels to display.
	 */
	private void buildPopupMenu(ResourceBundle labels) {

		_popupMenu = new PopupMenuOutput();
	}

	/**
	 * Build the output.
	 * 
	 * @return The initialized component.
	 */
	protected JTextComponent buildOutput() {

		_text = new DefaultStyledDocument();
		JTextPane textArea = new JTextPane(_text) {

			/**
			 * serialVersionUID.
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
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
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};
		return textArea;
	}

	/**
	 * Add a text given as a parameter to the output text.
	 *  
	 * @param text Text to add.
	 */
	public void addText(String text) {
		try {
			if (Boolean.parseBoolean(PropertiesManager
					.getProperty("echoCommand"))
					&& (_echoCommand.length() <= text.length())) {
				if (text.substring(0, _echoCommand.length()).equals(
						_echoCommand)) {
					text = text.substring(_echoCommand.length(), text.length());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		String auxText = _textComponent.getText();
		auxText = auxText + text;
		_textComponent.setText(auxText);
		_maxCaretPosition = _textComponent.getText().length();
		_textComponent.setCaretPosition(_textComponent.getText().length());
	}

	/**
	 * Executes the exit command for the output.
	 */
	public void executeExitCommand() {
		
		try {
			
			String exitCommand = PropertiesManager.getProperty("exitCommand");
			_echoCommand = exitCommand;
			
			if (ProcessThread.getWriter() != null) {
				ProcessThread.getWriter().write(exitCommand + '\n');
				ProcessThread.getWriter().flush();
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Executes a command in the output shell given as a parameter.
	 * 
	 * @param command
	 *            Command to execute.
	 */
	public void executeCommand(String command) {
		try {

			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0) {
				command = command.replace("$activeFile$", MainWindow
						.getInstance().getEditorBuilder().getSelectedEditor()
						.getAbsolutePath());
				command = command.replace("$activeFilePath$", MainWindow
						.getInstance().getEditorBuilder().getSelectedEditor()
						.getFilePath());
				command = command.replace("$activeFileExt$", MainWindow
						.getInstance().getEditorBuilder().getSelectedEditor()
						.getFileExtension());
				command = command.replace("$activeFileName$", MainWindow
						.getInstance().getEditorBuilder().getSelectedEditor()
						.getFileName());
			}

			// GET THE MAIN WINDOW
			MainWindow mainWindow = MainWindow.getInstance();

			// GET THE DEFAULT CONFIGURATION
			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// IF THERE IS NOT AN OPENED PROJECT
			if (project.equals("./configuration/default.acidePrj")
					&& mainWindow.getProjectConfiguration().getName()
							.equals("")) {

				// IF IT HAS A MAIN FILE
				if (MainWindow.getInstance().getEditorBuilder().getMainEditor() != null) {
					
					command = command.replace("$mainFile$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getAbsolutePath());
					command = command.replace("$mainFilePath$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getFilePath());
					command = command.replace("$mainFileExt$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getFileExtension());
					command = command.replace("$mainFileName$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getFileName());
				}
			} else {
				
				// THERE IS AN OPENED PROJECT
				
				// SEARCH THE MAIN FILE
				int posMainEditor = -1;
				for (int pos = 0; pos < mainWindow.getProjectConfiguration()
						.getNumFilesFromList(); pos++) {
					if (mainWindow.getProjectConfiguration().getFileAt(pos)
							.isMainFile())
						posMainEditor = pos;
				}
				
				// IF EXISTS THE MAIN FILE
				if (posMainEditor != -1) {
					command = command.replace("$mainFile$", mainWindow
							.getProjectConfiguration().getFileAt(posMainEditor).getPath());
					command = command.replace("$mainFilePath$", mainWindow
							.getProjectConfiguration().getFileAt(posMainEditor)
							.getFilePath());
					command = command.replace("$mainFileExt$", mainWindow
							.getProjectConfiguration().getFileAt(posMainEditor)
							.getFileExt());
					command = command.replace("$mainFileName$", mainWindow
							.getProjectConfiguration().getFileAt(posMainEditor)
							.getFileName());
				}
			}
			
			addText(command + '\n');
			_echoCommand = command;
			ProcessThread.getWriter().write(command + '\n');
			ProcessThread.getWriter().flush();

			// Process p = Runtime.getRuntime().exec (com + '\n');
			// InputStream is = p.getInputStream();

			// BufferedReader br = new BufferedReader (new InputStreamReader
			// (is));

			// System.out.println("m"+br.readLine());

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * Returns the text component.
	 * 
	 * @return The text component.
	 */
	public JTextComponent getTextComponent() {
		return _textComponent;
	}
	
	/**
	 * Returns the text of the output shell.
	 * 
	 * @return The text of the output shell.
	 */
	public String getText() {
		return _textComponent.getText();
	}

	/**
	 * Executes the output.
	 */
	@SuppressWarnings("static-access")
	public void execute() {
		String args[] = null;
		_processThread.main(args);
	}

	/**
	 * Reset the output.
	 */
	public void resetOutput() {
		_textComponent.setText("");
		execute();
	}

	/**
	 * Returns the maximum caret position.
	 * 
	 * @return The maximum caret position.
	 */
	public int getMaxCaretPosition() {
		return _maxCaretPosition;
	}
	

	/**
	 * Key listener for the output shell of the application.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class OutputKeyboardListener implements KeyListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
		 */
		public void keyTyped(KeyEvent arg0) {

			if (MainWindow.getInstance().getOutput().getTextComponent() != null) {

				if (_textComponent.getCaretPosition() < _maxCaretPosition)
					arg0.consume();
				else
					_selectionSize++;
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
		 */
		public void keyPressed(KeyEvent arg0) {

			if (MainWindow.getInstance().getOutput().getTextComponent() != null) {

				if ((_textComponent.getCaretPosition() == _maxCaretPosition)
						&& ((arg0.getKeyCode() == KeyEvent.VK_LEFT)
								|| (arg0.getKeyCode() == 8) || (arg0
								.getKeyCode() == KeyEvent.VK_UP))) {
					arg0.consume();
				}
				if (_textComponent.getSelectionStart() < _maxCaretPosition) {
					arg0.consume();
				}

				String command = (String) _textComponent.getText().subSequence(
						_maxCaretPosition, _text.getLength());

				switch (arg0.getKeyCode()) {

				case KeyEvent.VK_ENTER:

					if (arg0.getKeyChar() == '\n') {

						try {
							_selectionSize = 0;
							_echoCommand = command;

							ProcessThread.getWriter().write(
									command + arg0.getKeyChar());
							ProcessThread.getWriter().flush();

							_maxCaretPosition = _maxCaretPosition
									+ command.length();
							_textComponent.setCaretPosition(_maxCaretPosition);
							if (_historic.contains(command)) {
								_maxIndex--;
								_historic.remove(command);
							}
							_historic.add(command);
							_maxIndex++;
							_index = _maxIndex;
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
					break;

				case KeyEvent.VK_UP:

					arg0.consume();

					// historic
					if (_index > -1) {

						if (_index == 0)
							_index = _maxIndex - 1;
						else
							_index--;

						try {
							_text.remove(_maxCaretPosition, command.length());
							if (_index > -1)
								_text.insertString(_maxCaretPosition,
										_historic.get(_index), null);
						} catch (BadLocationException e) {
							e.printStackTrace();
						}
					}
					break;

				case KeyEvent.VK_DOWN:

					// historic
					if (_index > -1) {
						if (_index >= _maxIndex - 1)
							_index = 0;
						else
							_index++;

						try {
							_text.remove(_maxCaretPosition, command.length());
							_text.insertString(_maxCaretPosition,
									_historic.get(_index), null);
						} catch (BadLocationException e) {
							e.printStackTrace();
						}
					}
					break;

				case KeyEvent.VK_ESCAPE:

					try {
						_text.remove(_maxCaretPosition, command.length());
					} catch (BadLocationException e) {
						e.printStackTrace();
					}

					break;

				case KeyEvent.VK_HOME:

					arg0.consume();
					_textComponent.setCaretPosition(_maxCaretPosition);

					break;

				case KeyEvent.VK_END:

					arg0.consume();
					_textComponent.setCaretPosition(_maxCaretPosition
							+ command.length());

					break;

				case KeyEvent.VK_C:

					// CTRL + C --> COPY
					if (arg0.isControlDown())
						_textComponent.copy();

					/*
					 * try { ProcessThread.getWriter().write(arg0.getKeyCode());
					 * ProcessThread.getWriter().flush(); } catch (IOException
					 * e) { e.printStackTrace(); }
					 */
					break;
				}
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
		 */
		public void keyReleased(KeyEvent arg0) {
			if (_textComponent.getCaretPosition() < _maxCaretPosition) {
				arg0.consume();
			}
		}
	}

	/**
	 * Mouse listener for the output shell of the application.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class OutputMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			if (arg0.isPopupTrigger()) {
				_popupMenu.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
		 */
		public void mouseReleased(MouseEvent arg0) {
			
			if (arg0.isPopupTrigger()) {

				if (_textComponent.getSelectedText() == null) {
					_popupMenu.getCopy().setEnabled(false);
					_popupMenu.getCut().setEnabled(false);
				} else {
					_popupMenu.getCopy().setEnabled(true);
					if (_textComponent.getSelectionStart() < _maxCaretPosition)
						_popupMenu.getCut().setEnabled(false);
					else {
						_popupMenu.getCut().setEnabled(true);
					}
				}
				if (_textComponent.getSelectionStart() < _maxCaretPosition)
					_popupMenu.getPaste().setEnabled(false);
				else
					_popupMenu.getPaste().setEnabled(true);

				_popupMenu.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}
	}
}
