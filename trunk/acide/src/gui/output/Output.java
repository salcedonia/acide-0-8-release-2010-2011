package gui.output;

import gui.MainWindow;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.ResourceBundle;

import language.Language;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
 * 
 */
public class Output extends JPanel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JTextComponent _output;
	/**
	 * 
	 */
	private int _maxCaretPosition;
	/**
	 * 
	 */
	private ProcessThread _processThread;
	/**
	 * 
	 */
	private String _echoCommand;
	/**
	 * 
	 */
	private int _longSelection;
	/**
	 * 
	 */
	private JPopupMenu _popup;
	/**
	 * 
	 */
	private JMenuItem _copyMenu;
	/**
	 * 
	 */
	private JMenuItem _cutMenu;
	/**
	 * 
	 */
	private JMenuItem _pasteMenu;
	/**
	 * 
	 */
	private JScrollPane _scrollPane;
	/**
	 * 
	 */
	private ArrayList<String> _historic;
	/**
	 * 
	 */
	private int _index;
	/**
	 * 
	 */
	private int _maxIndex;
	/**
	 * 
	 */
	private DefaultStyledDocument _text;

	/**
	 * Constructor of the class.
	 */
	public Output(boolean editable) {

		super();

		ResourceBundle labels = Language.getInstance().getLabels();

		try {
			_logger.info(labels.getString("s422"));

			_output = buildOutput();
			_output.setEditable(editable);
			_echoCommand = "";
			_maxCaretPosition = 0;
			_longSelection = 0;
			_output.addKeyListener(new OutputEnterKey());
			_output.addKeyListener(new AcideKeyboardListener2());
			_output.setFont(new Font("Monospaced", Font.PLAIN, 12));
			_processThread = new ProcessThread();

			setLayout(new BorderLayout());

			_scrollPane = new JScrollPane(_output);
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
		_output.addMouseListener(new OutputMouseListener());

		buildPopupMenu(labels);
	}

	/**
	 * 
	 * @param labels
	 */
	private void buildPopupMenu(ResourceBundle labels) {

		_popup = new JPopupMenu();

		_copyMenu = new JMenuItem(labels.getString("s187"));
		_copyMenu.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				_output.copy();
			}
		});
		_popup.add(_copyMenu);
		_cutMenu = new JMenuItem(labels.getString("s188"));
		_cutMenu.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				if (_output.getSelectionStart() >= _maxCaretPosition)
					_output.cut();
			}
		});
		_popup.add(_cutMenu);
		_pasteMenu = new JMenuItem(labels.getString("s189"));
		_pasteMenu.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				if (_output.getSelectionStart() >= _maxCaretPosition)
					_output.paste();
			}
		});
		_popup.add(_pasteMenu);
	}

	/**
	 * 
	 * @return
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
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;

				super.setSize(d);
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.JEditorPane#getScrollableTracksViewportWidth()
			 */
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};
		return textArea;
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public JTextComponent getOutput() {
		return _output;
	}

	/**
	 * 
	 * 
	 * @param text
	 */
	public void setText(String text) {
		// salida.setText(texto);
	}

	/**
	 * 
	 * @param text
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
		String auxText = _output.getText();
		auxText = auxText + text;
		_output.setText(auxText);
		_maxCaretPosition = _output.getText().length();
		_output.setCaretPosition(_output.getText().length());
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public String getText() {
		return _output.getText();
	}

	/**
	 * 
	 */
	@SuppressWarnings("static-access")
	public void executeCMD() {
		String args[] = null;
		_processThread.main(args);
	}

	/**
	 * 
	 */
	public void resetOutput() {
		_output.setText("");
		executeCMD();
	}

	/**
	 * 
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
	 * 
	 * @param command
	 */
	public void executeCommand(String command) {
		try {
			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0) {
				command = command.replace("$activeFile$", MainWindow
						.getInstance().getEditorBuilder().getSelectedEditor()
						.getPath());
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

			MainWindow mainWindow = MainWindow.getInstance();
			String prj = null;
			try {
				prj = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if ((prj.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {
				// No project
				if (MainWindow.getInstance().getEditorBuilder().getMainEditor() != null) {
					command = command.replace("$mainFile$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getPath());
					command = command.replace("$mainFilePath$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getFilePath());
					command = command.replace("$mainFileExt$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getFileExtension());
					command = command.replace("$mainFileName$", MainWindow
							.getInstance().getEditorBuilder().getMainEditor()
							.getFileName());
				} else {
					// no main file
				}
			} else {// project
				int j = -1;
				for (int i = 0; i < mainWindow.getProjectConfiguration()
						.getNumFilesFromList(); i++) {
					if (mainWindow.getProjectConfiguration().getFile(i)
							.isMainFile())
						j = i;
				}
				if (j != -1) {
					command = command.replace("$mainFile$", mainWindow
							.getProjectConfiguration().getFile(j).getPath());
					command = command
							.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFilePath());
					command = command.replace("$mainFileExt$", mainWindow
							.getProjectConfiguration().getFile(j).getFileExt());
					command = command
							.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileName());
				} else {
					// no main file
				}
			}
			// System.out.println(com);

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
	 * 
	 * @return
	 */
	public int getMaxCaretPosition() {
		return _maxCaretPosition;
	}

	/**
	 * 
	 */
	class OutputEnterKey implements KeyListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
		 */
		public void keyTyped(KeyEvent arg0) {

			if (MainWindow.getInstance().getOutput().getOutput() != null) {

				if (_output.getCaretPosition() < _maxCaretPosition)
					arg0.consume();
				else
					_longSelection++;
			}
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
		 */
		public void keyPressed(KeyEvent arg0) {

			if (MainWindow.getInstance().getOutput().getOutput() != null) {

				if ((_output.getCaretPosition() == _maxCaretPosition)
						&& ((arg0.getKeyCode() == KeyEvent.VK_LEFT)
								|| (arg0.getKeyCode() == 8) || (arg0
								.getKeyCode() == KeyEvent.VK_UP))) {
					arg0.consume();
				}
				if (_output.getSelectionStart() < _maxCaretPosition) {
					arg0.consume();
				}

				String command = (String) _output.getText().subSequence(
						_maxCaretPosition, _text.getLength());

				switch (arg0.getKeyCode()) {

				case KeyEvent.VK_ENTER:

					if (arg0.getKeyChar() == '\n') {

						try {
							_longSelection = 0;
							_echoCommand = command;

							ProcessThread.getWriter().write(
									command + arg0.getKeyChar());
							ProcessThread.getWriter().flush();

							_maxCaretPosition = _maxCaretPosition
									+ command.length();
							_output.setCaretPosition(_maxCaretPosition);
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
					_output.setCaretPosition(_maxCaretPosition);

					break;

				case KeyEvent.VK_END:

					arg0.consume();
					_output.setCaretPosition(_maxCaretPosition
							+ command.length());

					break;

				case KeyEvent.VK_C:

					// CTRL + C --> COPY
					if (arg0.isControlDown())
						_output.copy();

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
		 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
		 */
		public void keyReleased(KeyEvent arg0) {
			if (_output.getCaretPosition() < _maxCaretPosition) {
				arg0.consume();
			}
		}
	}

	/**
	 * 
	 */
	class OutputMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			if (arg0.isPopupTrigger()) {
				_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
		 */
		public void mouseReleased(MouseEvent arg0) {
			if (arg0.isPopupTrigger()) {

				if (_output.getSelectedText() == null) {
					_copyMenu.setEnabled(false);
					_cutMenu.setEnabled(false);
				} else {
					_copyMenu.setEnabled(true);
					if (_output.getSelectionStart() < _maxCaretPosition)
						_cutMenu.setEnabled(false);
					else {
						_cutMenu.setEnabled(true);
					}
				}
				if (_output.getSelectionStart() < _maxCaretPosition)
					_pasteMenu.setEnabled(false);
				else
					_pasteMenu.setEnabled(true);

				_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}
	}
}
