package operations.configuration;

import java.util.ArrayList;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.undo.UndoableEdit;

import operations.factory.IOFactory;
import properties.PropertiesManager;
import es.text.TextFile;
import gui.MainWindow;

/**
 *
 */
public class DefaultConfiguration {

	/**
	 * 
	 */
	private boolean _explorer;
	/**
	 * 
	 */
	private boolean _shell;
	/**
	 * 
	 */
	private int _widthWindow;
	/**
	 * 
	 */
	private int _heightWindow;
	/**
	 * 
	 */
	private int _posX;
	/**
	 * 
	 */
	private int _posY;
	/**
	 * 
	 */
	private int _numEditor;
	/**
	 * 
	 */
	private ArrayList<String> _editors;
	/**
	 * 
	 */
	private IOFactory _ioFactory = IOFactory.getInstance();

	/**
	 * 
	 */
	public void load() {

		TextFile f = _ioFactory.buildFile();
		String text = "";
		try {
			text = f.load(PropertiesManager.getProperty("DefaultConfiguration"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		int cont = 0;
		int contf = 0;

		// Explorer
		contf = text.indexOf("\n", cont);
		String cond = text.substring(cont, contf);
		if (cond.equals("true"))
			_explorer = true;
		else
			_explorer = false;
		cont = contf + 1;

		// Shell
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		if (cond.equals("true"))
			_shell = true;
		else
			_shell = false;
		cont = contf + 1;

		// Width
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		_widthWindow = Integer.parseInt(cond);
		cont = contf + 1;
		// Height

		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		_heightWindow = Integer.parseInt(cond);
		cont = contf + 1;

		// Position x Window
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		_posX = Integer.parseInt(cond);
		cont = contf + 1;

		// Position y Window
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		_posY = Integer.parseInt(cond);
		cont = contf + 1;

		// Width1
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		Integer.parseInt(cond);
		cont = contf + 1;
		// Height1
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		cont = contf + 1;

		// Number of editors
		contf = text.indexOf("\n", cont);
		cond = text.substring(cont, contf);
		_numEditor = Integer.parseInt(cond);
		cont = contf + 1;
		// Editors
		_editors = new ArrayList<String>(_numEditor);
		for (int i = 0; i < _numEditor; i++) {
			contf = text.indexOf("\n", cont);
			cond = text.substring(cont, contf);
			cont = contf + 1;
			_editors.add(cond);
		}
	}

	/**
	 * 
	 */
	public void save() {
		
		MainWindow mainWindow = MainWindow.getInstance();
		String text = "";
		
		if (mainWindow.getMenu().getView().getShowBrowser().isSelected())
			_explorer = true;
		else
			_explorer = false;
		text = text + Boolean.toString(_explorer) + "\n";
		if (mainWindow.getMenu().getView().getShowShellWindow().isSelected())
			_shell = true;
		else
			_shell = false;
		text = text + Boolean.toString(_shell) + "\n";
		text = text + Integer.toString(mainWindow.getWidth()) + "\n";
		text = text + Integer.toString(mainWindow.getHeight()) + "\n";
		text = text + Integer.toString(mainWindow.getX()) + "\n";
		text = text + Integer.toString(mainWindow.getY()) + "\n";
		text = text + Integer.toString(mainWindow.getExplorer().getWidth())
				+ "\n"; // width1
		text = text + Integer.toString(mainWindow.getOutput().getHeight()) + "\n"; // height1
		
		TextFile f = _ioFactory.buildFile();
		text = text
				+ Integer.toString(mainWindow.getEditorBuilder().getNumEditors())
				+ "\n";
		for (int i = 0; i < mainWindow.getEditorBuilder().getNumEditors(); i++) {
			text = text + mainWindow.getEditorBuilder().getEditorAt(i).getPath()
					+ "\n";
		}
		f.save("./configuration/Defaultconf.acide", text);
	}

	/**
	 * 
	 */
	@SuppressWarnings("static-access")
	public void run() {
		
		MainWindow mainWindow = MainWindow.getInstance();
		if (!_explorer)
			mainWindow.getMenu().getView().getShowBrowser().doClick();

		if (!_shell)
			mainWindow.getMenu().getView().getShowShellWindow().doClick();

		mainWindow.setSize(_widthWindow, _heightWindow);
		mainWindow.setLocation(_posX, _posY);
		
		mainWindow.validate();
		mainWindow.repaint();
		mainWindow.setVisible(true);

		String prj = null;
		try {
			prj = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
				.getProjectConfiguration().getName() == null)) {
			mainWindow.getMenu().enableProjectMenu();
		}
		for (int j = 0; j < _numEditor; j++) {
			
			mainWindow.getMenu().enableFileMenu();
			mainWindow.getMenu().enableEditMenu();
			TextFile f = _ioFactory.buildFile();
			String text = null;
			text = f.load(_editors.get(j));
			String fich = null;
			String file = _editors.get(j);
			if (file != null) {
				int in = file.lastIndexOf("/");
				in++;
				fich = file.substring(in, file.length());
			}

			// Check the type
			int t = 0;

			mainWindow.getEditorBuilder().newTab(fich, file, text, true, t);

			// UNDO REDO
			// v.getnuevoMenu().habilita_salvarFich();

			int numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			DefaultStyledDocument doc = mainWindow.getEditorBuilder()
					.getSelectedEditor().getDoc();

			doc.addUndoableEditListener(new UndoableEditListener() {
				/*
				 * (non-Javadoc)
				 * @see javax.swing.event.UndoableEditListener#undoableEditHappened(javax.swing.event.UndoableEditEvent)
				 */
				public void undoableEditHappened(UndoableEditEvent evt) {
					MainWindow mainWindow = MainWindow.getInstance();
					UndoableEdit edit = evt.getEdit();
					if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
							.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
						mainWindow.getMenu().getEdit().getUndoManager().addEdit(evt.getEdit());
					}
				}
			});
			// Caret in the first position of the text.
			numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
					.setCaretPosition(0);
		}
	}

	/**
	 * 
	 * @return
	 */
	public int getHeightWindow() {
		return _heightWindow;
	}

	/**
	 * 
	 * @param heightWindow
	 */
	public void setHeightWindow(int heightWindow) {
		_heightWindow = heightWindow;
	}

	/**
	 * 
	 * @return
	 */
	public int getNumEditor() {
		return _numEditor;
	}

	/**
	 * 
	 * @param numEditor
	 */
	public void setNumEditor(int numEditor) {
		_numEditor = numEditor;
	}

	/**
	 * 
	 * @return
	 */
	public int getPosx() {
		return _posX;
	}

	/**
	 * 
	 * @param posx
	 */
	public void setPosx(int posx) {
		_posX = posx;
	}

	/**
	 * 
	 * @return
	 */
	public int getPosy() {
		return _posY;
	}

	/**
	 * 
	 * @param posy
	 */
	public void setPosy(int posy) {
		_posY = posy;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isShell() {
		return _shell;
	}

	/**
	 * 
	 * @param shell
	 */
	public void setShell(boolean shell) {
		_shell = shell;
	}

	/**
	 * 
	 * @return
	 */
	public int getWidthWindow() {
		return _widthWindow;
	}

	/**
	 * 
	 * @param widthWindow
	 */
	public void setWidthWindow(int widthWindow) {
		_widthWindow = widthWindow;
	}
}