package operations.configuration;

import es.configuration.programmingLanguage.ProgrammingLanguage;
import es.text.TextFile;
import gui.MainWindow;

import java.awt.Cursor;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.undo.UndoableEdit;

import language.Language;

import operations.factory.IOFactory;
import properties.PropertiesManager;

/**
 * 
 */
public class LoadDefaultProject {

	/**
	 * 
	 * @param list
	 * @param f
	 * @return
	 */
	private DefaultMutableTreeNode searchListDir(ArrayList<DefaultMutableTreeNode> list, String f) {
		int i = 0;
		boolean found = false;
		while (i < list.size() && !found) {
			DefaultMutableTreeNode temp = list.get(i);
			ExplorerFile file = (ExplorerFile) temp.getUserObject();
			if (file.getName().equals(f)) {
				found = true;
				return (DefaultMutableTreeNode) list.get(i);
			} else
				i++;
		}
		return null;
	}

	/**
	 * 
	 */
	public String preloadDefault() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();

		MainWindow mainWindow = MainWindow.getInstance(); 
		mainWindow.getProjectConfiguration().removeFiles();
		TextFile f = new TextFile();
		String text = null;
		String path = null;

		try {
			path = PropertiesManager.getProperty("defaultAcideProject");
			text = f.load(path);

			if (text == null) {
				text = f.load("./configuration/default.acidePrj");
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/default.acidePrj");
				JOptionPane.showMessageDialog(null, labels.getString("s960")
						+ path + labels.getString("s959"));
			}
		} catch (Exception e) {
			e.printStackTrace();
			text = f.load("./configuration/default.acidePrj");
			JOptionPane.showMessageDialog(null, labels.getString("s960") + path
					+ labels.getString("s959"));
		}
		return text;
	}

	/**
	 * 
	 * @param path
	 */
	@SuppressWarnings({ "static-access", "rawtypes", "unchecked" })
	public void loadDefault(String path) {

		// Wait cursor
		Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		MainWindow mainWindow = MainWindow.getInstance(); 
		mainWindow.setCursor(cursor);

		// Load project
		mainWindow.getProjectConfiguration().load(path);

		// last configuration
		if (!mainWindow.getProjectConfiguration().isExplorer())
			mainWindow.getMenu().getView().getShowBrowser().doClick();
		if (!mainWindow.getProjectConfiguration().isShell())
			mainWindow.getMenu().getView().getShowShellWindow().doClick();
		mainWindow.setSize(mainWindow.getProjectConfiguration().getWidthWindow(), mainWindow.getProjectConfiguration()
				.getHeightWindow());
		mainWindow.setLocation(mainWindow.getProjectConfiguration().getPosX(), mainWindow.getProjectConfiguration().getPosY());
		mainWindow.getSplitPaneVertical().setDividerLocation(mainWindow.getProjectConfiguration().getWidth1());
		mainWindow.getSplitPaneHorizontal().setDividerLocation(mainWindow.getProjectConfiguration().getHeight1());

		mainWindow.validate();
		mainWindow.repaint();
		mainWindow.setVisible(true);

		String project = null;
		try {
			project = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		boolean def = true;
		
		if (!(project.equals("./configuration/default.acidePrj") && mainWindow
				.getProjectConfiguration().getName().equals(""))) {
			mainWindow.getMenu().enableProjectMenu();
			def = false;
		}

		// open files
		for (int j = 0; j < mainWindow.getProjectConfiguration().getNumFilesFromList(); j++) {

			if (!mainWindow.getProjectConfiguration().getFile(j).isDirectory()) {

				IOFactory fact = IOFactory.getInstance();
				TextFile ff = fact.buildFile();
				String text = null;
				text = ff.load(mainWindow.getProjectConfiguration().getFile(j).getPath());
				String fich = null;
				String file = mainWindow.getProjectConfiguration().getFile(j).getPath();
				if (file != null) {
					int in = file.lastIndexOf("/");
					in++;
					fich = file.substring(in, file.length());
				}
				if (def || mainWindow.getProjectConfiguration().getFile(j).isOpened()) {

					mainWindow.getMenu().enableFileMenu();
					mainWindow.getMenu().enableEditMenu();

					// status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getProjectConfiguration().getFile(j).getPath());
					// Check the type
					int t = 0;
					if (mainWindow.getProjectConfiguration().getFile(j).isSetFile()) {
						t = 2;
						// status
						mainWindow.getStatusBar().setMessage(
								mainWindow.getProjectConfiguration().getFile(j).getPath()
										+ " <COMPILABLE>");
					}
					if (mainWindow.getProjectConfiguration().getFile(j).isMainFile()) {
						t = 1;
						// status
						mainWindow.getStatusBar().setMessage(
								mainWindow.getProjectConfiguration().getFile(j).getPath()
										+ " <MAIN>");
					}

					mainWindow.getEditorBuilder().newTab(fich, file, text, true, t);

					// Marked for empty project
					for (int i = 0; i < mainWindow.getEditorBuilder().getNumEditors(); i++) {
						if (mainWindow.getEditorBuilder().getEditorAt(i).getPath()
								.equals(mainWindow.getProjectConfiguration().getFile(j).getPath())) {
							if (mainWindow.getProjectConfiguration().getFile(j).isSetFile())
								mainWindow.getEditorBuilder().getEditorAt(i)
										.setCompilerFile(true);
							if (mainWindow.getProjectConfiguration().getFile(j).isMainFile())
								mainWindow.getEditorBuilder().getEditorAt(i)
										.setMainFile(true);
							;
						}
					}

					// UNDO REDO
					int numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
					DefaultStyledDocument doc = mainWindow.getEditorBuilder()
							.getSelectedEditor().getDoc();

					doc.addUndoableEditListener(new UndoableEditListener() {
						public void undoableEditHappened(UndoableEditEvent evt) {
							MainWindow mainWindow = MainWindow.getInstance();
							UndoableEdit edit = evt.getEdit();
							if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
									.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
								mainWindow.getMenu().getEdit().getUndoManager().addEdit(evt.getEdit());
							}
						}
					});
					
					// Caret in the first position of the editor
					numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
					mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
							.setCaretPosition(0);
				}
			}
		}

		// Load lexical
		PropertiesManager.setProperty("languagePath", mainWindow.getProjectConfiguration()
				.getLanguageConfiguration());

		ProgrammingLanguage programmingLanguage = ProgrammingLanguage.getInstance();
		try {
			programmingLanguage.load(PropertiesManager.getProperty("languagePath"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Load language
		String configurationLanguage = mainWindow.getProjectConfiguration().getLanguage();
		Language language = Language.getInstance();
		
		if (configurationLanguage.equals("0")) {
			mainWindow.getMenu().getConfiguration().getLanguage().getSpanish().doClick();
		} else {
			mainWindow.getMenu().getConfiguration().getLanguage().getEnglish().doClick();
		}
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (NumberFormatException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

		ResourceBundle labels = language.getLabels();
		mainWindow.getStatusBar().setMessagelexical(
				labels.getString("s449") + " " + programmingLanguage.getName());

		// Load Menu config

		/*
		 * boolean[] valores = null; String menuFile = null; try { menuFile =
		 * almacenPropiedades.getPropiedad("currentMenuConfiguration"); valores =
		 * MenuConfig.cargarMenuCfgFich(menuFile); } catch (Exception e3) {
		 * menuFile = "./configuration/menu/defaultAllOn.menuCfg"; try {
		 * valores = MenuConfig.cargarMenuCfgFich(menuFile); } catch (Exception
		 * e) { e.printStackTrace(); }
		 * almacenPropiedades.setPropiedad("currentMenuConfiguration",menuFile);
		 * almacenPropiedades.setPropiedad("previousMenuCfg",menuFile); }
		 * //valores =
		 * MenuConfig.cargarMenuCfgFich(almacenPropiedades.getPropiedad
		 * ("currentMenuConfiguration")); MenuConfig.setAll(valores);
		 * v.getnuevoMenu().setMenuConfig(); v.validate(); v.repaint();
		 * v.getnuevoMenu().getSaveMenu().setEnabled(true);
		 * MenuGUI.setChangesSaved(true);
		 */

		// Load TB config

		/*
		 * try {
		 * ListaiconsEditables.cargaLista(almacenPropiedades.getPropiedad(
		 * "currentToolBarConfiguration"));
		 * ListaiconsEditables.cargaListaAux(almacenPropiedades
		 * .getPropiedad("currentToolBarConfiguration")); } catch (Exception e4) { try {
		 * ListaiconsEditables
		 * .cargaLista("./configuration/toolbar/default.BHcfg"); } catch
		 * (Exception e) { e.printStackTrace(); } try {
		 * ListaiconsEditables.cargaListaAux
		 * ("./configuration/toolbar/default.BHcfg"); } catch (Exception e) {
		 * e.printStackTrace(); }
		 * almacenPropiedades.setPropiedad("currentToolBarConfiguration",
		 * "./configuration/toolbar/default.BHcfg");
		 * almacenPropiedades.setPropiedad
		 * ("previousToolBarConfiguration","./configuration/toolbar/default.BHcfg"); }
		 * icons.generaToolBarFija(); icons.generaToolBarEditable();
		 * v.validate(); v.repaint();
		 * v.getnuevoMenu().getSaveTB().setEnabled(true);
		 * EdicioniconsGUI.setChangesSaved(true);
		 */

		// Load shell
		PropertiesManager.setProperty("execPath", mainWindow.getProjectConfiguration()
				.getShellDir());
		PropertiesManager.setProperty("exec", mainWindow.getProjectConfiguration()
				.getShellPath());
		// almacenPropiedades.setPropiedad("echoCommand",
		// v.getProyecto().getEchoCommand());
		// almacenPropiedades.setPropiedad("exitCommand",
		// v.getProyecto().getExitCommand());
		mainWindow.getOutput().resetOutput();

		try {
			if (PropertiesManager.getProperty("defaultAcideProject").equals(
					"./configuration/default.acidePrj")) {
				mainWindow.setTitle(mainWindow.getTitle() + " - <empty>");
			} else {
				String file = mainWindow.getProjectConfiguration().getName();
				ExplorerFile fi = new ExplorerFile();
				fi.setPath(file);
				fi.setName(file);
				fi.setParent(null);
				fi.setDirectory(true);
				mainWindow.getExplorer().getAddFile().setEnabled(true);
				mainWindow.getExplorer().getRemoveFile().setEnabled(true);
				mainWindow.setTitle(labels.getString("s425") + " - "
						+ mainWindow.getProjectConfiguration().getName());
				mainWindow.getExplorer().getRoot().removeAllChildren();
				DefaultMutableTreeNode d = new DefaultMutableTreeNode(fi);
				mainWindow.getExplorer().getRoot().add(d);
				ArrayList listdir = new ArrayList();

				for (int i = 0; i < Integer.parseInt(mainWindow.getProjectConfiguration()
						.getNumFiles()); i++) {
					DefaultMutableTreeNode h = new DefaultMutableTreeNode(mainWindow
							.getProjectConfiguration().getFile(i));
					if (mainWindow.getProjectConfiguration().getFile(i).isDirectory()) {
						h.setAllowsChildren(true);
						listdir.add(h);
					} else
						h.setAllowsChildren(false);

					if (mainWindow.getProjectConfiguration().getFile(i).getParent()
							.equals(mainWindow.getProjectConfiguration().getName())) {
						d.add(h);
					} else {
						DefaultMutableTreeNode fh = searchListDir(listdir, mainWindow
								.getProjectConfiguration().getFile(i).getParent());
						fh.add(h);
					}
				}
				mainWindow.getExplorer().getTreeModel().reload();
				mainWindow.getExplorer().expandTree();
				mainWindow.getExplorer().setEnabledAddFile();
				mainWindow.getExplorer().setEnabledSaveProj();
				if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0)
					mainWindow.getExplorer().setEnabledRemoveFile();
				else
					mainWindow.getExplorer().getRemoveFile().setEnabled(false);

				mainWindow.getProjectConfiguration().setFirstSave(true);
				mainWindow.getProjectConfiguration().setPath(
						PropertiesManager.getProperty("defaultAcideProject"));
			}
		} catch (NumberFormatException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Standard cursor
		cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		mainWindow.setCursor(cursor);
	}
}
