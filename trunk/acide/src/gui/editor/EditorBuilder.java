package gui.editor;

import gui.MainWindow;
import gui.editor.EditorBuilder.TestPlaf.TestPlafLayout.CloseButton;

import javax.swing.*;
import javax.swing.plaf.basic.*;
import javax.swing.tree.TreePath;

import language.Language;
import operations.configuration.ExplorerFile;
import operations.factory.IOFactory;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.text.TextFile;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

/**
 * 
 */
public class EditorBuilder {

	/**
	 * 
	 */
	private static final String RESOURCE_PATH = "./resources/icons/editor/";
	/**
	 * 
	 */
	private DnDTabbedPane _tabbedPane;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private TestPlaf _testPlaf;
	/**
	 * 
	 */
	private static ArrayList<CloseButton> _closeButtons = new ArrayList<CloseButton>();

	/**
	 * Constructor of the class.
	 */
	public EditorBuilder() {

		ResourceBundle labels = Language.getInstance().getLabels();
		
		try {
			EditorBuilderMouseListener mouse = new EditorBuilderMouseListener();
			_tabbedPane = new DnDTabbedPane();
			_testPlaf = new TestPlaf();
			_tabbedPane.setUI(_testPlaf);
			_tabbedPane.addMouseListener(mouse);
		} catch (RuntimeException e) {
			_logger.info(labels.getString("s315"));
			e.printStackTrace();
		}
	}

	/**
	 * 
	 * @param name
	 * @param tooltip
	 */
	public void newEditor(String name, String tooltip, int type) {
		
		Editor e = new Editor();

		switch (type) {
		case 0: {
			_tabbedPane.addTab(name, null, e, tooltip);
			e.setIcon(null);
			break;
		}
		case 1: {
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH + "main.PNG"), e,
					tooltip);
			e.setIcon(new ImageIcon(RESOURCE_PATH + "main.PNG"));
			break;
		}
		case 2: {
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH + "compilable.PNG"),
					e, tooltip);
			new ImageIcon(RESOURCE_PATH + "compilable.PNG");
			break;
		}
		}
		_tabbedPane.revalidate();
		_tabbedPane.repaint();
	}

	/**
	 * 
	 * @return
	 */
	public JTabbedPane getTabbedPane() {
		return _tabbedPane;
	}

	/**
	 * 
	 * @param pos
	 * @return
	 */
	public Editor getEditorAt(int pos) {
		if ((pos < _tabbedPane.getComponentCount()) && (pos >= 0)) {
			return (Editor) _tabbedPane.getComponentAt(pos);
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @return
	 */
	public int getNumEditors() {
		return _tabbedPane.getTabCount();
	}

	/**
	 * 
	 * @param name
	 * @param textType
	 * @param text
	 * @param editable
	 */
	public void newTab(String name, String textType, String text,
			boolean editable, int type) {

		boolean found = false;
		int pos = 0;
		for (int i = 0; i < getNumEditors(); i++) {
			if (getEditorAt(i).getPath() == textType) {
				found = true;
				pos = i;
			}
		}
		
		if (!found) {
			File file = new File(textType);
			int index = name.lastIndexOf("\\");
			String subName = name.substring(index + 1);
			newEditor(subName, textType, type);
			int n = getNumEditors() - 1;
			getEditorAt(n).loadText(text);
			getEditorAt(n).setEditable(editable);
			getTabbedPane().setSelectedIndex(n);
			getEditorAt(n).setPath(textType);
			getEditorAt(n).setLastChange(file.lastModified());
			getEditorAt(n).setLastSize(file.length());
		} else {
			setSelectedEditorAt(pos);
		}
	}

	/**
	 * 
	 * @param n
	 */
	public void setSelectedEditorAt(int n) {
		getTabbedPane().setSelectedIndex(n);
	}

	/**
	 * 
	 * @return
	 */
	public int getSelectedEditorIndex() {
		return getTabbedPane().getSelectedIndex();
	}

	/**
	 * 
	 * @return
	 */
	public Editor getSelectedEditor() {
		return getEditorAt(getTabbedPane().getSelectedIndex());
	}

	/**
	 * 
	 * @return
	 */
	public Editor getMainEditor() {

		for (int i = 0; i < getNumEditors(); i++) {

			if (getEditorAt(i).isMainFile())
				return getEditorAt(i);
		}

		return null;
	}

	/**
	 * Handle the opened Editors.
	 */
	static class TestPlaf extends BasicTabbedPaneUI{

		/**
		 * 
		 */
		TestPlafLayout _testPlafLayout;

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#createLayoutManager()
		 */
		protected LayoutManager createLayoutManager(){

			_testPlafLayout = new TestPlafLayout();
			return _testPlafLayout;
		}

		/**
		 * 
		 * @param pos
		 * @return
		 */
		public CloseButton getCloseButtoni(int pos) {
			return _testPlafLayout.getCloseButtonAt(pos);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#getTabInsets(int, int)
		 */
		protected Insets getTabInsets(int tabPlacement, int tabIndex){

			Insets defaultInsets = (Insets) super.getTabInsets(tabPlacement,
					tabIndex).clone();

			defaultInsets.right += 40;

			defaultInsets.top += 4;

			defaultInsets.bottom += 4;

			return defaultInsets;
		}

		/**
		 * Handle the layout of the tab.
		 */
		class TestPlafLayout extends TabbedPaneLayout{

			/**
			 * 
			 * @param pos
			 * @return
			 */
			public CloseButton getCloseButtonAt(int pos) {
				return (CloseButton) _closeButtons.get(pos);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.LayoutManager#layoutContainer(java.awt.Container)
			 */
			public void layoutContainer(Container parent){

				super.layoutContainer(parent);

				while (tabPane.getTabCount() > _closeButtons.size())
					_closeButtons.add(new CloseButton(_closeButtons.size()));

				Rectangle rect = new Rectangle();
				int i;

				for (i = 0; i < tabPane.getTabCount(); i++){

					rect = getTabBounds(i, rect);
					JButton closeButton = (JButton) _closeButtons.get(i);
					closeButton.setLocation(rect.x + rect.width - 20,
							rect.y + 5);
					closeButton.setSize(15, 15);
					tabPane.add(closeButton);
				}

				for (; i < _closeButtons.size(); i++){
					tabPane.remove((JButton) _closeButtons.get(i));
					_closeButtons.remove(i);
				}
			}

			/**
			 * Implements used UIResource when the close button is added to the
			 * TabbedPane.
			 */
			class CloseButton extends JButton implements
					javax.swing.plaf.UIResource{

				/**
				 * serialVersionUID.
				 */
				private static final long serialVersionUID = 1L;

				/**
				 * 
				 * @param index
				 */
				public CloseButton(int index) {

					super(new CloseButtonAction(index));
					setForeground(new Color(0, 200, 0));
					setFont(new Font("Arial", Font.BOLD, 12));
					ResourceBundle labels = Language.getInstance().getLabels();
					setToolTipText(labels.getString("s316"));

					setMargin(new Insets(0, 0, 0, 0));
				}

				/**
				 * 
				 */
				public void redButton() {
					setForeground(new Color(255, 0, 0));
				}

				/**
				 * 
				 */
				public void greenButton() {
					setForeground(new Color(0, 200, 0));
				}

				/**
				 * 
				 * @return
				 */
				public boolean isRedButton() {
					if (getForeground().equals(new Color(255, 0, 0)))
						return true;
					return false;
				}
			}

			/**
			 * 
			 */
			class CloseButtonAction extends AbstractAction{

				/**
				 * serialVersionUID
				 */
				private static final long serialVersionUID = 1L;
				/**
				 * 
				 */
				private int _index;

				/**
				 * Constructor of the class.
				 * 
				 * @param index
				 */
				public CloseButtonAction(int index){

					super("x");
					_index = index;
				}

				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent e){
					
					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception ee) {
						ee.printStackTrace();
					}
					final ResourceBundle labels = language.getLabels();
					
					if (mainWindow.getEditorBuilder().isRedButton(_index) == true) {
						int chosenOption = JOptionPane.showConfirmDialog(null,
								labels.getString("s643"));

						if (chosenOption == JOptionPane.OK_OPTION) {

							if (mainWindow.getEditorBuilder().getEditorAt(_index)
									.getPath().equals(labels.getString("s79")) == true) {

								IOFactory ioFactory = IOFactory.getInstance();
								TextFile textFile = ioFactory.buildFile();
								String text = " ";
								text = textFile.write();
								if (text.equals(" ")) {
									// logger.info(labels.getString("s92"));
								} else {
									
									boolean result = textFile.save(
											text,
											mainWindow.getEditorBuilder()
													.getEditorAt(_index)
													.getText());
									if (result) {
										// logger.info(labels.getString("s93")+archivo+labels.getString("s94"));
										MainWindow.getInstance()
												.getEditorBuilder()
												.greenButton(_index);
										mainWindow.getEditorBuilder().getEditorAt(_index)
												.setPath(text);
										mainWindow.getEditorBuilder().getEditorAt(_index)
												.setToolTipText(text);
										int in = text.lastIndexOf("\\");
										in++;
										String file = text.substring(in,
												text.length());
										mainWindow.getEditorBuilder().getEditorAt(_index)
												.setName(file);
										mainWindow.getStatusBar().setMessage("");
									} else {
										// logger.info(labels.getString("s95") +
										// text);
									}
								}

							} else {
								IOFactory fact = IOFactory.getInstance();
								TextFile textFile = fact.buildFile();
								// string text = " ";
								if (mainWindow.getEditorBuilder().getEditorAt(_index)
										.getPath()
										.equals(labels.getString("s79")) == false) {
									boolean result = textFile.save(
											mainWindow.getEditorBuilder()
													.getEditorAt(_index)
													.getPath(), mainWindow
													.getEditorBuilder()
													.getEditorAt(_index)
													.getText());
									if (result) {
										// logger.info(labels.getString("s93") +
										// text+ labels.getString("s94"));
										MainWindow.getInstance()
												.getEditorBuilder()
												.greenButton(_index);
									} else {
										// logger.info(labels.getString("s95") +
										// text);
									}
								} else {
									String text = " ";
									if (mainWindow.getEditorBuilder().getNumEditors() == 0) {
										// logger.info(labels.getString("s89"));
									} else {
										text = textFile.write();
										if (text.equals(" ")) {
											// logger.info(labels.getString("s92"));
										} else {
											boolean result = textFile.save(text,
													mainWindow.getEditorBuilder()
															.getEditorAt(_index)
															.getText());
											if (result) {
												// logger.info(labels.getString("s93")
												// + archivo+
												// labels.getString("s94"));
												MainWindow.getInstance()
														.getEditorBuilder()
														.greenButton(_index);
												int in = text
														.lastIndexOf("\\");
												in++;
												String file = text
														.substring(in, text
																.length());
												mainWindow.getEditorBuilder()
														.getPane()
														.setTitleAt(_index, file);
												mainWindow.getEditorBuilder()
														.getEditorAt(_index)
														.setPath(text);
												mainWindow.getEditorBuilder()
														.getPane()
														.setToolTipText(text);
												mainWindow.getStatusBar().setMessage("");

											} else {
												// logger.info(labels.getString("s95")
												// + archivo);
											}
										}
									}
								}

							}

							for (int i2 = 0; i2 < mainWindow.getProjectConfiguration()
									.getFileListSize(); i2++) {
								if (mainWindow.getProjectConfiguration()
										.getFile(i2)
										.getPath()
										.equals(mainWindow.getEditorBuilder()
												.getEditorAt(_index).getPath())) {
									mainWindow.getProjectConfiguration().getFile(i2)
											.setOpened(false);
								}
							}
							String prj = null;
							try {
								prj = PropertiesManager
										.getProperty("defaultAcideProject");
							} catch (Exception e1) {
								e1.printStackTrace();
							}
							if (!(prj
									.equals("./configuration/default.acidePrj") && mainWindow
									.getProjectConfiguration().getName()
									.equals(""))) {
								mainWindow.getProjectConfiguration().setModified(true);
							}
							mainWindow.getEditorBuilder().getPane().remove(_index);

						} else if (chosenOption == JOptionPane.NO_OPTION) {
							mainWindow.getEditorBuilder().getPane().remove(_index);
							mainWindow.getStatusBar().setMessage("");
						}
					} else {

						for (int i2 = 0; i2 < mainWindow.getProjectConfiguration()
								.getFileListSize(); i2++) {
							if (mainWindow.getProjectConfiguration()
									.getFile(i2)
									.getPath()
									.equals(mainWindow.getEditorBuilder()
											.getEditorAt(_index).getPath())) {
								mainWindow.getProjectConfiguration().getFile(i2)
										.setOpened(false);
							}
						}
						String prj = null;
						try {
							prj = PropertiesManager
									.getProperty("defaultAcideProject");
						} catch (Exception e1) {
							e1.printStackTrace();
						}
						if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
								.getProjectConfiguration().getName()
								.equals(""))) {
							mainWindow.getProjectConfiguration().setModified(true);
						}
						mainWindow.getEditorBuilder().getPane().remove(_index);
						mainWindow.getStatusBar().setMessage("");
					}
					if (mainWindow.getEditorBuilder().getPane().getTabCount() == 0) {
						mainWindow.getMenu().disableFileMenu();
						mainWindow.getMenu().disableEditMenu();
					}
					
					for (int pos = _index; pos < _closeButtons.size() - 1; pos++) {
						CloseButton c1 = (CloseButton) _closeButtons.get(pos);
						CloseButton c2 = (CloseButton) _closeButtons
								.get(pos + 1);
						c1.setForeground(c2.getForeground());
						_closeButtons.set(pos, c1);
					}
				}
			}

			/**
			 * 
			 * @return
			 */
			public ArrayList<CloseButton> getCloseButtons() {
				return _closeButtons;
			}
		}
	}

	/**
	 * 
	 */
	class EditorBuilderMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			
			if (_tabbedPane.getComponentCount() != 0) {
				
				String s = getSelectedEditor().getPath();
				
				if (s != null) {
					
					File f = new File(s);
					
					if ((f.lastModified() != getSelectedEditor()
							.getLastChange())
							|| (f.length() != getSelectedEditor()
									.getLastSize())) {
						Language language = Language.getInstance();

						try {
							language.getLanguage(Integer.parseInt(PropertiesManager
									.getProperty("language")));
						} catch (Exception e) {
							e.printStackTrace();
						}
						ResourceBundle labels = language.getLabels();
						
						int choosenOption = JOptionPane.showConfirmDialog(null,
								labels.getString("s65"));
						if (choosenOption == 0) {
							TextFile newTextFile = new TextFile();
							getSelectedEditor().loadText(newTextFile.load(s));
							getSelectedEditor().setLastChange(
									f.lastModified());
							getSelectedEditor().setLastSize(f.length());
						} else {
							getSelectedEditor().setLastChange(
									f.lastModified());
							getSelectedEditor().setLastSize(f.length());
						}
					}
				}
			}

			// Updates the Status Bar
			if (getSelectedEditor() != null){
				
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getStatusBar().setMessage(
						mainWindow.getEditorBuilder().getSelectedEditor().getPath());
				
				for (int i = 0; i < mainWindow.getProjectConfiguration().getNumFilesFromList(); i++) {
					if (mainWindow.getProjectConfiguration()
							.getFile(i)
							.getPath()
							.equals(mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath()))
						if (mainWindow.getProjectConfiguration().getFile(i).isSetFile())
							if (mainWindow.getProjectConfiguration().getFile(i)
									.isMainFile())
								mainWindow.getStatusBar().setMessage(
										mainWindow.getEditorBuilder()
												.getSelectedEditor().getPath()
												+ " <MAIN>");
							else
								mainWindow.getStatusBar().setMessage(
										mainWindow.getEditorBuilder()
												.getSelectedEditor().getPath()
												+ " <COMPILABLE>");
						else
							mainWindow.getStatusBar().setMessage(
									mainWindow.getEditorBuilder().getSelectedEditor()
											.getPath());
				}

				String project = null;
				try {
					project = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}

				if ((project.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {

					// status
					if (mainWindow.getEditorBuilder().getSelectedEditor()
							.isCompilerFile())
						if (mainWindow.getEditorBuilder().getSelectedEditor()
								.isMainFile())
							mainWindow.getStatusBar().setMessage(
									mainWindow.getEditorBuilder().getSelectedEditor()
											.getPath()
											+ " <MAIN>");
						else
							mainWindow.getStatusBar().setMessage(
									mainWindow.getEditorBuilder().getSelectedEditor()
											.getPath()
											+ " <COMPILABLE>");
					else
						mainWindow.getStatusBar().setMessage(
								mainWindow.getEditorBuilder().getSelectedEditor()
										.getPath());
				}

			}

			// Put the selected file in the editor if there is not open project
			MainWindow mainWindow = MainWindow.getInstance();
			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			if (!(project.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {

				if (getSelectedEditor() != null) {
					ExplorerFile f = new ExplorerFile();
					int y = -1;
					for (int j = 0; j < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); j++) {

						if (mainWindow.getProjectConfiguration()
								.getFile(j)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath())) {
							f = mainWindow.getProjectConfiguration().getFile(j);
							for (int m = 0; m < mainWindow.getProjectConfiguration()
									.getNumFilesFromList() + 1; m++) {
								if (mainWindow.getExplorer().getTree().getPathForRow(m)
										.getLastPathComponent().toString()
										.equals(f.getLastPathComponent())) {

									y = m;
								}
							}
						}
					}

					TreePath currentSelection = mainWindow.getExplorer().getTree()
							.getPathForRow(y);
					mainWindow.getExplorer().getTree()
							.setSelectionPath(currentSelection);
				}
			}

			if (mainWindow.getEditorBuilder().getNumEditors() > 0) {
				int index = MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditorIndex();
				ImageIcon ic = (ImageIcon) _tabbedPane.getIconAt(index);
				MainWindow.getInstance().getEditorBuilder().getPane()
						.setIcon(ic);
			}
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			
			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0) {
				
				int index = MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditorIndex();
				ImageIcon ic = (ImageIcon) _tabbedPane.getIconAt(index);
				MainWindow.getInstance().getEditorBuilder().getPane()
						.setIcon(ic);
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public DnDTabbedPane getPane() {
		return _tabbedPane;
	}

	/**
	 * 
	 * @return
	 */
	public TestPlaf getTestPlaf() {
		return _testPlaf;
	}

	/**
	 * 
	 */
	public void greenButton() {
		MainWindow
				.getInstance()
				.getEditorBuilder()
				.getTestPlaf()
				.getCloseButtoni(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditorIndex()).greenButton();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isRedButton() {
		return MainWindow
				.getInstance()
				.getEditorBuilder()
				.getTestPlaf()
				.getCloseButtoni(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditorIndex()).isRedButton();
	}

	/**
	 * 
	 * @param i
	 */
	public void greenButton(int i) {
		MainWindow.getInstance().getEditorBuilder().getTestPlaf().getCloseButtoni(i)
				.greenButton();
	}

	/**
	 * 
	 * @param i
	 * @return
	 */
	public boolean isRedButton(int i) {
		return MainWindow.getInstance().getEditorBuilder().getTestPlaf()
				.getCloseButtoni(i).isRedButton();
	}
}
