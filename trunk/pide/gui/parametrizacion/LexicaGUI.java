package gui.parametrizacion;
import gui.Ventana;
import idioma.Idioma;
import java.awt.BorderLayout;
import java.awt.Checkbox;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ResourceBundle;
import javax.swing.AbstractButton;
import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import operaciones.genericas.Windows;
import operaciones.lexicas.Comments;
import operaciones.lexicas.DividerList;
import operaciones.lexicas.ListaTiposToken;
import operaciones.lexicas.TipoToken;
import operaciones.log.Log;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;
import es.configuracion.lenguajeprog.Lenguaje;
import es.configuracion.lexica.MyTableModel;
import es.configuracion.lexica.TableSorter;

/**
 * Clase que implementa la ventana que incluye los campos necesarios para
 * realizar la parametrizacion lexica de un lenguaje.
 */
public class LexicaGUI extends JFrame {

	private static final long serialVersionUID = 1L;

	private ResourceBundle labels = Idioma.getInstance().getLabels();
	private JFrame frame;
	private JTable listaPalTabla;
	private JTable listaDelimiterTabla;
	private String[] columnas = { labels.getString("s374"), labels.getString("s443") , labels.getString("s375") };
	private String[] columnas2 = { labels.getString("s440")};
	private JPanel panelTokens;
	private JPanel panelDividers;
	private JPanel panelComment;
	private TableSorter sorter;
	private TableSorter sorter2;
	private JTextField previewTextField;
	private JTextField previewCommentTextField;
	private JTextField palabraField;
	private Checkbox caseSensitiveCheckBox;
	private JComboBox colorCombo;
	private JFrame frameColores;
	private JComboBox tipoFuenteCombo;
	private JComboBox colorComboComment;
	private JButton modify;
	private JButton quitarBoton;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	
	public LexicaGUI() {
		logger.info(labels.getString("s376"));
		
		final String tempPath = Lenguaje.getInstance().guardarTemp(Lenguaje.getInstance().getNombre(),false);
		
		frame = new JFrame();
		frame.addWindowListener(new java.awt.event.WindowAdapter(){
	           public void windowClosing(WindowEvent e){
	        	   try {
					Lenguaje.getInstance().cargar(almacenPropiedades.getPropiedad("pathLenguaje"));
				} catch (Exception e1) {
					e1.printStackTrace();
				}
	           }
	        }
	        );
		frame.setLayout(new GridBagLayout());
		panelTokens = new JPanel();
		panelTokens.setBorder(BorderFactory.createTitledBorder(labels.getString("s428")));
		panelTokens.setLayout(new GridBagLayout());
		panelDividers = new JPanel();
		panelDividers.setBorder(BorderFactory.createTitledBorder(labels.getString("s429")));
		panelDividers.setLayout(new GridBagLayout());
		panelComment = new JPanel();
		panelComment.setBorder(BorderFactory.createTitledBorder(labels.getString("s430")));
		panelComment.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		frame.setTitle(labels.getString("s377") + " - " + Lenguaje.getInstance().getNombre());
		// Creacion de todas las componentes de la ventana
		colorCombo = new JComboBox();
		colorCombo.addItem("");
		colorCombo.addItem(labels.getString("s378"));
		colorCombo.addItem(labels.getString("s379"));
		colorCombo.addItem(labels.getString("s380"));
		colorCombo.addItem(labels.getString("s381"));
		colorCombo.addItem(labels.getString("s382"));
		colorCombo.addItem(labels.getString("s383"));
		colorCombo.setEnabled(true);
		colorCombo.setToolTipText(labels.getString("s384"));
		
		colorComboComment = new JComboBox();
		colorComboComment.addItem("");
		colorComboComment.addItem(labels.getString("s378"));
		colorComboComment.addItem(labels.getString("s379"));
		colorComboComment.addItem(labels.getString("s380"));
		colorComboComment.addItem(labels.getString("s381"));
		colorComboComment.addItem(labels.getString("s382"));
		colorComboComment.addItem(labels.getString("s383"));
		colorComboComment.setEnabled(true);
		colorComboComment.setToolTipText(labels.getString("s384"));
		
		JButton añadir = new JButton(labels.getString("s385"));
		añadir.setVerticalTextPosition(AbstractButton.CENTER);
		añadir.setHorizontalTextPosition(AbstractButton.LEADING);
		añadir.setMnemonic(KeyEvent.VK_A);
		añadir.setToolTipText(labels.getString("s386"));
		
		modify = new JButton(labels.getString("s436"));
		modify.setVerticalTextPosition(AbstractButton.CENTER);
		modify.setHorizontalTextPosition(AbstractButton.LEADING);
		modify.setMnemonic(KeyEvent.VK_M);
		modify.setToolTipText(labels.getString("s437"));
		//modify.setEnabled(false);
		
		JButton setDelimiters = new JButton(labels.getString("s438"));
		setDelimiters.setVerticalTextPosition(AbstractButton.CENTER);
		setDelimiters.setHorizontalTextPosition(AbstractButton.LEADING);
		setDelimiters.setMnemonic(KeyEvent.VK_S);
		setDelimiters.setToolTipText(labels.getString("s439"));
		
		
		quitarBoton = new JButton(labels.getString("s387"));
		quitarBoton.setVerticalTextPosition(AbstractButton.CENTER);
		quitarBoton.setHorizontalTextPosition(AbstractButton.LEADING);
		quitarBoton.setMnemonic(KeyEvent.VK_Q);
		quitarBoton.setToolTipText(labels.getString("s388"));
		//quitarBoton.setEnabled(false);
		
		JButton addDivider = new JButton(labels.getString("s385"));
		addDivider.setVerticalTextPosition(AbstractButton.CENTER);
		addDivider.setHorizontalTextPosition(AbstractButton.LEADING);
		//añadir.setMnemonic(KeyEvent.VK_A);
		//addDivider.setToolTipText(labels.getString("s386"));
		JButton deleteDivider = new JButton(labels.getString("s387"));
		deleteDivider.setVerticalTextPosition(AbstractButton.CENTER);
		deleteDivider.setHorizontalTextPosition(AbstractButton.LEADING);
		//quitarBoton.setMnemonic(KeyEvent.VK_Q);
		//deleteDivider.setToolTipText(labels.getString("s388"));
		final JLabel caseSensitiveLabel = new JLabel(labels.getString("s431"),JLabel.CENTER);
		caseSensitiveCheckBox = new Checkbox();
		final JLabel palabraLabel = new JLabel(labels.getString("s389"),JLabel.CENTER);
		palabraField = new JTextField();
		palabraField.setToolTipText(labels.getString("s390"));
		JLabel colorLabel = new JLabel(labels.getString("s391"), JLabel.CENTER);
		JLabel colorLabelComment = new JLabel(labels.getString("s391"), JLabel.CENTER);
		JLabel previewLabel = new JLabel(labels.getString("s392"), JLabel.CENTER);
		JLabel previewCommentLabel = new JLabel(labels.getString("s392"), JLabel.CENTER);
		previewTextField = new JTextField();
		previewTextField.setToolTipText(labels.getString("s393"));
		previewTextField.setEditable(false);
		Font f = palabraLabel.getFont();
		previewTextField.setHorizontalAlignment(JTextField.CENTER);
		previewTextField.setFont(new Font(f.getFontName(), Font.PLAIN, f.getSize()));
		previewTextField.setForeground(Color.BLACK);
		previewTextField.setText(labels.getString("s394"));
		
		previewCommentTextField = new JTextField();
		previewCommentTextField.setToolTipText(labels.getString("s393"));
		previewCommentTextField.setEditable(false);
		previewCommentTextField.setHorizontalAlignment(JTextField.CENTER);
		previewCommentTextField.setFont(new Font(f.getFontName(), Font.PLAIN, f.getSize()));
		previewCommentTextField.setForeground(Color.BLACK);
		previewCommentTextField.setText(labels.getString("s394"));
		
		JLabel tipoFuenteLabel = new JLabel(labels.getString("s395"),JLabel.CENTER);
		tipoFuenteCombo = new JComboBox();
		tipoFuenteCombo.addItem(labels.getString("s396"));
		tipoFuenteCombo.addItem(labels.getString("s397"));
		tipoFuenteCombo.addItem(labels.getString("s398"));
		tipoFuenteCombo.addItem(labels.getString("s399"));
		tipoFuenteCombo.setEnabled(true);
		tipoFuenteCombo.setToolTipText(labels.getString("s400"));
		final JLabel dividerLabel = new JLabel(labels.getString("s432"),JLabel.CENTER); 
		final JTextField dividerField = new JTextField();
		final JLabel commentLabel = new JLabel(labels.getString("s433"),JLabel.CENTER); 
		final JTextField commentField = new JTextField();
		commentField.setText(Comments.getInstance().getLineComment());
		previewCommentTextField.setText(Comments.getInstance().getLineComment()+ " " + labels.getString("s444") );
		previewCommentTextField.setForeground(Comments.getInstance().getLineCommentColor());
		previewCommentTextField.setFont(new Font(f.getFontName(), Font.ITALIC, f.getSize()));
		palabraField.addKeyListener(new KeyListener() {

			public void keyTyped(KeyEvent arg0) {
				//previewTextField.setText(palabraField.getText());
			}

			public void keyPressed(KeyEvent arg0) {
				//previewTextField.setText(palabraField.getText());
			}

			public void keyReleased(KeyEvent arg0) {
				previewTextField.setText(palabraField.getText());
			}

		});
		commentField.addKeyListener(new KeyListener() {

			public void keyTyped(KeyEvent arg0) {
				//previewTextField.setText(palabraField.getText());
			}

			public void keyPressed(KeyEvent arg0) {
				//previewTextField.setText(palabraField.getText());
			}

			public void keyReleased(KeyEvent arg0) {
				previewCommentTextField.setText(commentField.getText());
			}

		});
		// Listener para JComboBox tipoCombo
		colorCombo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Color c = Color.BLACK;
				String s = (String) colorCombo.getSelectedItem();
				if (s.equals(labels.getString("s401"))) c = Color.BLUE;
				else if (s.equals(labels.getString("s402"))) c = Color.BLUE.darker().darker();
				else if (s.equals(labels.getString("s403"))) c = Color.RED;
				else if (s.equals(labels.getString("s404"))) c = Color.GREEN.darker().darker();
				else if (s.equals(labels.getString("s405"))) c = Color.BLACK;
				else if (s.equals(labels.getString("s406"))) {
					logger.info(labels.getString("s407"));
					frameColores = new JFrame();
			        frameColores.setTitle(labels.getString("s408"));
			        frameColores.setLayout(new BorderLayout());
			        JPanel panelPaleta = new JPanel();
			        panelPaleta.setLayout(new GridBagLayout());
			        GridBagConstraints constraintsPaleta = new GridBagConstraints();
			        final JColorChooser tcc = new JColorChooser(Color.BLUE);
			        tcc.getSelectionModel().addChangeListener(
			            new ChangeListener() {
			                public void stateChanged(ChangeEvent e) {
			                	Color newColor = tcc.getColor();			                    
			                	previewTextField.setText(palabraField.getText());
			    				previewTextField.setForeground(newColor);
			                }
			            }
			        );
			        tcc.setBorder(BorderFactory.createTitledBorder(labels.getString("s409")));
			        JButton aceptarBoton = new JButton(labels.getString("s410"));
			        aceptarBoton.setToolTipText(labels.getString("s411"));
			        aceptarBoton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent arg0) {
							frameColores.dispose();
						}        	
			        });
			        constraintsPaleta.fill = GridBagConstraints.BOTH;
					constraintsPaleta.gridx = 0;
					constraintsPaleta.gridy = 0;
					constraintsPaleta.gridwidth = 5;
					constraintsPaleta.insets = new Insets(5, 5, 5, 5);
			        panelPaleta.add(tcc,constraintsPaleta);
			        constraintsPaleta.fill = GridBagConstraints.NONE;
					constraintsPaleta.gridx = 3;
					constraintsPaleta.gridy = 1;
					constraintsPaleta.insets = new Insets(10, 5, 5, 5);
			        panelPaleta.add(aceptarBoton,constraintsPaleta);
			        frameColores.add(panelPaleta,BorderLayout.CENTER);
			        frameColores.setResizable(false);
			        frameColores.setVisible(true);
			        frameColores.setLocation(250,100);
			        frameColores.pack();
			        logger.info(labels.getString("s412"));
				}
				previewTextField.setText(palabraField.getText());
				previewTextField.setForeground(c);
			}
		});
		
//		 Listener para JComboBox tipoCombo
		colorComboComment.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Color c = Color.BLACK;
				String s = (String) colorComboComment.getSelectedItem();
				if (s.equals(labels.getString("s401"))) c = Color.BLUE;
				else if (s.equals(labels.getString("s402"))) c = Color.BLUE.darker().darker();
				else if (s.equals(labels.getString("s403"))) c = Color.RED;
				else if (s.equals(labels.getString("s404"))) c = Color.GREEN.darker().darker();
				else if (s.equals(labels.getString("s405"))) c = Color.BLACK;
				else if (s.equals(labels.getString("s406"))) {
					logger.info(labels.getString("s407"));
					frameColores = new JFrame();
			        frameColores.setTitle(labels.getString("s408"));
			        frameColores.setLayout(new BorderLayout());
			        JPanel panelPaleta = new JPanel();
			        panelPaleta.setLayout(new GridBagLayout());
			        GridBagConstraints constraintsPaleta = new GridBagConstraints();
			        final JColorChooser tcc = new JColorChooser(Color.BLUE);
			        tcc.getSelectionModel().addChangeListener(
			            new ChangeListener() {
			                public void stateChanged(ChangeEvent e) {
			                	Color newColor = tcc.getColor();			                    
			                	previewCommentTextField.setText(commentField.getText());
			    				previewCommentTextField.setForeground(newColor);
			                }
			            }
			        );
			        tcc.setBorder(BorderFactory.createTitledBorder(labels.getString("s409")));
			        JButton aceptarBoton = new JButton(labels.getString("s410"));
			        aceptarBoton.setToolTipText(labels.getString("s411"));
			        aceptarBoton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent arg0) {
							frameColores.dispose();
						}        	
			        });
			        constraintsPaleta.fill = GridBagConstraints.BOTH;
					constraintsPaleta.gridx = 0;
					constraintsPaleta.gridy = 0;
					constraintsPaleta.gridwidth = 5;
					constraintsPaleta.insets = new Insets(5, 5, 5, 5);
			        panelPaleta.add(tcc,constraintsPaleta);
			        constraintsPaleta.fill = GridBagConstraints.NONE;
					constraintsPaleta.gridx = 3;
					constraintsPaleta.gridy = 1;
					constraintsPaleta.insets = new Insets(10, 5, 5, 5);
			        panelPaleta.add(aceptarBoton,constraintsPaleta);
			        frameColores.add(panelPaleta,BorderLayout.CENTER);
			        frameColores.setResizable(false);
			        frameColores.setVisible(true);
			        frameColores.setLocation(250,100);
			        frameColores.pack();
			        logger.info(labels.getString("s412"));
				}
				previewCommentTextField.setText(commentField.getText());
				previewCommentTextField.setForeground(c);
			}
		});
		
		tipoFuenteCombo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Font f = palabraLabel.getFont();
				String s = (String) tipoFuenteCombo.getSelectedItem();
				if (s.equals(labels.getString("s413"))) previewTextField.setFont(new Font(f.getFontName(), Font.PLAIN, f.getSize()));
				else if (s.equals(labels.getString("s414"))) previewTextField.setFont(new Font(f.getFontName(), Font.ITALIC, f.getSize()));
				else if (s.equals(labels.getString("s415"))) previewTextField.setFont(new Font(f.getFontName(), Font.BOLD, f.getSize()));
				else if (s.equals(labels.getString("s416"))) previewTextField.setFont(new Font(f.getFontName(), Font.BOLD + Font.ITALIC, f.getSize()));
				previewTextField.setText(palabraField.getText());
			}
		});
		// Listener para el boton Añadir
		añadir.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TipoToken tt = new TipoToken();
				tt.setColor(previewTextField.getForeground());
				tt.setCursiva(previewTextField.getFont().isItalic());
				tt.setNegrita(previewTextField.getFont().isBold());
				tt.setToken(palabraField.getText());
				tt.setNombreTipo();	
				tt.setCaseSensitive(caseSensitiveCheckBox.getState());
				ListaTiposToken ltt = ListaTiposToken.getInstance();
				ltt.insertarTipoToken(tt,palabraField.getText());
				
				int num = 0;
				for (int i = 0; i <ltt.getTamanio(); i++ ){
					num = num + ltt.getTipoToken(i).getTamanioListaTokens();
				}
				
				Object[][] datos = new Object[num][3];
				int aux = 0;
				for (int i = 0; i <ltt.getTamanio(); i++ ){
					for (int j = 0; j <ltt.getTipoToken(i).getTamanioListaTokens(); j++){
						datos[aux][0] = ltt.getTipoToken(i).getToken(j);
						String color = "";
						int auxC = ltt.getTipoToken(i).getColor().getRed();
						if (Integer.toHexString(auxC).length() == 1){
							color += "0" + Integer.toHexString(auxC);
						}
						else{
							color += Integer.toHexString(auxC);
						}
						
						auxC = ltt.getTipoToken(i).getColor().getGreen();
						if (Integer.toHexString(auxC).length() == 1){
							color += "0" + Integer.toHexString(auxC);
						}
						else{
							color += Integer.toHexString(auxC);
						}
						auxC = ltt.getTipoToken(i).getColor().getBlue();
						if (Integer.toHexString(auxC).length() == 1){
							color += "0" + Integer.toHexString(auxC);
						}
						else{
							color += Integer.toHexString(auxC);
						}
						String s = "";		
						boolean b1 = ltt.getTipoToken(i).isCursiva();
						boolean b2 = ltt.getTipoToken(i).isNegrita();
						
						if (!ltt.getTipoToken(i).getToken(j).equalsIgnoreCase("<")){
							if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</CENTER></div></body></html>";
							else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</I></CENTER></div></body></html>";
							else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</B></CENTER></div></body></html>";
							else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</I></B></CENTER></div></body></html>";						
						}
						else{
							if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
		    				+ "&lt;" + "</CENTER></div></body></html>";
							else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
		    				+ "&lt;" + "</I></CENTER></div></body></html>";
							else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
		    				+ "&lt;" + "</B></CENTER></div></body></html>";
							else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
		    				+ "&lt;" + "</I></B></CENTER></div></body></html>";		
						}
						datos[aux][1] = s;
						datos[aux][2] = ltt.getTipoToken(i).getColor();
						aux++;
					}					
				}
				MyTableModel myModel = new MyTableModel();
				myModel.setValues(columnas,datos);
				sorter.setModel(myModel);
				int c = sorter.getCol();
				if (c!=-1)	sorter.sortByColumn(c);
				sorter.fireTableDataChanged();
				palabraField.setText("");
				previewTextField.setText("");
				// Mensaje de log
				logger.info(labels.getString("s417") + palabraField.getText());
			}
		});
		// Listener para el boton Quitar
		quitarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int row = listaPalTabla.getSelectedRow();
				int col = 0;
				for (int i =0; i< listaPalTabla.getColumnCount();i++){
					if (listaPalTabla.getColumnName(i) == labels.getString("s374")){
						col = i;
					}
				}
				if (row != -1){
					String val = (String) listaPalTabla.getValueAt(row,col);
					ListaTiposToken ltt = ListaTiposToken.getInstance();
					ltt.quitarToken(val);
					int num = 0;
					for (int i = 0; i <ltt.getTamanio(); i++ ){
						num = num + ltt.getTipoToken(i).getTamanioListaTokens();
					}
				
					Object[][] datos = new Object[num][3];
					int aux = 0;
					for (int i = 0; i <ltt.getTamanio(); i++ ){
						for (int j = 0; j <ltt.getTipoToken(i).getTamanioListaTokens(); j++){
							datos[aux][0] = ltt.getTipoToken(i).getToken(j);
							String color = "";
							int auxC = ltt.getTipoToken(i).getColor().getRed();
							if (Integer.toHexString(auxC).length() == 1){
								color += "0" + Integer.toHexString(auxC);
							}
							else{
								color += Integer.toHexString(auxC);
							}
							
							auxC = ltt.getTipoToken(i).getColor().getGreen();
							if (Integer.toHexString(auxC).length() == 1){
								color += "0" + Integer.toHexString(auxC);
							}
							else{
								color += Integer.toHexString(auxC);
							}
							auxC = ltt.getTipoToken(i).getColor().getBlue();
							if (Integer.toHexString(auxC).length() == 1){
								color += "0" + Integer.toHexString(auxC);
							}
							else{
								color += Integer.toHexString(auxC);
							}
							String s = "";		
							boolean b1 = ltt.getTipoToken(i).isCursiva();
							boolean b2 = ltt.getTipoToken(i).isNegrita();
							if (!ltt.getTipoToken(i).getToken(j).equalsIgnoreCase("<")){
								if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</CENTER></div></body></html>";
								else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</I></CENTER></div></body></html>";
								else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</B></CENTER></div></body></html>";
								else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</I></B></CENTER></div></body></html>";						
							}
							else{
								if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
			    				+ "&lt;" + "</CENTER></div></body></html>";
								else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
			    				+ "&lt;" + "</I></CENTER></div></body></html>";
								else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
			    				+ "&lt;" + "</B></CENTER></div></body></html>";
								else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
			    				+ "&lt;" + "</I></B></CENTER></div></body></html>";		
							}						
							datos[aux][1] = s;							
							datos[aux][2] = ltt.getTipoToken(i).getColor();
							aux++;
						}					
					}		
					MyTableModel myModel = new MyTableModel();
					myModel.setValues(columnas,datos);
					sorter.setModel(myModel);
					int c = sorter.getCol();
					if (c!=-1)	sorter.sortByColumn(c);
					sorter.fireTableDataChanged();
					palabraField.setText("");
					previewTextField.setText("");
					// Mensaje de log
					logger.info(labels.getString("s419") + val);
					}
			}
		});
		// Listener para el boton addDivider
		addDivider.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				DividerList dl = DividerList.getInstance();
				dl.insertDivider(dividerField.getText());			
				int num = dl.getTamanio();
				Object[][] datos = new Object[num][1];
				int aux = 0;
				for (int i = 0; i <dl.getTamanio(); i++ ){
						datos[aux][0] = dl.getDivider(i);
						aux++;				
				}
				MyTableModel myModel = new MyTableModel();
				myModel.setValues(columnas2,datos);
				sorter2.setModel(myModel);
				int c = sorter2.getCol();
				if (c!=-1)	sorter2.sortByColumn(c);
				sorter2.fireTableDataChanged();			
				dividerField.setText("");
			}
		});
		// Listener para el boton deleteDivider
		deleteDivider.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int row = listaDelimiterTabla.getSelectedRow();
				int col = 0;
				for (int i =0; i< listaDelimiterTabla.getColumnCount();i++){
						col = i;
				}
				if (row != -1){
					String val = (String) listaDelimiterTabla.getValueAt(row,col);
					DividerList dl = DividerList.getInstance();
					dl.deleteDelimiter(val);
					int num = dl.getTamanio();
					Object[][] datos = new Object[num][1];
					int aux = 0;
					for (int i = 0; i <dl.getTamanio(); i++ ){
						datos[aux][0] = dl.getDivider(i);
						aux++;				
					}
					MyTableModel myModel = new MyTableModel();
					myModel.setValues(columnas2,datos);
					sorter2.setModel(myModel);
					int c = sorter2.getCol();
					if (c!=-1)	sorter2.sortByColumn(c);
					sorter2.fireTableDataChanged();	
				}
			}
		});
////////Listener para el boton deleteDivider
		modify.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int row = listaPalTabla.getSelectedRow();
				int col = 0;
				for (int i =0; i< listaPalTabla.getColumnCount();i++){
					if (listaPalTabla.getColumnName(i) == labels.getString("s374")){
						col = i;
					}
				}
				if (row != -1){
					String val = (String) listaPalTabla.getValueAt(row,col);
					ListaTiposToken ltt = ListaTiposToken.getInstance();
					ltt.quitarToken(val);
					TipoToken tt = new TipoToken();
					tt.setColor(previewTextField.getForeground());
					tt.setCursiva(previewTextField.getFont().isItalic());
					tt.setNegrita(previewTextField.getFont().isBold());
					tt.setCaseSensitive(caseSensitiveCheckBox.getState());
					tt.setToken(val);
					tt.setNombreTipo();	
					ltt.insertarTipoToken(tt,palabraField.getText());
					int num = 0;
					for (int i = 0; i <ltt.getTamanio(); i++ ){
						num = num + ltt.getTipoToken(i).getTamanioListaTokens();
					}
					
					Object[][] datos = new Object[num][3];
					int aux = 0;
					for (int i = 0; i <ltt.getTamanio(); i++ ){
						for (int j = 0; j <ltt.getTipoToken(i).getTamanioListaTokens(); j++){
							datos[aux][0] = ltt.getTipoToken(i).getToken(j);
							String color = "";
							int auxC = ltt.getTipoToken(i).getColor().getRed();
							if (Integer.toHexString(auxC).length() == 1){
								color += "0" + Integer.toHexString(auxC);
							}
							else{
								color += Integer.toHexString(auxC);
							}
							
							auxC = ltt.getTipoToken(i).getColor().getGreen();
							if (Integer.toHexString(auxC).length() == 1){
								color += "0" + Integer.toHexString(auxC);
							}
							else{
								color += Integer.toHexString(auxC);
							}
							auxC = ltt.getTipoToken(i).getColor().getBlue();
							if (Integer.toHexString(auxC).length() == 1){
								color += "0" + Integer.toHexString(auxC);
							}
							else{
								color += Integer.toHexString(auxC);
							}
							String s = "";		
							boolean b1 = ltt.getTipoToken(i).isCursiva();
							boolean b2 = ltt.getTipoToken(i).isNegrita();
							if (!ltt.getTipoToken(i).getToken(j).equalsIgnoreCase("<")){
								if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</CENTER></div></body></html>";
								else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</I></CENTER></div></body></html>";
								else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</B></CENTER></div></body></html>";
								else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
			    				+ ltt.getTipoToken(i).getToken(j) + "</I></B></CENTER></div></body></html>";						
							}
							else{
								if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
			    				+ "&lt;" + "</CENTER></div></body></html>";
								else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
			    				+ "&lt;" + "</I></CENTER></div></body></html>";
								else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
			    				+ "&lt;" + "</B></CENTER></div></body></html>";
								else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
			    				+ "&lt;" + "</I></B></CENTER></div></body></html>";		
							}						
							datos[aux][1] = s;
							
							datos[aux][2] = ltt.getTipoToken(i).getColor();
							aux++;
						}					
					}		
					MyTableModel myModel = new MyTableModel();
					myModel.setValues(columnas,datos);
					sorter.setModel(myModel);
					int c = sorter.getCol();
					if (c!=-1)	sorter.sortByColumn(c);
					sorter.fireTableDataChanged();
					palabraField.setText("");
					previewTextField.setText("");
				}
				}
		});
		// Listener para el boton setDelimiters
		setDelimiters.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				ListaTiposToken ltt = ListaTiposToken.getInstance();
				DividerList dl = DividerList.getInstance();
				for (int i = 0; i <dl.getTamanio(); i++ ){
					String val = dl.getDivider(i);		
					ltt.quitarToken(val);
				}
				for (int i = 0; i <dl.getTamanio(); i++ ){
						String val = dl.getDivider(i);		
						TipoToken tt = new TipoToken();
						tt.setColor(previewTextField.getForeground());
						tt.setCursiva(previewTextField.getFont().isItalic());
						tt.setNegrita(previewTextField.getFont().isBold());
						tt.setToken(val);
						tt.setNombreTipo();	
						tt.setCaseSensitive(caseSensitiveCheckBox.getState());
						ltt.insertarTipoToken(tt,val);
				}
				int num = 0;
				for (int i = 0; i <ltt.getTamanio(); i++ ){
					num = num + ltt.getTipoToken(i).getTamanioListaTokens();
				}
				
				Object[][] datos = new Object[num][3];
				int aux = 0;
				for (int i = 0; i <ltt.getTamanio(); i++ ){
					for (int j = 0; j <ltt.getTipoToken(i).getTamanioListaTokens(); j++){
						datos[aux][0] = ltt.getTipoToken(i).getToken(j);
						String color = "";
						int auxC = ltt.getTipoToken(i).getColor().getRed();
						if (Integer.toHexString(auxC).length() == 1){
							color += "0" + Integer.toHexString(auxC);
						}
						else{
							color += Integer.toHexString(auxC);
						}
						
						auxC = ltt.getTipoToken(i).getColor().getGreen();
						if (Integer.toHexString(auxC).length() == 1){
							color += "0" + Integer.toHexString(auxC);
						}
						else{
							color += Integer.toHexString(auxC);
						}
						auxC = ltt.getTipoToken(i).getColor().getBlue();
						if (Integer.toHexString(auxC).length() == 1){
							color += "0" + Integer.toHexString(auxC);
						}
						else{
							color += Integer.toHexString(auxC);
						}
						String s = "";		
						boolean b1 = ltt.getTipoToken(i).isCursiva();
						boolean b2 = ltt.getTipoToken(i).isNegrita();
						if (!ltt.getTipoToken(i).getToken(j).equalsIgnoreCase("<")){
							if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</CENTER></div></body></html>";
							else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</I></CENTER></div></body></html>";
							else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</B></CENTER></div></body></html>";
							else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
		    				+ ltt.getTipoToken(i).getToken(j) + "</I></B></CENTER></div></body></html>";						
						}
						else{
							if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
		    				+ "&lt;" + "</CENTER></div></body></html>";
							else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
		    				+ "&lt;" + "</I></CENTER></div></body></html>";
							else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
		    				+ "&lt;" + "</B></CENTER></div></body></html>";
							else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
		    				+ "&lt;" + "</I></B></CENTER></div></body></html>";		
						}						
						datos[aux][1] = s;
						
						datos[aux][2] = ltt.getTipoToken(i).getColor();
						aux++;
					}					
				}
				MyTableModel myModel = new MyTableModel();
				myModel.setValues(columnas,datos);
				sorter.setModel(myModel);
				int c = sorter.getCol();
				if (c!=-1)	sorter.sortByColumn(c);
				sorter.fireTableDataChanged();
				
				palabraField.setText("");
				previewTextField.setText("");
			}
		});
		// Valores para la etiqueta palabraLabel
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5, 5, 5, 5);
		panelTokens.add(palabraLabel, c);
		// Valores para el campo de texto palabraField
		c.gridx = 1;
		panelTokens.add(palabraField, c);
		
		c.ipadx = 0;
		c.insets = new Insets(10, 5, 5, 5);
		
		c.gridx = 0;
		c.gridy = 1;
		panelTokens.add(caseSensitiveLabel, c);
		c.gridx = 1;
		panelTokens.add(caseSensitiveCheckBox, c);
		
		// Valores para la etiqueta colorLabel 

		c.gridx = 0;
		c.gridy = 2;
		panelTokens.add(colorLabel, c);
		// Valores para el comboBox tipoCombo
		c.gridx = 1;
		panelTokens.add(colorCombo, c);
		// Valores para la etiqueta tipoFuenteLabel
		c.gridx = 0;
		c.gridy = 3;
		panelTokens.add(tipoFuenteLabel, c);
		// Valores para el comboBox tipoFuenteCombo
		c.gridx = 1;
		panelTokens.add(tipoFuenteCombo, c);
		

		
		// Valores para la etiqueta previewLabel
		c.gridx = 0;
		c.gridy = 4;
		panelTokens.add(previewLabel, c);
		// Valores para el campo de texto previewTextField
		c.gridx = 1;
		panelTokens.add(previewTextField, c);
		
		// Valores para el boton Añadir
		c.ipadx = 70;
		c.gridx = 0;
		c.gridy = 5;
		panelTokens.add(añadir, c);
		// Valores para el boton Quitar
		c.gridx = 1;
		panelTokens.add(quitarBoton, c);
		
		// Valores para el boton Modificar
		c.gridy = 6;
		c.gridx = 0;
		panelTokens.add(setDelimiters, c);
		c.gridx = 1;
    	panelTokens.add(modify, c);	
		setListado();
		
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5, 5, 5, 5);
		panelDividers.add(dividerLabel, c);
		
		c.gridx = 1;
		panelDividers.add(dividerField, c);
		
		c.ipadx = 0;
		c.gridx = 0;
		c.gridy = 1;
		panelDividers.add(addDivider, c);
		c.gridx = 1;
		panelDividers.add(deleteDivider, c);
		
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		//c.insets = new Insets(5, 5, 5, 5);
		panelComment.add(commentLabel, c);
		
		c.gridx = 1;
		c.ipadx = 0;
		panelComment.add(commentField, c);
		
		c.gridx = 0;
		c.gridy = 1;
		panelComment.add(colorLabelComment, c);
		c.gridx = 1;
		panelComment.add(colorComboComment, c);
		
		c.gridx = 0;
		c.gridy = 2;
		panelComment.add(previewCommentLabel, c);
		c.gridx = 1;
		panelComment.add(previewCommentTextField, c);
		
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		c.ipady = 0;
		panelTokens.setSize(new Dimension(100,100));
		frame.add(panelTokens,c);
		
		JPanel jpaux = new JPanel();
		GridBagLayout gbl = new GridBagLayout();
		jpaux.setLayout(gbl);
		c.ipady = 22;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		jpaux.add(panelDividers,c);
		c.ipady = 22;
		c.gridx = 0;
		c.gridy = 1;
		c.ipadx = 0;
		jpaux.add(panelComment,c);
		
		c.ipady = 0;
		c.gridx = 1;
		c.gridy = 0;
		c.ipadx = 0;
		frame.add(jpaux,c);
		
		JButton apply = new JButton();
		apply.setText(labels.getString("s434"));
		apply.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Comments.getInstance().setLineComment(commentField.getText());
				Comments.getInstance().setLineCommentColor(previewCommentTextField.getForeground());
				int editor = Ventana.getInstance().getCreadorEditor().dameNumEditores();
				for (int i =0; i< editor; i++){
					Ventana.getInstance().getCreadorEditor().dameEditorI(i).resetDoc();
			
				}
				//Lenguaje l = Lenguaje.getInstance();
				//l.guardar(l.getNombre(), false);
				frame.dispose();
				Ventana.getInstance().getStatusBar().getMessageLexical().setText(labels.getString("s449") + " " + Lenguaje.getInstance().getNombre()); 
			}
		});
		JButton cancel = new JButton();
		cancel.setText(labels.getString("s435"));
		cancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Lenguaje.getInstance().cargarTemp(tempPath);	
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				frame.dispose();
			}
		});
		
		ActionListener l = new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				frame.dispose();
			}
		};

		cancel.registerKeyboardAction(l, "EscapeKey", 
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 
									   0 , true), 
				JComponent.WHEN_IN_FOCUSED_WINDOW);                
		
		c.ipady = 0;
		c.gridx = 0;
		c.gridy = 1;
		c.ipadx = 0;
		frame.add(apply,c);
		c.ipady = 0;
		c.gridx = 1;
		c.gridy = 1;
		c.ipadx = 0;
		frame.add(cancel,c);
		
		frame.setVisible(true);
		frame.setResizable(true);
		frame.setLocationRelativeTo(null);
		frame.setResizable(false);
		frame.setLocationRelativeTo(null);
		logger.info(labels.getString("s420"));
		Windows window=new Windows();
		frame.addWindowListener(window);
		frame.pack();
		frame.setLocationRelativeTo(null);
	}
	
	public JScrollPane listado(){
		ListaTiposToken ltt = ListaTiposToken.getInstance();
		
		int num = 0;
		for (int i = 0; i <ltt.getTamanio(); i++ ){
			num = num + ltt.getTipoToken(i).getTamanioListaTokens();
		}
		
		Object[][] datos = new Object[num][3];
		int aux = 0;
		for (int i = 0; i <ltt.getTamanio(); i++ ){
			for (int j = 0; j <ltt.getTipoToken(i).getTamanioListaTokens(); j++){
				datos[aux][0] = ltt.getTipoToken(i).getToken(j);
				String color = "";
				int auxC = ltt.getTipoToken(i).getColor().getRed();
				if (Integer.toHexString(auxC).length() == 1){
					color += "0" + Integer.toHexString(auxC);
				}
				else{
					color += Integer.toHexString(auxC);
				}
				
				auxC = ltt.getTipoToken(i).getColor().getGreen();
				if (Integer.toHexString(auxC).length() == 1){
					color += "0" + Integer.toHexString(auxC);
				}
				else{
					color += Integer.toHexString(auxC);
				}
				auxC = ltt.getTipoToken(i).getColor().getBlue();
				if (Integer.toHexString(auxC).length() == 1){
					color += "0" + Integer.toHexString(auxC);
				}
				else{
					color += Integer.toHexString(auxC);
				}
				String s = "";		
				boolean b1 = ltt.getTipoToken(i).isCursiva();
				boolean b2 = ltt.getTipoToken(i).isNegrita();
				if (!ltt.getTipoToken(i).getToken(j).equalsIgnoreCase("<")){
					if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
    				+ ltt.getTipoToken(i).getToken(j) + "</CENTER></div></body></html>";
					else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
    				+ ltt.getTipoToken(i).getToken(j) + "</I></CENTER></div></body></html>";
					else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
    				+ ltt.getTipoToken(i).getToken(j) + "</B></CENTER></div></body></html>";
					else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
    				+ ltt.getTipoToken(i).getToken(j) + "</I></B></CENTER></div></body></html>";						
				}
				else{
					if (!b1 && !b2) s = "<html><head></head><body><div style=\"color:" + color + ";\"><CENTER>" 
    				+ "&lt;" + "</CENTER></div></body></html>";
					else if (b1 && !b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><I>" 
    				+ "&lt;" + "</I></CENTER></div></body></html>";
					else if (!b1 && b2) s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B>" 
    				+ "&lt;" + "</B></CENTER></div></body></html>";
					else if (b1 && b2)  s = "<html><head></head><body><div style=\"color:"  + color + ";\"><CENTER><B><I>" 
    				+ "&lt;" + "</I></B></CENTER></div></body></html>";		
				}						
				datos[aux][1] = s;
				
				datos[aux][2] = ltt.getTipoToken(i).getColor();
				aux++;
			}					
		}
		
		MyTableModel myModel = new MyTableModel();
		myModel.setValues(columnas,datos);
        sorter = new TableSorter(myModel); 
        listaPalTabla = new JTable(sorter);  
        listaPalTabla.addKeyListener(new KeyListener(){
			public void keyReleased(KeyEvent arg0) {checkRows();}
			public void keyTyped(KeyEvent arg0) {checkRows();}
			public void keyPressed(KeyEvent arg0) {checkRows();}
        	
        });
        
        listaPalTabla.addMouseListener(new MouseListener(){

			public void mouseClicked(MouseEvent arg0) {checkRows();}
			public void mousePressed(MouseEvent arg0) {checkRows();}
			public void mouseReleased(MouseEvent arg0) {} //checkRows();}
			public void mouseEntered(MouseEvent arg0) {} //checkRows();}
			public void mouseExited(MouseEvent arg0) {} //checkRows();}
        	
        });
        
        sorter.addMouseListenerToHeaderInTable(listaPalTabla); 
        
        listaPalTabla.setDefaultRenderer(Color.class,
                                 new ColorRenderer(true));
        listaPalTabla.setDefaultEditor(Color.class,
                               new ColorEditor());
        listaPalTabla.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        listaPalTabla.setPreferredScrollableViewportSize(new Dimension(310,200));
        JScrollPane jsp = new JScrollPane(listaPalTabla);
        return jsp;
	}
	
	public JScrollPane listado2(){
		DividerList dl = DividerList.getInstance();
		
		int num = dl.getTamanio();
		
		
		Object[][] datos = new Object[num][1];
		int aux = 0;
		for (int i = 0; i <dl.getTamanio(); i++ ){
				datos[aux][0] = dl.getDivider(i);
				aux++;					
		}
		
		MyTableModel myModel = new MyTableModel();
		myModel.setValues(columnas2,datos);
        sorter2 = new TableSorter(myModel); 
        listaDelimiterTabla = new JTable(sorter2);             
        sorter2.addMouseListenerToHeaderInTable(listaDelimiterTabla); 
        
        listaDelimiterTabla.setDefaultRenderer(Color.class,
                                 new ColorRenderer(true));
        listaDelimiterTabla.setDefaultEditor(Color.class,
                               new ColorEditor());
        listaDelimiterTabla.setPreferredScrollableViewportSize(new Dimension(280,200));
        JScrollPane jsp = new JScrollPane(listaDelimiterTabla);
        return jsp;
	}
	
	public void setListado(){				
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 7;
		c.gridwidth = 2;
		panelTokens.add(listado(), c);
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 2;
		c.insets = new Insets(5, 5, 5, 5);
		panelDividers.add(listado2(), c);
	}
	
	public void checkRows(){
		int row = listaPalTabla.getSelectedRow();
		int col = 0;
		for (int i =0; i< listaPalTabla.getColumnCount();i++){
			if (listaPalTabla.getColumnName(i) == labels.getString("s374")){
				col = i;
			}
		}
		if (row != -1){
			String val = (String) listaPalTabla.getValueAt(row,col);
			ListaTiposToken ltt = ListaTiposToken.getInstance();
			TipoToken tt = ltt.devolverToken(val);
			colorCombo.setSelectedIndex(0);
			palabraField.setText(val);
			previewTextField.setText(val);
			previewTextField.setForeground(tt.getColor());
			caseSensitiveCheckBox.setState(tt.isCaseSensitive());
			Font f = previewTextField.getFont();
			if (!tt.isCursiva() && !tt.isNegrita()) {
				previewTextField.setFont(new Font(f.getFontName(), Font.PLAIN, f.getSize()));
				tipoFuenteCombo.setSelectedIndex(0);
			}
			else if (tt.isCursiva() && !tt.isNegrita()) {
				previewTextField.setFont(new Font(f.getFontName(), Font.ITALIC, f.getSize()));
				tipoFuenteCombo.setSelectedIndex(1);
			}
			else if (!tt.isCursiva() && tt.isNegrita()) {
				previewTextField.setFont(new Font(f.getFontName(), Font.BOLD, f.getSize()));
				tipoFuenteCombo.setSelectedIndex(2);
			}
			else if (tt.isCursiva() && tt.isNegrita()) {
				previewTextField.setFont(new Font(f.getFontName(), Font.BOLD + Font.ITALIC, f.getSize()));
				tipoFuenteCombo.setSelectedIndex(3);
			}
		}
	}

}

class ColorRenderer extends JLabel implements TableCellRenderer {
	Border unselectedBorder = null;

	Border selectedBorder = null;

	boolean isBordered = true;

	public ColorRenderer(boolean isBordered) {
		this.isBordered = isBordered;
		setOpaque(true); //MUST do this for background to show up.
	}

	public Component getTableCellRendererComponent(JTable table, Object color,
			boolean isSelected, boolean hasFocus, int row, int column) {
		Color newColor = (Color) color;
		setBackground(newColor);
		if (isBordered) {
			if (isSelected) {
				if (selectedBorder == null) {
					selectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getSelectionBackground());
				}
				setBorder(selectedBorder);
			} else {
				if (unselectedBorder == null) {
					unselectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getBackground());
				}
				setBorder(unselectedBorder);
			}
		}
		return this;
	}
}



class ColorEditor extends AbstractCellEditor implements TableCellEditor,
		ActionListener {
	Color currentColor;

	JButton button;

	JColorChooser colorChooser;

	JDialog dialog;

	protected static final String EDIT = "edit";

	public ColorEditor() {
		button = new JButton();
		button.setActionCommand(EDIT);
		button.addActionListener(this);
		button.setBorderPainted(false);
		colorChooser = new JColorChooser();
		dialog = JColorChooser.createDialog(button, "Pick a Color", true,
				colorChooser, this,
				null);
	}


	public void actionPerformed(ActionEvent e) {
		if (EDIT.equals(e.getActionCommand())) {

			button.setBackground(currentColor);
			colorChooser.setColor(currentColor);
			dialog.setVisible(true);

			fireEditingStopped();

		} else { 
			currentColor = colorChooser.getColor();
		}
	}

	public Object getCellEditorValue() {
		return currentColor;
	}

	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		currentColor = (Color) value;
		return button;
	}
	
	
}
