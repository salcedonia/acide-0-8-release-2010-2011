package gui.editor;

import idioma.Idioma;

import java.awt.Color;
import java.util.Hashtable;
import java.util.ResourceBundle;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import operaciones.lexicas.Comments;
import operaciones.lexicas.DividerList;
import operaciones.lexicas.ListaTiposToken;
import operaciones.lexicas.TipoToken;
import operaciones.log.Log;
import org.apache.log4j.Logger;

public class SyntaxisDoc extends DefaultStyledDocument {
	/**
	 * Atributo: serialVersionUID: clase serializable
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	private DefaultStyledDocument doc;
	private Element rootElement;
	private MutableAttributeSet normal;
	private MutableAttributeSet comment;
	private MutableAttributeSet brace;
	private MutableAttributeSet[] keyword;
	private Hashtable[] keywords;

	/**
	 * Creador de SintaxisDoc
	 */
	public SyntaxisDoc() {
		super();
		ResourceBundle labels = Idioma.getInstance().getLabels();
		try {
			//Leemos los tokens de un archivo
			ListaTiposToken ltt = ListaTiposToken.getInstance();
			
			logger.info(labels.getString("s321"));
			doc = this;
			rootElement = doc.getDefaultRootElement();
			putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");
			normal = new SimpleAttributeSet();
			comment = new SimpleAttributeSet();
			
			StyleConstants.setForeground(normal, Color.black);
			StyleConstants.setForeground(comment, Comments.getInstance().getLineCommentColor());
			StyleConstants.setItalic(comment, true);
			StyleConstants.setBold(comment, false);
			
			brace = new SimpleAttributeSet();
			StyleConstants.setBackground(brace,Color.blue);
			StyleConstants.setForeground(brace, Color.white);
			
			keyword = new MutableAttributeSet[ltt.getTamanio()];
			for (int i=0; i< ltt.getTamanio(); i++){
				keyword[i] = new SimpleAttributeSet();
				// Vamos asociando los tipos
				TipoToken tt = ltt.getTipoToken(i);
				StyleConstants.setForeground(keyword[i], tt.getColor());
				StyleConstants.setItalic(keyword[i], tt.isCursiva());
				StyleConstants.setBold(keyword[i], tt.isNegrita());
			}
			
			keywords = new Hashtable[ltt.getTamanio()];
			Object dummyObject = new Object();
			for (int i=0; i< ltt.getTamanio(); i++){
				keywords[i] = new Hashtable();
				TipoToken tt = ltt.getTipoToken(i);
				for (int j=0;j<tt.getTamanioListaTokens();j++){
					String s = tt.getToken(j);
					if (!tt.isCaseSensitive()) s = s.toLowerCase();
					keywords[i].put(s,dummyObject);
				}
			}
		}

		catch (Exception e) {
			logger.info(labels.getString("s322"));
			e.printStackTrace();
		}
		logger.info(labels.getString("s323"));

	}

	/*
	 * Override to apply syntax highlighting after the document has been updated
	 */
	public void insertString(int offset, String str, AttributeSet a)
			throws BadLocationException {
		super.insertString(offset, str, a);
		processChangedLines(offset, str.length());
	}

	/*
	 * Override to apply syntax highlighting after the document has been updated
	 */
	public void remove(int offset, int length) throws BadLocationException {
		super.remove(offset, length);
		processChangedLines(offset, 0);
	}
	
	/*
	 * Determine how many lines have been changed, then apply highlighting to
	 * each line
	 */
	private void processChangedLines(int offset, int length)
			throws BadLocationException {
		String content = doc.getText(0, doc.getLength());
		// The lines affected by the latest document update
		int startLine = rootElement.getElementIndex(offset);
		int endLine = rootElement.getElementIndex(offset + length);
		// Do the actual highlighting
		for (int i = startLine; i <= endLine; i++){
			applyHighlighting(content, i);
			if (Comments.getInstance().getLineComment() != "") 
				applyComments(content,i);
		}
	}

	
	/*
	 * Parse the line to determine the appropriate highlighting
	 */
	private void applyHighlighting(String content, int line)
			throws BadLocationException {
		int startOffset = rootElement.getElement(line).getStartOffset();
		int endOffset = rootElement.getElement(line).getEndOffset() - 1;
		int lineLength = endOffset - startOffset;
		int contentLength = content.length();
		if (endOffset >= contentLength)
			endOffset = contentLength - 1;
       // set normal attributes for the line
		doc.setCharacterAttributes(startOffset, lineLength, normal, true);
		// check for tokens
		checkForTokens(content, startOffset, endOffset);
	}

	/*
	 * Parse the line for tokens to highlight
	 */
	private void checkForTokens(String content, int startOffset, int endOffset) {
		while (startOffset <= endOffset) {
			// skip the delimiters to find the start of a new token
			while (isDelimiter(content.substring(startOffset, startOffset + 1),startOffset,content)) {
				if (startOffset < endOffset)
					startOffset++;
				else
					return;
			}
			// Extract and process the entire token
			startOffset = getOtherToken(content, startOffset, endOffset);
		}
	}

	private int getOtherToken(String content, int startOffset, int endOffset) {
		int endOfToken = startOffset + 1;
		while (endOfToken <= endOffset) {
			if (isDelimiter(content.substring(endOfToken, endOfToken + 1),endOfToken,content))
				break;
			endOfToken++;
		}
		String token = content.substring(startOffset, endOfToken);
		int pos = isKeyword(token);		
		if (pos!=-1){
			doc.setCharacterAttributes(startOffset, endOfToken - startOffset,
					keyword[pos-1], false);
		}
		
		return endOfToken + 1;
	}

	/*
	 * Override for other languages
	 */
	protected boolean isDelimiter(String character, int pos, String content) {
		DividerList.getInstance();
		for (int i = 0; i< DividerList.getInstance().getTamanio(); i++){
			String operands = DividerList.getInstance().getDivider(i);
			
			if (Character.isWhitespace(character.charAt(0))) return true;
			int spos = 0;
			 
			while (operands.indexOf(character,spos) != -1){ 
				if (((pos - operands.indexOf(character,spos) + operands.length()) <= content.length()) &&
						(pos - operands.indexOf(character,spos)>=0)){
					String sAux = content.substring(pos - operands.indexOf(character,spos), pos - operands.indexOf(character,spos) + operands.length());
					if (sAux.equals(operands)){
						int posD = isKeyword(sAux);
						if (posD !=-1){
							doc.setCharacterAttributes(pos - operands.indexOf(character,spos),sAux.length() ,
									keyword[posD-1], false);
						}
						return true;
					}
				}	
				spos++;
			}
		}
		return false;
	}

	/*
	 * Override for other languages
	 */
	protected int isKeyword(String token) {
		Object o;
		Object a = null;
		ListaTiposToken ltt = ListaTiposToken.getInstance();
		String aux = token;
		boolean encontrado = false;
		int i = 0;
		while (!encontrado && i < keywords.length){
			if (!ltt.getTipoToken(i).isCaseSensitive()){
				token = token.toLowerCase();
			}
			else{
				token = aux;
			}
			o = keywords[i].get(token);
			if (o != null){
				a = o;
				encontrado = true;
			}
			i++;
		}
		return a == null ? (-1) : i;
	}

	/*
	 * Override for other languages
	 */
	protected String getEscapeString(String quoteDelimiter) {
		return "\\" + quoteDelimiter;
	}
	
	/*
	 * 
	 */
	protected String addMatchingBrace(int offset) throws BadLocationException {
		StringBuffer whiteSpace = new StringBuffer();
		int line = rootElement.getElementIndex(offset);
		int i = rootElement.getElement(line).getStartOffset();
		while (true) {
			String temp = doc.getText(i, 1);
			if (temp.equals(" ") || temp.equals("\t")) {
				whiteSpace.append(temp);
				i++;
			} else
				break;
		}
		return "{\n" + whiteSpace.toString() + whiteSpace.toString() + "\n"
				+ whiteSpace.toString() + "}";
	}
	
	private void applyComments(String content, int line){
		Comments c= Comments.getInstance();
		String s = c.getLineComment();
		String texto  = "";		
		int startOffset = rootElement.getElement(line).getStartOffset();
		int endOffset = rootElement.getElement(line).getEndOffset() - 1;
		int lineLength = endOffset - startOffset;
		int contentLength = content.length();
		try {
			texto = doc.getText(startOffset,lineLength);
			int pos = texto.indexOf(s);
			if (pos>=0)	doc.setCharacterAttributes(startOffset + pos,lineLength-pos,comment, true);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
			
	}
	
	public void setBrace(int pos){
		try{
		if ((pos+1)< doc.getLength()){
			doc.setCharacterAttributes(pos,1,brace, true);
		}
		}
		catch(Exception e){
			
		}
	}
	
	public void elimBrace(int pos){
		try{
		doc.setCharacterAttributes(pos,1,normal, true);
		}
		catch(Exception e){
			
		}
	}
	
}
