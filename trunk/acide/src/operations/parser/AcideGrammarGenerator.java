/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package operations.parser;

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.util.ResourceBundle;

import es.bytes.ByteFile;
import es.text.AcideFileManager;
import gui.consolePanel.AcideConsolePanel;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import language.AcideLanguageManager;

import org.apache.log4j.Logger;

import operations.console.ConsoleThread;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * 
 */
public class AcideGrammarGenerator {
	
	/**
	 * 
	 */
	private static Logger _logger = AcideLog.getLog();
	
	/**
	 * Constructor of the class.
	 */
	public AcideGrammarGenerator(){
		
	}
	
	// TODO Cambiar todos los Thread.sleep() por waitFor() de Process (no funciona bien con waitFor(), se queda colgao)
	// FIXME Pasar todas las rutas a linux (mirar sobre todo en el configuracion.properties)
	// TODO Cambiar las llamadas a la consola de comandos por Runtime exec sin llamar a la consola de 
	// comandos (deberia hacerlo a la consola de comandos del SO por defecto) -> No crea bien el jar
	// FIXME No carga bien la ruta del menu: si falla no busca la default. En cambio si lo hace bien con TB
	// FIXME No se carga el language que se deja puesto
	// FIXME Delfin tiene que arreglar la carga de los ultimos archivos (menu,TB,grammar) xq no carga los wenos en caso de que esten modificados. Revisar el WindowClosing de la clase Ventana
	
	/**
	 * 
	 */
	public static boolean generate(String grammarName) throws Exception {
		
		// Gets the language TO DISPLAY
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		boolean generated = true;
		ConsoleThread p = new ConsoleThread();
		//Process p1 = Runtime.getRuntime().exec();
		AcideConsolePanel s = new AcideConsolePanel(false);
		//String sep = System.getProperty("file.separator");
		//System.out.println(sep);
		//String javaPath = javacPath.substring(0,javacPath.length() - 1);
		String javaPath = null;
		try {
			javaPath = AcideResourceManager.getInstance().getProperty("javaPath");
			if (javaPath.equals("null")) throw new Exception(labels.getString("s927"));
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,labels.getString("s928"),labels.getString("s934"),JOptionPane.ERROR_MESSAGE);
			_logger.error(e.getMessage());
			generated = false;
		}
		//System.out.println("javaPath --> " + javaPath);
		//System.out.println("\"" + javaPath  + "\" antlr.Tool grammar.g");
//		try {
//			Process p1 = Runtime.getRuntime().exec("cmd",".","\"" + javaPath + "\" antlr.Tool grammar.g");
//			p1.waitFor();
//		}
//		catch (Exception e) {
//			JOptionPane.showMessageDialog(null,e.getMessage(),"ERRROR",JOptionPane.ERROR_MESSAGE);
//		}
		p.executeCommand("cmd",".","\"" + javaPath  + "\" antlr.Tool grammar.g","exit",s);
//		try {
//			p1.waitFor();
//		}
//		catch (InterruptedException e1) {
//			// 
//			e1.printStackTrace();
//		}
		JFrame output = new JFrame(labels.getString("s946"));
		output.add(s);
		output.setSize(new Dimension(300,400));
		//resultado.pack();
		//antlrOutput.setVisible(true);
		//File f1 = new File(".\\GrammarLexer.java");
		/*
		 * Waiting is used because first time we call this method source 
		 * file is not generated because antlr tool have not had enough 
		 * time to complete its job
		 */
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			//e.printStackTrace();
			_logger.error(labels.getString("s245"));
			generated = false;
		}
		ModifyParser();
		//while(!f1.exists());
		String javacPath = null;
		try {
			javacPath = AcideResourceManager.getInstance().getProperty("javacPath");
			if (javacPath.equals("null")) throw new Exception(labels.getString("s929"));
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,labels.getString("s929"),labels.getString("s933"),JOptionPane.ERROR_MESSAGE);
			_logger.error(e.getMessage());
			generated = false;
		}
		//System.out.println("\"" + javacPath + "\" -cp .;c:\\classes .\\*.java -d .");
		p.executeCommand("cmd",".","\"" + javacPath + "\" -cp .;c:\\classes .\\*.java -d .","exit",s);
		//JFrame javacOutput = new JFrame("javac");
		output.add(s);
		//javacOutput.setSize(new Dimension(300,400));
		//resultado.pack();
		//javacOutput.setVisible(true);
		//File f2 = new File(".\\operaciones\\sintacticas\\grammar\\GrammarLexer.class");
		
		/*
		 * Waiting is used because first time we call this method source 
		 * file is not generated because javac tool have not had enough 
		 * time to complete its job
		 */
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			//e.printStackTrace();
			_logger.error(labels.getString("s245"));
			generated = false;
		}
		//while(!f2.exists());
		/* 
		 * Save a copy and reallocate generated files:
		 * 	GrammarLexer.java
		 *  GrammarLexer.class
		 * 	GrammarLexerTokenTypes.java
		 *  GrammarLexerTokenTypes.class
		 * 	GrammarParser.java
		 *  GrammarParser.class
		 * 	GrammarLexer.smap
		 * 	GrammarParser.smap
		 * 	GrammarLexerTokenTypes.txt
		 */
//		boolean witness;
//		witness = reallocateFile(".//GrammarLexerTokenTypes.txt", ".//operaciones/sintacticas/grammar/GrammarLexerTokenTypes.txt");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarLexerTokenTypes.txt","./configuration/grammars/currentGrammar/GrammarLexerTokenTypes.txt");
//		generated = generated && witness;
//		witness = reallocateFile(".//GrammarLexerTokenTypes.java", ".//operaciones/sintacticas/grammar/GrammarLexerTokenTypes.java");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarLexerTokenTypes.java","./configuration/grammars/currentGrammar/GrammarLexerTokenTypes.java");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarLexerTokenTypes.class","./configuration/grammars/currentGrammar/GrammarLexerTokenTypes.class");
//		generated = generated && witness;
//		witness = reallocateFile(".//GrammarLexer.smap", ".//operaciones/sintacticas/grammar/GrammarLexer.smap");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarLexer.smap","./configuration/grammars/currentGrammar/GrammarLexer.smap");
//		generated = generated && witness;
//		witness = reallocateFile(".//GrammarLexer.java", ".//operaciones/sintacticas/grammar/GrammarLexer.java");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarLexer.java","./configuration/grammars/currentGrammar/GrammarLexer.java");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarLexer.class","./configuration/grammars/currentGrammar/GrammarLexer.class");
//		generated = generated && witness;
//		witness = reallocateFile(".//GrammarParser.smap", ".//operaciones/sintacticas/grammar/GrammarParser.smap");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarParser.smap","./configuration/grammars/currentGrammar/GrammarParser.smap");
//		generated = generated && witness;
//		witness = reallocateFile(".//GrammarParser.java", ".//operaciones/sintacticas/grammar/GrammarParser.java");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarParser.java","./configuration/grammars/currentGrammar/GrammarParser.java");
//		copyFile(".//operaciones/sintacticas/grammar/GrammarParser.class","./configuration/grammars/currentGrammar/GrammarParser.class");
//		generated = generated && witness;
//		witness = saveToGrammarFolder(grammarName);
//		generated = generated && witness;
		/*
		 * Create .jar with new grammar's files
		 *  GrammarLexer.java
		 *  GrammarLexer.class
		 * 	GrammarLexerTokenTypes.java
		 *  GrammarLexerTokenTypes.class
		 * 	GrammarParser.java
		 *  GrammarParser.class
		 * 	GrammarLexer.smap
		 * 	GrammarParser.smap
		 * 	GrammarLexerTokenTypes.txt
		 *  grammar.g
		 *  lexicalCats.txt
		 *  syntaxRules.txt
		 *  grammarName.txt
		 */
		// Reallocate generated files
		reallocateFile("GrammarLexer.java","operaciones/sintacticas/grammar/GrammarLexer.java");
		reallocateFile("GrammarLexer.smap","operaciones/sintacticas/grammar/GrammarLexer.smap");
		reallocateFile("GrammarLexerTokenTypes.java","operaciones/sintacticas/grammar/GrammarLexerTokenTypes.java");
		reallocateFile("GrammarLexerTokenTypes.txt","operaciones/sintacticas/grammar/GrammarLexerTokenTypes.txt");
		reallocateFile("GrammarParser.java","operaciones/sintacticas/grammar/GrammarParser.java");
		reallocateFile("GrammarParser.smap","operaciones/sintacticas/grammar/GrammarParser.smap");
		// TODO Añadir al .jar manifiesto y clase que tenga el main para ejecutarlo
		//String jarPath = javacPath.substring(0, javacPath.length() - 3) + "r";
		String jarPath = null;
		try {
			jarPath = AcideResourceManager.getInstance().getProperty("jarPath");
			if (jarPath.equals("null")) throw new Exception(labels.getString("s930"));
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,labels.getString("s930"),labels.getString("s932"),JOptionPane.ERROR_MESSAGE);
			_logger.error(e.getMessage());
			generated = false;
		}
		//System.out.println("jarPath --> " + jarPath);
		//System.out.println("\"" + jarPath + "\" cvf0 " + grammarName + ".jar GrammarLexer.java GrammarLexer.class GrammarLexerTokenTypes.java GrammarLexerTokenTypes.class GrammarParser.java GrammarParser.class GrammarLexer.smap GrammarParser.smap GrammarLexerTokenTypes.txt grammar.g lexicalCats.txt syntaxRules.txt grammarName.txt");
//		copyFile(".\\operaciones\\sintacticas\\grammar\\GrammarLexer.class","GrammarLexer.class");
//		copyFile(".\\operaciones\\sintacticas\\grammar\\GrammarLexerTokenTypes.class","GrammarLexerTokenTypes.class");
//		copyFile(".\\operaciones\\sintacticas\\grammar\\GrammarParser.class","GrammarParser.class");
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			//e.printStackTrace();
			_logger.error(labels.getString("s245"));
			generated = false;
		}
		//p.executeCommand("cmd",".","\"" + jarPath + "\" cvf " + grammarName + ".jar GrammarLexer.java GrammarLexer.class GrammarLexerTokenTypes.java GrammarLexerTokenTypes.class GrammarParser.java GrammarParser.class GrammarLexer.smap GrammarParser.smap GrammarLexerTokenTypes.txt grammar.g lexicalCats.txt syntaxRules.txt","exit",s);
		p.executeCommand("cmd",".","\"" + jarPath + "\" cvmf " + grammarName + ".jar manifiestoGrammar.txt grammar.g lexicalCats.txt syntaxRules.txt operaciones/sintacticas/Analyzer.java operaciones/sintacticas/Analyzer.class operaciones/sintacticas/grammar","exit",s);
//		Process p1 = null;
//		try {
//			p1 = Runtime.getRuntime().exec("\"" + jarPath + "\" cvf pruebaProcess.jar Release.txt",null,new File("."));
//		}
//		catch (IOException e1) {
//			// 
//			e1.printStackTrace();
//		}
//		try {
//			p1.waitFor();
//		}
//		catch (InterruptedException e1) {
//			// 
//			e1.printStackTrace();
//		}
		//p.executeCommand("cmd",".","\"" + jarPath + "\" cvf " + grammarName + ".jar","exit",s);
		//p.executeCommand("cmd",".","\"" + jarPath + "\" uvf " + grammarName + ".jar  GrammarLexer.java GrammarLexer.class GrammarLexerTokenTypes.java GrammarLexerTokenTypes.class GrammarParser.java GrammarParser.class GrammarLexer.smap GrammarParser.smap GrammarLexerTokenTypes.txt grammar.g lexicalCats.txt syntaxRules.txt grammarName.txt","exit",s);
		//JFrame jarOutput = new JFrame("jar");
		output.add(s);
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			//e.printStackTrace();
			_logger.error(labels.getString("s245"));
			generated = false;
		}
		File f = new File("GrammarLexer.java");
		f.delete();
		f = new File("GrammarLexer.class");
		f.delete();
		f = new File("GrammarLexerTokenTypes.java");
		f.delete();
		f = new File("GrammarLexerTokenTypes.class");
		f.delete();
		f = new File("GrammarParser.java");
		f.delete();
		f = new File("GrammarParser.class");
		f.delete();
		f = new File("GrammarLexer.smap");
		f.delete();
		f = new File("GrammarParser.smap");
		f.delete();
		f = new File("GrammarLexerTokenTypes.txt");
		f.delete();
		f = new File("grammar.g");
		f.delete();
		f = new File("lexicalCats.txt");
		f.delete();
		f = new File("syntaxRules.txt");
		f.delete();
//		f = new File("grammarName.txt");
//		f.delete();
		//copyFile(grammarName + ".jar",".\\\\\configuration\\grammars\\currentGrammar\\" + grammarName + ".jar");
		reallocateFile(grammarName + ".jar",".\\configuration\\grammars\\" + grammarName + ".jar");
		//p.executeCommand("cmd",".\\\\\configuration\\grammars\\currentGrammar","\"" + jarPath + "\" xvf " + grammarName + ".jar syntaxRules.txt lexicalCats.txt","exit",s);
		output.add(s);
		//jarOutput.setSize(new Dimension(300,400));
		//resultado.pack();
		output.setVisible(true);
		return generated;
	}
	
	/**
	 * 
	 */
	private static void ModifyParser() {
		AcideFileManager f = new AcideFileManager();
		String txt = null;
		txt = f.load("GrammarParser.java");
		//String message = "message";
		String exception = "throw new RecognitionException(ex.getMessage());";//\"" + message + "\");";
		//String txt2 = txt.replaceAll("reportError","throw new Exception(\"" + message + "\");reportError");
		// Insert first exception
		String aux = "";
		int index = txt.indexOf("recover(ex");
		index++;
		aux = txt.substring(index);
		int indexAux = aux.indexOf(";");
		index += indexAux + 1;
		String head = txt.substring(0,index);
		String tail = txt.substring(index);
		txt = head + exception + tail;
		// Insert following exceptions
		boolean finished = false;
		while (!finished) {
			index = txt.lastIndexOf(exception);
			aux = txt.substring(index);
			indexAux = aux.indexOf("recover(ex");
			if (indexAux == -1) finished = true;
			else {
				String aux2 = aux.substring(indexAux);
				int indexAux2 = aux2.indexOf(";");
				index = index + indexAux + indexAux2 + 1;
				head = txt.substring(0,index);
				tail = txt.substring(index);
				txt = head + exception + tail;
			}
		}
		//System.out.println(txt);
		f.write("GrammarParser.java",txt);
	}
	
	/**
	 * 
	 * @param source
	 * @param target
	 * @return
	 */
	private static boolean reallocateFile(String source, String target) {
		
		// Gets the language TO DISPLAY
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		File sourceFile = new File(source);
		/*
		 * Pooling is used because first time we call this method source 
		 * file is not generated because antlr tool have not had enough 
		 * time to complete its job
		 */
		while(!sourceFile.exists()) sourceFile = new File(source);
		File targetFile = new File(target);
		//Fichero f = new Fichero();
		if(targetFile.exists()) targetFile.delete();
		try {
			targetFile.createNewFile();
		}
		catch (IOException e) {
			//e.printStackTrace();
			JOptionPane.showMessageDialog(null,labels.getString("s211"),labels.getString("s210"),JOptionPane.ERROR_MESSAGE);
			_logger.error(labels.getString("s210") + ": " + labels.getString("s211"));
		}
//		String sourceText = f.cargar(source);
//		//System.out.println("sourceText: " + sourceText);
//		boolean saved = f.salvar(target,sourceText);
		//System.out.println("saved: " + saved);
		boolean saved = false;
		try {
			ByteFile.copy(source, target);
			saved = true;
		}
		catch (IOException e) {
			//e.printStackTrace();
			saved = false;
			JOptionPane.showMessageDialog(null,e.getMessage(),labels.getString("s945"),JOptionPane.ERROR_MESSAGE);
			// logger.error...
		}
		/*if(saved)*/ _logger.info(labels.getString("s212") + target);
		//else logger.error(labels.getString("s213") + target);
		boolean deleted = sourceFile.delete();
		//System.out.println("deleted: " + deleted);
		if(deleted) _logger.info(labels.getString("s214") + source);
		else _logger.error(labels.getString("s215") + source);
		return (saved && deleted);
	}
	
	/**
	 * 
	 * @param source
	 * @param target
	 * @return
	 */
/*	private static boolean copyFile(String source, String target) {
		Language i = Language.getInstance();
		try {
			i.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		File sourceFile = new File(source);
		/*
		 * Pooling is used because first time we call this method source 
		 * file is not generated because antlr tool have not had enough 
		 * time to complete its job
		 */
		/*while(!sourceFile.exists()) sourceFile = new File(source);
		File targetFile = new File(target);
		//Fichero f = new Fichero();
		if(targetFile.exists()) targetFile.delete();
		try {
			targetFile.createNewFile();
		}
		catch (IOException e) {
			//e.printStackTrace();
			JOptionPane.showMessageDialog(null,labels.getString("s211"),labels.getString("s210"),JOptionPane.ERROR_MESSAGE);
			_logger.error(labels.getString("s210") + ": " + labels.getString("s211"));
		}
//		String sourceText = f.cargar(source);
//		//System.out.println("sourceText: " + sourceText);
//		boolean copied = f.salvar(target,sourceText);
		//System.out.println("copied: " + copied);
		boolean copied = false;
		try {
			ByteFile.copy(source, target);
			copied = true;
		}
		catch (IOException e) {
			//e.printStackTrace();
			copied = false;
			JOptionPane.showMessageDialog(null,e.getMessage(),labels.getString("s945"),JOptionPane.ERROR_MESSAGE);
			// logger.error...
		}
		/*if(copied)*/// _logger.info(labels.getString("s212") + target);
		//else logger.error(labels.getString("s213") + target);
		//return copied;
//	}
}
