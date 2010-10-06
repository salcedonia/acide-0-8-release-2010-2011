package operaciones.salida;
import gui.Ventana;
import gui.salida.Salida;

import java.io.*;

import principal.almacenPropiedades;

public class ProcessThread extends Thread {
    
    static BufferedWriter writer = null;
	public Process p = null;
	
	public ProcessThread() {}
    
	
    public synchronized void run() {
                Runtime runtime = Runtime.getRuntime();
                
                // Usar "/bin/sh" en Linux.
                String exec = null; 
                String pathS = null;
                try {
                	exec = almacenPropiedades.getPropiedad("ejecutable");
                	pathS = almacenPropiedades.getPropiedad("pathEjecutable");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if(!pathS.equals("null") && !exec.equals("null")){
				try{
                	File path = new File(pathS);
                	p = runtime.exec(exec,null,path);
                }
                catch(Exception e){
                	System.out.println("Información sobre ejecutable incompleta");
                	e.printStackTrace();
                }
                writer = new BufferedWriter(
                        new OutputStreamWriter(p.getOutputStream()));
                
                ProcessInputThread inputThread =
                        new ProcessInputThread(writer,System.in);

                StreamGobbler errorGobbler = new StreamGobbler(p
    					.getErrorStream(),null,Ventana.getInstance().getnuevaSalida());

    			StreamGobbler outputGobbler = new StreamGobbler(p
    					.getInputStream(),null,Ventana.getInstance().getnuevaSalida());
    			
    			//Disponible disponible = new Disponible(p.getInputStream());
    			
    			//disponible.start();
    			errorGobbler.start();
    			outputGobbler.start();
    			inputThread.start( );
                
    			try {
	    			int exitVal;
					exitVal = p.waitFor();
					System.out.println("ExitValue: " + exitVal);
				} catch (InterruptedException e) {
					e.printStackTrace();
				} 
				}     
    }
    
    public static void main(String args[]){
    	ProcessThread pt = new ProcessThread();
    	pt.start();
    }
    
    public static BufferedWriter getWriter(){
    	return writer;
    }
    
    public Process getProcess(){
    	return p;
    }
    
    public void executeCommand(String ejecutable, String pathEjecutable, String command, String exit, Salida salida2) {
    	Runtime runtime = Runtime.getRuntime();
    	Process proc = null;
        String pathS = pathEjecutable;
        try{
        	File path = new File(pathS);
        	proc = runtime.exec(ejecutable,null,path);
        }
        catch(Exception e){
        	System.out.println("Información sobre ejecutable incompleta");
        	e.printStackTrace();
        }
        BufferedWriter writer2 = new BufferedWriter(
                new OutputStreamWriter(proc.getOutputStream()));
        
        StreamGobbler errorGobbler2 = new StreamGobbler(proc.getErrorStream(),null,salida2);

		StreamGobbler outputGobbler2 = new StreamGobbler(proc
				.getInputStream(),null,salida2);
		
		errorGobbler2.start();
		outputGobbler2.start();
				
		try {
			//salida2.añadeTexto(command + '\n');
			writer2.write(command + '\n');
			writer2.flush();
			writer2.write(exit + '\n');
			writer2.flush();
		} catch (IOException e1) {
			e1.printStackTrace();
		}

}

}

