package operaciones.configuracion;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

public class Fich {
			private String path;
	    
		private boolean setFile;
		
		private boolean mainFile;
		
        private String name;
		
        private String padre;
        
        private boolean directory;
        
        //mig
        private boolean opened;
        
        private TreePath treePath;
		
        public String getPadre() {
			return padre;
		}

		public void setPadre(String padre) {
			this.padre = padre;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public Fich(){

		}
     public String toString(){
    
    	 return this.name;
     }
	public void setString (String s){
		name=s;
	}
     public boolean isMainFile() {
			return mainFile;
		}

	public void setMainFile(boolean mainFile) {
			this.mainFile = mainFile;
		}
	
	public String getPath() {
			return path;
		}

	public void setPath(String name) {
			this.path = name;
		}

	public boolean isSetFile() {
			return setFile;
		}

	public void setSetFile(boolean setFile) {
			this.setFile = setFile;
		}

	public boolean isDirectory() {
			return directory;
		}

	public void setDirectory(boolean directory) {
			this.directory = directory;
		}

	public String getFilePath() {
		String filePath;
		
		filePath = path.substring(0,path.lastIndexOf("\\"));
		
		return filePath;
	}

	public String getFileExt() {
		String fileExt;
		
		fileExt = path.substring(path.lastIndexOf(".")+1);
		
		return fileExt;
	}

	public String getFileName() {
		String fileExt;
		
		fileExt = path.substring(path.lastIndexOf("\\")+1,path.lastIndexOf("."));
		
		return fileExt;
	}

	public String getLastPathComponent() {
		String s;
		
		s = path.substring(path.lastIndexOf("\\")+1);
		
		return s;
	}
	
	public void setOpened(boolean b){
		this.opened = b;
	}
	
	public boolean isOpened(){
		return opened;
	}

	public TreePath getTreePath() {
		return treePath;
	}

	}

