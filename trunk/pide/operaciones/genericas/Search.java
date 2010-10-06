package operaciones.genericas;

import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Clase que busca en un Texto
 * 
 * 
 */
public class Search {

	private Pattern p;

	private Matcher m;

	private String cadExpReg;

	private int cont;

	private int posTemp;

	private ArrayList listaExpReg;

	private static boolean ciclo;

	public Search() {
		cadExpReg = " ";
		listaExpReg = new ArrayList();
		cont = 0;
		posTemp = -2;
		ciclo = false;
	}

	/**
	 * @param pos
	 * @param cad
	 * @param texto
	 * @param invertido
	 * @param sensible
	 * @param expreg
	 * @return
	 */
	public int buscar(int pos, String cad, String texto, boolean sensible,
			boolean expreg, boolean completas, int dir) {
		if (expreg == true) {
			if (sensible == false) {
				p = Pattern.compile(cad, Pattern.CASE_INSENSITIVE);
			} else {
				p = Pattern.compile(cad);
			}

			m = p.matcher(texto);
           cadExpReg=" ";
			if ((dir == 0) || (dir == 2)) {
				if (m.find(pos) == true){
					cadExpReg=" ";
					cadExpReg = m.group(0).toString();
					}
				} else {
				int k = 0;
				int limite = pos;
				boolean fin = false;
				listaExpReg.clear();
				while ((m.find() == true) && (fin == false)) {
					listaExpReg.add(m.group(0).toString());
					k = texto.indexOf(m.group(0).toString(), k)
							+ m.group(0).toString().length();

					if (k > limite) {
						fin = true;
						listaExpReg.remove(listaExpReg.size() - 1);
					}

				}

			}

		}

		int posicion = -1;

		// Dirección adelante
		if (dir == 0) {
			if (sensible == true) {
				if ((expreg == false) && (completas == false))
					posicion = texto.indexOf(cad, pos);

				if (expreg == true) {
					if (cadExpReg != " ")
						posicion = texto.indexOf(cadExpReg, pos);
					else
						posicion=-1;

				} else if (completas == true) {

					int posi = texto.indexOf(cad, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + cad.length() < texto.length())
							pos2 = posi + cad.length();
						else
							pos2 = posi + cad.length() - 1;
						char c1 = texto.charAt(pos1);
						char c2 = texto.charAt(pos2);

						char c = cad.charAt(cad.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == cad
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							posicion = texto.indexOf(cad, pos);

						} else
							posicion = buscar(pos + 1, cad, texto, sensible,
									expreg, completas, dir);
					} else
						posicion=-1;
				}
			} else if (sensible == false) {
				String cad2 = texto.toLowerCase();
				String cad1 = cad.toLowerCase();
				if ((expreg == false) && (completas == false))
					posicion = cad2.indexOf(cad1, pos);
				if (expreg == true) {
					if (cadExpReg != " ")
						posicion = texto.indexOf(cadExpReg, pos);
					else
						posicion=-1;
					  
				} else if (completas == true) {

					int posi = texto.indexOf(cad1, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + cad.length() < cad2.length())
							pos2 = posi + cad1.length();
						else
							pos2 = posi + cad1.length() - 1;
						System.out.println(pos1 + " " + pos2);
						char c1 = cad2.charAt(pos1);
						char c2 = cad2.charAt(pos2);

						char c = cad1.charAt(cad1.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == cad
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							posicion = cad2.indexOf(cad1, pos);

						} else
							posicion = buscar(pos + 1, cad, texto, sensible,
									expreg, completas, dir);
					} else
						posicion=-1;

				}
			}
		}

		// Direccion Atrás
		if (dir == 1) {
			if (sensible == true) {
				if ((expreg == false) && (completas == false))
					posicion = texto.lastIndexOf(cad, pos - 1);

				if (expreg == true) {
					cont = listaExpReg.size() - 1;
					if (listaExpReg.size() > 0) {
						cadExpReg = listaExpReg.get(cont).toString();

						posicion = texto.lastIndexOf(cadExpReg, pos);
					} else
						posicion=-1;

				} else if (completas == true) {

					int posi = texto.lastIndexOf(cad, pos - 1);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + cad.length() < texto.length())
							pos2 = posi + cad.length();
						else
							pos2 = posi + cad.length() - 1;
    					char c1 = texto.charAt(pos1);
						char c2 = texto.charAt(pos2);

						char c = cad.charAt(cad.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == cad
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							posicion = texto.lastIndexOf(cad, pos - 1);

						} else
							posicion = buscar(pos - 2, cad, texto, sensible,
									expreg, completas, dir);
					} else
						posicion=-1;
				}
			}

			if (sensible == false) {
				String cad2 = texto.toLowerCase();
				String cad1 = cad.toLowerCase();
				if ((expreg == false) && (completas == false)) {

					posicion = cad2.lastIndexOf(cad1, pos - 1);
				}

				if (expreg == true) {

					if (listaExpReg.size() > 0) {
						cadExpReg = listaExpReg.get(listaExpReg.size() - 1)
								.toString();
						posicion = texto.lastIndexOf(cadExpReg, pos - 1);
					} else
						posicion=-1;
				} else if (completas == true) {

					int posi = cad2.lastIndexOf(cad1, pos - 1);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + cad1.length() < cad2.length())
							pos2 = posi + cad1.length();
						else
							pos2 = posi + cad1.length() - 1;
						char c1 = cad2.charAt(pos1);
						char c2 = cad2.charAt(pos2);

						char c = cad1.charAt(cad1.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == cad1
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							posicion = cad2.lastIndexOf(cad1, pos - 1);

						} else
							posicion = buscar(pos - 1, cad, texto, sensible,
									expreg, completas, dir);
					} else
						posicion=-1;
				}

			}
		}
		// Dirección Todo
		else if (dir == 2) {

			if (sensible == true) {
				if ((expreg == false) && (completas == false))
					posicion = texto.indexOf(cad, pos);

				if (expreg == true) {
					posicion = texto.indexOf(cadExpReg, pos);

				} else if (completas == true) {

					int posi = texto.indexOf(cad, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + cad.length() < texto.length())
							pos2 = posi + cad.length();
						else
							pos2 = posi + cad.length() - 1;
    					char c1 = texto.charAt(pos1);
						char c2 = texto.charAt(pos2);

						char c = cad.charAt(cad.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == cad
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							posicion = texto.indexOf(cad, pos);

						} else
							posicion = buscar(pos + 1, cad, texto, sensible,
									expreg, completas, dir);
					} else
						posicion=-1;
				}
			}
			if (sensible == false) {
				String cad2 = texto.toLowerCase();
				String cad1 = cad.toLowerCase();
				if ((expreg == false) && (completas == false)) {
					posicion = cad2.indexOf(cad1, pos);
				}

				if (expreg == true) {
					posicion = texto.indexOf(cadExpReg, pos);

				} else if (completas == true) {

					int posi = texto.indexOf(cad1, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + cad.length() < cad2.length())
							pos2 = posi + cad1.length();
						else
							pos2 = posi + cad1.length() - 1;
						char c1 = cad2.charAt(pos1);
						char c2 = cad2.charAt(pos2);
						char c = cad1.charAt(cad1.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == cad
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							posicion = cad2.indexOf(cad1, pos);

						} else
							posicion = buscar(pos + 1, cad, texto, sensible,
									expreg, completas, dir);

					} else
						posicion=-1;
				}
			}
		 
			if ((ciclo == false) && (posTemp == -2))
				posTemp = pos;
			
			if ((posicion == -1) && (ciclo == false)) {
				ciclo = true;
				posicion = buscar(0, cad, texto, sensible, expreg, completas,
						dir);
				
			} else {
				if ((ciclo == true) && (posTemp <= posicion)) {
					return -1;
				}

			}

		}

		return posicion;

	}

	public String getCadExpReg() {
		return cadExpReg;
	}

	public String getListaExpReg(int c) {
		return listaExpReg.get(c).toString();
	}

	public  boolean isCiclo() {
		return ciclo;
	}

	public  void setCiclo(boolean ci) {
		ciclo = ci;
	}

	public int getPosTemp() {
		return posTemp;
	}

	public void setPosTemp(int posT) {
		posTemp = posT;
	}

}