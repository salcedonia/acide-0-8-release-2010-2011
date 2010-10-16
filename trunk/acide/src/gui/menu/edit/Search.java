package gui.menu.edit;

import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * 
 */
public class Search {

	/**
	 * 
	 */
	private Pattern _pattern;
	/**
	 * 
	 */
	private Matcher _matcher;
	/**
	 * 
	 */
	private String _regularExpresion;
	/**
	 * 
	 */
	private int _count;
	/**
	 * 
	 */
	private int _temporalPosition;
	/**
	 * 
	 */
	@SuppressWarnings("rawtypes")
	private ArrayList _regularExpresionList;
	/**
	 * 
	 */
	private static boolean _cycle;

	/**
	 * Constructor of the class.
	 */
	@SuppressWarnings("rawtypes")
	public Search() {
		_regularExpresion = " ";
		_regularExpresionList = new ArrayList();
		_count = 0;
		_temporalPosition = -2;
		_cycle = false;
	}

	/**
	 * 
	 * @param pos
	 * @param string
	 * @param text
	 * @param isCaseSensitive
	 * @param isRegularExpresion
	 * @param isCompleted
	 * @param dir
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public int search(int pos, String string, String text,
			boolean isCaseSensitive, boolean isRegularExpresion,
			boolean isCompleted, int dir) {

		if (isRegularExpresion) {

			if (!isCaseSensitive)
				_pattern = Pattern.compile(string, Pattern.CASE_INSENSITIVE);
			else
				_pattern = Pattern.compile(string);

			_matcher = _pattern.matcher(text);
			_regularExpresion = " ";
			if ((dir == 0) || (dir == 2)) {
				if (_matcher.find(pos)) {
					_regularExpresion = " ";
					_regularExpresion = _matcher.group(0).toString();
				}
			} else {
				int k = 0;
				int limit = pos;
				boolean end = false;
				_regularExpresionList.clear();
				while ((_matcher.find()) && (!end)) {
					_regularExpresionList.add(_matcher.group(0).toString());
					k = text.indexOf(_matcher.group(0).toString(), k)
							+ _matcher.group(0).toString().length();

					if (k > limit) {
						end = true;
						_regularExpresionList.remove(_regularExpresionList.size() - 1);
					}
				}
			}
		}

		int position = -1;

		// Forward direction
		if (dir == 0) {
			if (isCaseSensitive) {
				if ((!isRegularExpresion) && (!isCompleted))
					position = text.indexOf(string, pos);

				if (isRegularExpresion) {
					if (_regularExpresion != " ")
						position = text.indexOf(_regularExpresion, pos);
					else
						position = -1;

				} else if (isCompleted) {

					int posi = text.indexOf(string, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + string.length() < text.length())
							pos2 = posi + string.length();
						else
							pos2 = posi + string.length() - 1;
						char c1 = text.charAt(pos1);
						char c2 = text.charAt(pos2);

						char c = string.charAt(string.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == string
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							position = text.indexOf(string, pos);

						} else
							position = search(pos + 1, string, text,
									isCaseSensitive, isRegularExpresion,
									isCompleted, dir);
					} else
						position = -1;
				}
			} else 
				if (!isCaseSensitive) {
				
				String cad2 = text.toLowerCase();
				String cad1 = string.toLowerCase();
				if ((!isRegularExpresion) && (!isCompleted))
					position = cad2.indexOf(cad1, pos);
				if (isRegularExpresion) {
					if (_regularExpresion != " ")
						position = text.indexOf(_regularExpresion, pos);
					else
						position = -1;

				} else if (isCompleted) {

					int posi = text.indexOf(cad1, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + string.length() < cad2.length())
							pos2 = posi + cad1.length();
						else
							pos2 = posi + cad1.length() - 1;
						System.out.println(pos1 + " " + pos2);
						char c1 = cad2.charAt(pos1);
						char c2 = cad2.charAt(pos2);

						char c = cad1.charAt(cad1.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == string
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							position = cad2.indexOf(cad1, pos);

						} else
							position = search(pos + 1, string, text,
									isCaseSensitive, isRegularExpresion,
									isCompleted, dir);
					} else
						position = -1;
				}
			}
		}

		// Backward Direction
		if (dir == 1) {
			if (isCaseSensitive) {
				if ((!isRegularExpresion) && (!isCompleted))
					position = text.lastIndexOf(string, pos - 1);

				if (isRegularExpresion) {
					_count = _regularExpresionList.size() - 1;
					if (_regularExpresionList.size() > 0) {
						_regularExpresion = _regularExpresionList.get(_count).toString();

						position = text.lastIndexOf(_regularExpresion, pos);
					} else
						position = -1;

				} else if (isCompleted) {

					int posi = text.lastIndexOf(string, pos - 1);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + string.length() < text.length())
							pos2 = posi + string.length();
						else
							pos2 = posi + string.length() - 1;
						char c1 = text.charAt(pos1);
						char c2 = text.charAt(pos2);

						char c = string.charAt(string.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == string
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							position = text.lastIndexOf(string, pos - 1);

						} else
							position = search(pos - 2, string, text,
									isCaseSensitive, isRegularExpresion,
									isCompleted, dir);
					} else
						position = -1;
				}
			}

			if (!isCaseSensitive) {
				
				String cad2 = text.toLowerCase();
				String cad1 = string.toLowerCase();
				
				if ((!isRegularExpresion) && (!isCompleted))
					position = cad2.lastIndexOf(cad1, pos - 1);
			
				if (isRegularExpresion) {

					if (_regularExpresionList.size() > 0) {
						_regularExpresion = _regularExpresionList.get(_regularExpresionList.size() - 1)
								.toString();
						position = text.lastIndexOf(_regularExpresion, pos - 1);
					} else
						position = -1;
				} else if (isCompleted) {

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
							position = cad2.lastIndexOf(cad1, pos - 1);

						} else
							position = search(pos - 1, string, text,
									isCaseSensitive, isRegularExpresion,
									isCompleted, dir);
					} else
						position = -1;
				}

			}
		}
		// All Directions
		else if (dir == 2) {

			if (isCaseSensitive) {
				if ((!isRegularExpresion) && (!isCompleted))
					position = text.indexOf(string, pos);

				if (isRegularExpresion) {
					position = text.indexOf(_regularExpresion, pos);

				} else if (isCompleted) {

					int posi = text.indexOf(string, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + string.length() < text.length())
							pos2 = posi + string.length();
						else
							pos2 = posi + string.length() - 1;
						char c1 = text.charAt(pos1);
						char c2 = text.charAt(pos2);

						char c = string.charAt(string.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == string
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							position = text.indexOf(string, pos);

						} else
							position = search(pos + 1, string, text,
									isCaseSensitive, isRegularExpresion,
									isCompleted, dir);
					} else
						position = -1;
				}
			}
			if (!isCaseSensitive) {
				
				String cad2 = text.toLowerCase();
				String cad1 = string.toLowerCase();
				
				if ((!isRegularExpresion) && (!isCompleted))
					position = cad2.indexOf(cad1, pos);
				
				if (isRegularExpresion) {
					position = text.indexOf(_regularExpresion, pos);

				} else if (isCompleted) {

					int posi = text.indexOf(cad1, pos);
					if (posi != -1) {
						int pos2 = 0;
						int pos1 = 0;
						if (posi - 1 >= 0)
							pos1 = posi - 1;
						else
							pos1 = posi;
						if (posi + string.length() < cad2.length())
							pos2 = posi + cad1.length();
						else
							pos2 = posi + cad1.length() - 1;
						char c1 = cad2.charAt(pos1);
						char c2 = cad2.charAt(pos2);
						char c = cad1.charAt(cad1.length() - 1);
						if (((c1 == '\n') || (c1 >= ' ') && (c1 <= '/')
								|| (c1 >= ':') && (c1 <= '@') || (c1 >= '[')
								&& (c1 <= '`') || (c1 >= '{') && (c1 <= '~') || (c1 == string
								.charAt(0)))
								&& ((c2 == '\n') || (c2 >= ' ') && (c2 <= '/')
										|| (c2 >= ':') && (c2 <= '@')
										|| (c2 >= '[') && (c2 <= '`')
										|| (c2 >= '{') && (c2 <= '~') || (c2 == c))) {
							position = cad2.indexOf(cad1, pos);

						} else
							position = search(pos + 1, string, text,
									isCaseSensitive, isRegularExpresion,
									isCompleted, dir);

					} else
						position = -1;
				}
			}

			if ((!_cycle) && (_temporalPosition == -2))
				_temporalPosition = pos;

			if ((position == -1) && (!_cycle)) {
				_cycle = true;
				position = search(0, string, text, isCaseSensitive,
						isRegularExpresion, isCompleted, dir);

			} else {
				if ((_cycle) && (_temporalPosition <= position)) {
					return -1;
				}
			}
		}
		return position;
	}

	/**
	 * 
	 * @return
	 */
	public String getRegularExpresion() {
		return _regularExpresion;
	}

	/**
	 * 
	 * @param c
	 * @return
	 */
	public String getRegularExpresionList(int c) {
		return _regularExpresionList.get(c).toString();
	}

	/**
	 * 
	 * @return
	 */
	public boolean cycle() {
		return _cycle;
	}

	/**
	 * 
	 * @param cycle
	 */
	public void setCycle(boolean cycle) {
		_cycle = cycle;
	}

	/**
	 * 
	 * @return
	 */
	public int getTemporalPosition() {
		return _temporalPosition;
	}

	/**
	 * 
	 * @param temporalPosition
	 */
	public void setTemporalPosition(int temporalPosition) {
		_temporalPosition = temporalPosition;
	}
}