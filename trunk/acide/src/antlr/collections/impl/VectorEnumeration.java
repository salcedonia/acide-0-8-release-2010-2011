package antlr.collections.impl;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: VectorEnumeration.java,v 1.1 2007/04/18 16:34:41 juanjortiz Exp $
 */

import java.util.Enumeration;
import java.util.NoSuchElementException;

import antlr.collections.Enumerator;

// based on java.lang.Vector; returns any null indices between non-null ones.

class VectorEnumeration implements Enumeration {
    Vector vector;
    int i;


    VectorEnumeration(Vector v) {
        vector = v;
        i = 0;
    }

    public boolean hasMoreElements() {
        synchronized (vector) {
            return i <= vector.lastElement;
        }
    }

    public Object nextElement() {
        synchronized (vector) {
            if (i <= vector.lastElement) {
                return vector.data[i++];
            }
            throw new NoSuchElementException("VectorEnumerator");
        }
    }
}
