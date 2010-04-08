package org.sciviews.zooimage.tools;

/**
 * Sort utilities. Implements a sort function for arrays of strings
 */
public class SortUtilities {
	
	/**
	 * This is a slightly modified version of the ij.util.sort() function
	 * @param a String array to sort
	 */
	public static void sort(String[] a) {
		if(!alreadySorted(a))
			sortIt(a, 0, a.length - 1);
	}

	/**
	 * Sort the array from one index to another
	 * @param a string array to sort
	 * @param from start index
	 * @param to end index
	 */
	private static void sortIt(String[] a, int from, int to) {
		int i = from, j = to;
		String center = a[ (from + to) / 2 ];
		do {
			while ( i < to && center.compareTo(a[i]) > 0 ) i++;
			while ( j > from && center.compareTo(a[j]) < 0 ) j--;
			if (i < j) {String temp = a[i]; a[i] = a[j]; a[j] = temp; }
			if (i <= j) { i++; j--; }
		} while(i <= j);
		if (from < j) sortIt(a, from, j);
		if (i < to) sortIt(a,  i, to);
	}

	/**
	 * checks if the array is already sorted
	 * @param a a string array
	 * @return true if the array is already sorted
	 */
	private static boolean alreadySorted(String[] a) {
		for ( int i=1; i<a.length; i++ ) {
			if (a[i].compareTo(a[i-1]) < 0 )
				return false;
		}
		return true;
	}

}
