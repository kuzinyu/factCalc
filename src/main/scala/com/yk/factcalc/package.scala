package com.yk

package object factcalc {
	/**
	 * Converts a string to an integer value, defaulting to fallback in case of an error.
	 * @param value A string to convert from
	 * @param fallback A fallback
	 * @return Converted value
	 */
	def str2Int(value: String, fallback: Int) =
		try value.toInt catch { case e: NumberFormatException => fallback }

	/**
	 * Converts a string to an integer value if possible.
	 * @param value A Some if a value can be converted, None otherwise
	 * @return An Option representing a result of a conversion
	 */
	def str2Int(value: String): Option[Int] =
		try Some(value.toInt) catch { case e: NumberFormatException => None }

	/**
	 * Converts bytes to a string taking specific encoding into account.
	 * @param bytes A sequence of bytes to convert
	 * @param encoding An encoding used in conversion
	 * @return Converted value
	 */
	def bytes2Str(bytes: Array[Byte], encoding: String) = new String(bytes, encoding)

	/**
	 * Converts a string to bytes taking specific encoding into account.
	 * @param str A string to convert to bytes
	 * @param encoding An encoding used in conversion
	 * @return Converted value
	 */
	def str2Bytes(str: String, encoding: String) = str.getBytes(encoding)
}
